open Result
open Mirage_net
open Lwt.Infix
open OS.Esp32

(*
 NETWORK INTERFACE FOR ESP32. 
 Hugely inspired from Solo5 implementation https://github.com/mirage/mirage-net-solo5/blob/master/src/netif.ml
 *)

let src = Logs.Src.create "netif" ~doc:"Mirage esp32 network module"
module Log = (val Logs.src_log src : Logs.LOG)

type +'a io = 'a Lwt.t

type t = {
  interface: Wifi.wifi_interface;
  mutable active: bool;
  mac: Macaddr.t;
  stats: stats;
}
  
type error = [
  | Mirage_net.error
  | `Invalid_argument
  | `Unspecified_error
  | `Exn of exn
]

(* Get main event group handle and give it to wifi library *)
external register_wifi_events : unit -> unit = "ml_register_wifi_events"

(* Initialize wifi library and connect to mirage event manager *)
let () =
  match Wifi.initialize () with 
  | Ok _ ->
    begin
      register_wifi_events ();
      let wifi_evt = Wifi.([STA_started;STA_stopped;AP_started;AP_stopped;STA_connected;STA_disconnected;STA_frame_received;AP_frame_received;]) in
      let wifi_evt_id = List.map Wifi.id_of_event wifi_evt in 
      List.iter OS.Event.register_event_number wifi_evt_id
    end
  | Error _ -> failwith "wifi couldn't be initialized"

let if_of_dev = function
  | "ap" -> Wifi.IF_AP
  | "sta" -> Wifi.IF_STA
  | _ -> failwith "No interface has this name."

let pp_error ppf = function
  | #Mirage_net.error as e -> Mirage_net.pp_error ppf e
  | `Invalid_argument      -> Fmt.string ppf "Invalid argument"
  | `Unspecified_error     -> Fmt.string ppf "Unspecified error"
  | `Exn e                 -> Fmt.exn ppf e

let devices = Hashtbl.create 1

let if_ap = ref false 
let if_sta = ref false

let set_wifi_mode () = ignore (match(!if_ap, !if_sta) with 
  | true, true -> Wifi.set_mode Wifi.MODE_APSTA
  | false, true -> Wifi.set_mode Wifi.MODE_STA 
  | true, false -> Wifi.set_mode Wifi.MODE_AP
  | false, false -> assert false)

let connect devname =
  let macaddr = Wifi.get_mac (if_of_dev devname) in
  let _ = match (if_of_dev devname) with
  | Wifi.IF_AP -> if_ap := true
  | Wifi.IF_STA -> if_sta := true
  in
  set_wifi_mode ();
  match Macaddr.of_bytes (Bytes.to_string macaddr) with
  | None -> Lwt.fail_with "Netif: Could not get MAC address"
  | Some mac ->
     Log.info (fun f -> f "Plugging into %s with mac %s"
                        devname (Macaddr.to_string mac));
     let active = true in
     (* XXX: hook up ni.mtu *)
     let t = {
         interface=(if_of_dev devname); active; mac;
         stats= { rx_bytes=0L;rx_pkts=0l; tx_bytes=0L; tx_pkts=0l } }
     in
     Hashtbl.add devices devname t;
     Lwt.return t

let disconnect t =
  (match t.interface with 
  | Wifi.IF_AP -> Log.info (fun f -> f "Disconnect access point")
  | Wifi.IF_STA -> Log.info (fun f -> f "Disconnect station"));
  t.active <- false;
  (match (t.interface) with
  | Wifi.IF_AP -> if_ap := false
  | Wifi.IF_STA -> if_sta := false);
  set_wifi_mode ();
  Lwt.return_unit

type macaddr = Macaddr.t
type page_aligned_buffer = Io_page.t
type buffer = Cstruct.t

(* Input a frame, and block if nothing is available *)
let rec read t buf =
  let evt = match t.interface with 
    | Wifi.IF_AP -> Wifi.AP_frame_received
    | Wifi.IF_STA -> Wifi.STA_frame_received 
  and
  process () =
    let r = match Wifi.read t.interface buf.Cstruct.buffer buf.Cstruct.len with
      | Ok len    ->
        t.stats.rx_pkts <- Int32.succ t.stats.rx_pkts;
        t.stats.rx_bytes <- Int64.add t.stats.rx_bytes (Int64.of_int len);
        let buf = Cstruct.sub buf 0 len in
        Ok buf
      | Error Wifi.Nothing_to_read   -> Error `Continue
      | Error Wifi.Unspecified  -> Error `Unspecified_error
      | Error _  -> assert false
    in
    Lwt.return r 
  in
  process () >>= function
  | Ok buf                   -> Lwt.return (Ok buf)
  | Error `Continue          ->
    OS.Event.wait_for_event (Wifi.id_of_event evt) >>= fun () -> read t buf
  | Error `Canceled          -> Lwt.return (Error `Canceled)
  | Error `Invalid_argument  -> Lwt.return (Error `Invalid_argument)
  | Error `Unspecified_error -> Lwt.return (Error `Unspecified_error)

let safe_apply f x =
  Lwt.catch
    (fun () -> f x)
    (fun exn ->
       Log.err (fun f -> f "[listen] error while handling %s, continuing. bt: %s"
                           (Printexc.to_string exn) (Printexc.get_backtrace ()));
       Lwt.return_unit)

(* Loop and listen for packets permanently *)
(* this function has to be tail recursive, since it is called at the
   top level, otherwise memory of received packets and all reachable
   data is never claimed.  take care when modifying, here be dragons! *)
let rec listen t fn =
  match t.active with
  | true ->
    let buf = Cstruct.create 1514 in (* XXX: hook up ni.mtu *)
    let process () =
      read t buf >|= function
      | Ok buf                   ->
        Lwt.async (fun () -> safe_apply fn buf) ; Ok ()
      | Error `Canceled          -> Error `Disconnected
      | Error `Invalid_argument  -> Error `Invalid_argument
      | Error `Unspecified_error -> Error `Unspecified_error
    in
    process () >>= (function
      | Ok () -> (listen[@tailcall]) t fn
      | Error e -> Lwt.return (Error e))
  | false -> Lwt.return (Ok ())

(* Transmit a packet from a Cstruct.t *)
let write t buf =
  let open Cstruct in
  let r = match Wifi.write t.interface buf.buffer buf.len with
    | Ok _      ->
      t.stats.tx_pkts <- Int32.succ t.stats.tx_pkts;
      t.stats.tx_bytes <- Int64.add t.stats.tx_bytes (Int64.of_int buf.len);
      Ok ()
    | Error Wifi.Invalid_argument  -> Error `Invalid_argument
    | Error Wifi.Unspecified -> Error `Unspecified_error
    | Error _   -> assert false 
    
  in
  Lwt.return r

let writev t = function
  | []       -> Lwt.return (Ok ())
  | [buffer] -> write t buffer
  | buffers  ->
    write t @@ Cstruct.concat buffers

let mac t = t.mac

let get_stats_counters t = t.stats

let reset_stats_counters t =
  t.stats.rx_bytes <- 0L;
  t.stats.rx_pkts  <- 0l;
  t.stats.tx_bytes <- 0L;
  t.stats.tx_pkts <- 0l