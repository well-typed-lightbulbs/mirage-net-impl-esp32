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
  id: string;
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

let pp_error ppf = function
  | #Mirage_net.error as e -> Mirage_net.pp_error ppf e
  | `Invalid_argument      -> Fmt.string ppf "Invalid argument"
  | `Unspecified_error     -> Fmt.string ppf "Unspecified error"
  | `Exn e                 -> Fmt.exn ppf e

type esp32_net_info = {
  mac_address: string;
  mtu: int;
}

external esp32_net_info:
  unit -> esp32_net_info = "mirage_esp32_net_info"
external esp32_net_read:
  Cstruct.buffer -> int -> esp32_result * int = "mirage_esp32_net_read"
external esp32_net_write:
  Cstruct.buffer -> int -> esp32_result = "mirage_esp32_net_write"

let devices = Hashtbl.create 1

let connect devname =
  let ni = esp32_net_info () in
  match Macaddr.of_bytes ni.mac_address with
  | None -> Lwt.fail_with "Netif: Could not get MAC address"
  | Some mac ->
     Log.info (fun f -> f "Plugging into %s with mac %s"
                        devname (Macaddr.to_string mac));
     let active = true in
     (* XXX: hook up ni.mtu *)
     let t = {
         id=devname; active; mac;
         stats= { rx_bytes=0L;rx_pkts=0l; tx_bytes=0L; tx_pkts=0l } }
     in
     Hashtbl.add devices devname t;
     Lwt.return t

let disconnect t =
  Log.info (fun f -> f "Disconnect %s" t.id);
  t.active <- false;
  Lwt.return_unit

type macaddr = Macaddr.t
type page_aligned_buffer = Io_page.t
type buffer = Cstruct.t

(* Input a frame, and block if nothing is available *)
let rec read t buf =
  let process () =
    let r = match esp32_net_read buf.Cstruct.buffer buf.Cstruct.len with
      | (ESP32_OK, len)    ->
        t.stats.rx_pkts <- Int32.succ t.stats.rx_pkts;
        t.stats.rx_bytes <- Int64.add t.stats.rx_bytes (Int64.of_int len);
        let buf = Cstruct.sub buf 0 len in
        Ok buf
      | (ESP32_AGAIN, _)   -> Error `Continue
      | (ESP32_EINVAL, _)  -> Error `Invalid_argument
      | (ESP32_EUNSPEC, _) -> Error `Unspecified_error
    in
    Lwt.return r
  in
  process () >>= function
  | Ok buf                   -> Lwt.return (Ok buf)
  | Error `Continue          ->
    OS.Main.wait_for_work () >>= fun () -> read t buf
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
  let r = match esp32_net_write buf.buffer buf.len with
    | ESP32_OK      ->
      t.stats.tx_pkts <- Int32.succ t.stats.tx_pkts;
      t.stats.tx_bytes <- Int64.add t.stats.tx_bytes (Int64.of_int buf.len);
      Ok ()
    | ESP32_AGAIN   -> assert false (* Not returned by solo5_net_write() *)
    | ESP32_EINVAL  -> Error `Invalid_argument
    | ESP32_EUNSPEC -> Error `Unspecified_error
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