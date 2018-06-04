(** ESP32 interface for Ethernet I/O. *)
include Mirage_net_lwt.S
val connect : string -> t Lwt.t