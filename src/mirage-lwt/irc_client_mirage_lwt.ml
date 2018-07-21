module Io_mirage_lwt
    (S : Mirage_stack_lwt.V4)
    (Time : Mirage_time_lwt.S)
    (Pclock : Mirage_clock.PCLOCK) =
  functor (T : sig val tcp : S.TCPV4.t val pclock : Pclock.t end) ->
  struct
  module TCP = S.TCPV4
  open T
  type 'a t = 'a Lwt.t
  let (>>=) = Lwt.bind
  let return = Lwt.return

  type file_descr = {
    flow : TCP.flow;
    mutable buffer : TCP.buffer;
  }
  type inet_addr = Ipaddr.V4.t
  type config = unit

  let open_socket ?(config=()) addr port =
    TCP.create_connection tcp (addr, port) >>= function
    | Ok flow -> return { flow; buffer = Cstruct.empty }
    | Error _ -> (* FIXME *) Lwt.fail_with "lol"

  let close_socket { flow; _ } = TCP.close flow

  let read ({ flow; buffer } as fd) dbuf ofs len =
    if Cstruct.len buffer > 0
    then let len' = min len (Cstruct.len buffer) in
      Cstruct.blit_to_bytes buffer 0 dbuf ofs len';
      fd.buffer <- Cstruct.shift buffer len';
      Lwt.return len'
    else
      TCP.read flow >>= function
      | Ok `Eof -> Lwt.return 0
      | Ok (`Data buffer) ->
        let len' = min len (Cstruct.len buffer) in
        Cstruct.blit_to_bytes buffer 0 dbuf ofs len';
        fd.buffer <- Cstruct.shift buffer len';
        Lwt.return len'
      | Pervasives.Error _ -> (* FIXME *) Lwt.fail_with "lol"

  let write { flow; _ } buffer ofs len =
    let buffer = Cstruct.of_bytes buffer in
    let buffer = Cstruct.sub buffer ofs len in
    TCP.write flow buffer >>= function
    | Pervasives.Ok () -> return len
    | Pervasives.Error _ -> (* FIXME *) Lwt.fail_with "lol"

  let read_with_timeout ~timeout fd buf off len =
    let open Lwt.Infix in
    Lwt.pick
      [ (read fd buf off len >|= fun i -> Some i);
        (Time.sleep_ns (Duration.of_sec timeout) >|= fun () -> None);
      ]

  let gethostbyname name =
    Lwt.return_nil

  let iter = Lwt_list.iter_s
  let sleep d = Time.sleep_ns (Duration.of_sec d)
  let catch = Lwt.catch
  let time () = let (day, ps) = Pclock.now_d_ps pclock in
    float_of_int (day * 24 * 60 * 60) +.
    Int64.to_float (Int64.div ps 1_000_000_000_000L)

  let pick = Some Lwt.pick
end

module Make
    (S : Mirage_stack_lwt.V4)
    (Time : Mirage_time_lwt.S)
    (Pclock : Mirage_clock.PCLOCK)
    (T : sig val tcp : S.TCPV4.t val pclock : Pclock.t end) = struct
  include Irc_client.Make(Io_mirage_lwt(S)(Time)(Pclock)(T))
end
