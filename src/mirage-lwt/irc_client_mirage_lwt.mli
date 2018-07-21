module Make : functor
  (S : Mirage_stack_lwt.V4)
  (Time : Mirage_time_lwt.S)
  (Pclock : Mirage_clock.PCLOCK)
  (T : sig val tcp : S.TCPV4.t val pclock : Pclock.t end) ->
  sig
    include Irc_client.CLIENT
      with type 'a Io.t = 'a Lwt.t
       and type Io.inet_addr = Ipaddr.V4.t
       and type Io.config = unit
  end
