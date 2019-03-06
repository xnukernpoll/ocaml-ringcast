open Ringcast

type 'data t = {
    my_nid : View.key;
    my_data : 'data;
    mutable view : 'data node View.t;
    view_len : int;
    xchg_len : int;
    period : float;
    view_str : (unit -> 'data node View.t);
    distance : (View.key -> 'data -> View.key -> 'data -> int);
    send_cb : ('data t -> View.key -> 'data -> 'data node View.t -> 'data node View.t Lwt.t);
    recv_cb : ('data t -> View.key -> 'data -> 'data node View.t -> 'data node View.t -> 'data node View.t Lwt.t);
    view_cb : ('data t -> View.key -> 'data -> 'data node View.t -> unit Lwt.t);
}

let init my_nid my_data view view_len xchg_len period
      view_str distance send_cb recv_cb view_cb =
  { my_nid; my_data; view; view_len; xchg_len; period;
    view_str; distance; send_cb; recv_cb; view_cb }

let view t = t.view

(** wait for [delay] seconds,
    then return the result of thread [t],
    or cancel it if not finished yet **)
let timeout delay t =
  let%lwt _ = Lwt_unix.sleep delay in
  match Lwt.state t with
  | Lwt.Sleep    -> Lwt.cancel t; Lwt.return None
  | Lwt.Return v -> Lwt.return (Some v)
  | Lwt.Fail ex  -> Lwt.fail ex

(** initiate exchange with a node from [t.view],
    wait for response, and return merged view *)
let init_xchg t xnid xdata sent view =
  match (xnid, xdata) with
  | (Some nid, Some data) ->
     let%lwt recvd = t.send_cb t nid data sent in
     let%lwt recvd = t.recv_cb t t.my_nid t.my_data t.view recvd in
     t.view <- merge_recvd view t.view_len recvd t.xchg_len
                 t.my_nid t.my_data t.distance;
     let%lwt _ = t.view_cb t t.my_nid t.my_data t.view in
     Lwt.return t.view
  | _ ->
     Lwt.return t.view

(** run initiator:
    pick a random node from [t.view] to gossip with every [t.period] seconds *)
let rec run t =
  let (xnid, xdata, sent, xview)
    = make_exchange t.view (t.view_str ()) t.xchg_len
        t.my_nid t.my_data t.distance in
  let%lwt view = timeout t.period (init_xchg t xnid xdata sent xview) in
  let%lwt _ = Lwt.return (
                  t.view <- match view with
                            | Some v -> v     (* new, merged view *)
                            | _ -> xview) in  (* current view with timed out node removed *)
  run t

(** receive entries from a peer and send response *)
let recv t rnid rdata recvd =
  let sent = make_response t.view (t.view_str ()) t.xchg_len rnid rdata recvd
               t.my_nid t.my_data t.distance in
  let%lwt _ = t.send_cb t rnid rdata sent in
  let%lwt recvd = t.recv_cb t t.my_nid t.my_data t.view recvd in
  t.view <- merge_recvd t.view t.view_len recvd t.xchg_len
              t.my_nid t.my_data t.distance;
  Lwt.return t.view
