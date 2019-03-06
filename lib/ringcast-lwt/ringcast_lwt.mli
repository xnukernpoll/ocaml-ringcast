(** RingCast *)

(** {2 Ringcast_lwt} *)

(** High-level library implementing the RingCast protocol using Lwt *)

open Ringcast

type 'data t

val init :
  View.key
  -> 'data
  -> 'data node View.t
  -> int
  -> int
  -> float
  -> (unit -> 'data node View.t)
  -> (View.key -> 'data -> View.key -> 'data -> int)
  -> ('data t -> View.key -> 'data -> 'data node View.t -> 'data node View.t Lwt.t)
  -> ('data t -> View.key -> 'data -> 'data node View.t -> 'data node View.t -> 'data node View.t Lwt.t)
  -> ('data t -> View.key -> 'data -> 'data node View.t -> unit Lwt.t)
  -> 'data t
(** [init my_nid my_data view view_len xchg_len period view_str distance send_cb recv_cb view_cb]
    initializes a RingCast instance with the following configuration:
    - [my_nid] - ID of this node,
    - [my_data] - data associated with [my_nid] in view,
    - [view] - map of neighbour entries with peer ID as key,
    - [view_len] - max view length,
    - [xchg_len] - number of entries to exchange at each period,
    - [period] - gossip period, in seconds,
    - [view_str] - function that returns the current view
      of the topology management protocol (VICINITY),
      responsible for creating the structure of the overlay.
    - [distance] - function to calculate distance between two nodes
    - [send_cb nid ndata xchg] - send [xchg] entries to node [(nid, ndata)]
    - [recv_cb my_nid my_data view recvd] - called after receiving entries
      during an exchange; allows rewriting [recvd] entries with the returned value
    - [view_cb my_nid my_node view] - called after the view has been updated
 *)

val view :
  'data t
  -> 'data node View.t
(** [view t]
    retrieve current view *)

val run :
  'data t
  -> unit Lwt.t
(** [run t send_cb recv_cb view_cb]
    run initiator:
    pick a random node from [view] to gossip with every [period] seconds
 **)

val recv :
  'data t
  -> View.key
  -> 'data
  -> 'data node View.t
  -> 'data node View.t Lwt.t
(** [recv t rnid rdata recvd]
    receive entries from a peer and send response;
    run [recv_cb] with received entries, and
    return updated view *)
