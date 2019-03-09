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
  -> int
  -> int
  -> (unit -> 'data node View.t)
  -> (View.key -> 'data -> View.key -> 'data -> int)
  -> ('data t -> View.key -> 'data -> 'data node View.t -> 'data node View.t Lwt.t)
  -> ('data t -> View.key -> 'data -> 'data node View.t -> 'data node View.t -> 'data node View.t Lwt.t)
  -> ('data t -> View.key -> 'data -> 'data node View.t -> unit Lwt.t)
  -> ('data t -> View.key -> 'data -> string -> Cstruct.t -> unit Lwt.t)
  -> 'data t
(** [init my_nid my_data view view_len xchg_len period fanout seen_len view_str distance send_cb recv_cb view_cb send_msg_cb]
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
    returns the current local partial view of the network *)

val run :
  'data t
  -> unit Lwt.t
(** [run t send_cb recv_cb view_cb]
    runs initiator:
    pick a random node from [view] to gossip with every [period] seconds
 **)

val recv_gossip :
  'data t
  -> View.key
  -> 'data
  -> 'data node View.t
  -> 'data node View.t Lwt.t
(** [recv_gossip t nid ndata recvd]
    receives a gossip message from a node ([nid, ndata]) and sends a response;
    runs [recv_cb] with the received entries, and
    returns the updated view *)

val recv_msg :
  'data t
  -> View.key
  -> 'data
  -> string
  -> Cstruct.t
  -> unit Lwt.t
(** [recv_msg t nid ndata msgid msg]
    receives a message from a node ([nid, ndata]) and
    selects nodes from the local view it should be forwarded to
    (predecessor or successor on the ring and some random nodes,
     limited by [fanout]);
    calls [send_msg_cb] for each node the message should be forwarded to
*)
