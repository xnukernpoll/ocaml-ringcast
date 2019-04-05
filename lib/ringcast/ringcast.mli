(** RingCast: P2P hybrid dissemination protocol *)

type 'data node =
  {
    age: int;
    data: 'data;
  }
(** a node's profile:
    - [age]: age of this node profile, incremented after each gossip round
    - [data]: application-specific data associated with the node
 *)

module View : module type of Map.Make(String)
(** local, partial view of other node's profiles from the network *)

type seenq
(** queue of recently seen message IDs *)

module SeenQ : sig
  val empty : int -> seenq
  (** [empty len] returns an empty queue with maximum length [len] *)
end

val add :
  View.key
  -> 'data
  -> 'data node View.t
  -> 'data node View.t
(** [add_node id data view]
    adds an entry to [view]
    returns new view with the added entry *)

val remove :
  View.key
  -> 'data node View.t
  -> 'data node View.t
(** [remove_node id view]
    removes an entry from [view]
    returns new view without the removed entry *)

val make_exchange :
  'data node View.t
  -> 'data node View.t
  -> int
  -> View.key
  -> 'data
  -> (View.key -> 'data -> View.key -> 'data -> int)
  -> (View.key option * 'data option * 'data node View.t * 'data node View.t)
(** [view view_ext my_nid my_data xchg_len distance]
    selects a node to exchange with and a list of nodes to send
    from the union of [view] and [view_ext]
    - [view] is the current view of this node
    - [view_ext] is the current view of the topology management service (VICINITY)
    - [xchg_len] is the number of nodes in the gossip exchange
    - [my_nid] is the ID of this node,
    - [my_data] is the data associated with this node,
    - [distance] is a function that returns the distance of two nodes

    return [(Some nid, Some ndata, xchg, view)] where
    [nid] is the node_id to exchange with
    [ndata] associated with [nid] in [view]
    [xchg] is the [xchg_len] nodes from the two views to send to [nid]
    [view] is the updated view with the age of all nodes increased
           and the node associated with [nid] removed *)

val make_response :
  'data node View.t
  -> 'data node View.t
  -> int
  -> View.key
  -> 'data
  -> 'data node View.t
  -> View.key
  -> 'data
  -> (View.key -> 'data -> View.key -> 'data -> int)
  -> 'data node View.t
(** [view view_ext xchg_len rnid rndata recvd my_nid my_data distance]
    responds to a gossip exchange initiated by [(rnid, rndata)]

    returns [xchg_len] nodes closest to [rnid]
    according to the [distance] function
    from the union of [view] and [view_ext]
    to be sent as a response to [rnid]

    - [view] is the current view of this node
    - [view_ext] is the current view of the topology management service (VICINITY)
    - [xchg_len] is the number of nodes in the gossip exchange
    - [my_nid] is the ID of this node
    - [my_data] is the data associated with this node
    - [distance] is a function that returns the distance of two nodes
 *)

val merge_recvd :
  'data node View.t
  -> int
  -> 'data node View.t
  -> int
  -> View.key
  -> 'data
  -> (View.key -> 'data -> View.key -> 'data -> int)
  -> 'data node View.t
(** [merge_recvd view view_len recvd xchg_len my_nid my_data distance]
    merges received nodes during an exchange to the current [view]

    - [view] is the current view of this node
    - [view_len] is the maximum number of nodes in [view]
    - [recvd] are the received nodes to be merged
    - [my_nid] is the ID of this node
    - [my_ndata] is the data associated with this node
    - [distance] is a function that returns the distance of two nodes
*)

val fwd_targets :
  'data node View.t
  -> seenq
  -> string
  -> int
  -> View.key
  -> 'data
  -> View.key
  -> 'data
  -> (View.key -> 'data -> View.key -> 'data -> int)
  -> 'data node View.t * seenq

(** [fwd_targets view seen seen_len msgid rnid rndata my_nid my_ndata fanout distance]
    selects [fanout] nodes a message should be forwarded to:
    - a message already seen is not forwarded
    - a message from a predecessor on the ring is forwarded to the closest successor
    - a message from a successor on the ring is forwarded to the closest predecessor
    - a message from self is forwarded to both the closest predecessor & successor
    - the remaining nodes are selected at random from [view]

    where:
    - [view] is the current view of this node
    - [seen] is a queue of recently seen message IDs
    - [seen_len] is the maximum length of the [seen] queue
    - [msgid] is the message ID
    - [fanout] is the number of nodes the message should be forwarded to
    - [rnid] is the node ID the message was received from
    - [rndata] is the data associated with [rnid]
    - [my_nid] is the ID of this node
    - [my_ndata] is the data associated with this node
    - [distance] is a function that returns the distance of two nodes
 *)

(** Subscriptions *)
module Sub : sig

  (** Subscribed rings *)
  module Rings : module type of Map.Make(String)

  (** [empty] contains no subscriptions *)
  val empty :
    'data node View.t Rings.t

  (** [add rid view] adds a ring with ID [rid] and [view] to the subscriptions  *)
  val add :
    string
    -> 'data node View.t
    -> 'data node View.t Rings.t
    -> 'data node View.t Rings.t

  (** [remove rid view] removes a ring from the subscriptions *)
  val remove :
    string
    -> 'data node View.t Rings.t
    -> 'data node View.t Rings.t

  (** [intersect t node is_member] returns a list of common rings with [node]

      [is_member node rid ring] determines whether a [node] is member of [ring] with ID [rid] *)
  val intersect :
    'data node View.t Rings.t
    -> 'data node
    -> ('data node -> string -> 'data node View.t -> bool)
    -> (string * 'data node View.t) list
end
