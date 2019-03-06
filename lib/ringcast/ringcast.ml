(** RingCast *)

module View = Map.Make(String)

type 'data node =
  {
    age: int;
    data: 'data;
  }

type 'data dist =
  {
    dist: int;
    id: string;
    node: 'data node;
  }

let add nid data view =
  View.add nid {data; age = 0} view

let remove nid view =
  View.remove nid view

let zero_age view  =
  View.mapi
    (fun _nid node -> {node with age = 0})
    view

let inc_age view  =
  View.mapi
    (fun _nid node -> {node with age = node.age + 1})
    view

(** retrieve oldest node from [view],
    in case there are multiple oldest nodes,
    pick a random one of those

    return [Some (nid, node)] or [None] if [view] is empty *)
let oldest view =
  match
    View.fold
      (fun nid node oldest ->
        match oldest with
        | None ->
           Some (nid, node, 1)
        | Some (_onid, onode, _on) when onode.age < node.age ->
           Some (nid, node, 1)
        | Some (onid, onode, on) when onode.age = node.age ->
           if Random.float 1. < 1. /. float_of_int (on + 1)
           then Some (nid, node, on + 1)
           else Some (onid, onode, on + 1)
        | _ -> oldest)
      view None
  with
  | Some (nid, node, _n) -> Some (nid, node)
  | None -> None

(** compare distance of two nodes, used for sorting *)
let cmp_dist n1 n2 =
  let n1d = abs n1.dist in
  let n2d = abs n2.dist in
  if n1d < n2d then -1
  else if n2d < n1d then 1
  else 0

(** select [n] closest neighbours (n/2 with lower and n/2 with higher ID)
    to [(nid, ndata)] from [view]
    using the [distance] function to sort nodes by ID *)
let closest view nid ndata n distance =
  let dlist =
    List.stable_sort cmp_dist @@
      List.sort (fun _n1 _n2 -> if Random.bool () then 1 else -1) @@
        View.fold
          (fun id node lst ->
            { dist = distance nid ndata id node.data; id; node } :: lst)
          view
          [] in
  let (dview_lo, dview_hi) =
    List.fold_left
      (fun (dview_lo, dview_hi) node ->
        let dview_lo =
          if node.dist < 0 then
            if View.cardinal dview_lo < n/2 then
              View.add node.id node.node dview_lo
            else dview_lo
          else dview_lo in
        let dview_hi =
          if 0 < node.dist then
            if View.cardinal dview_hi < n/2 then
              View.add node.id node.node dview_hi
            else dview_hi
          else dview_hi in
        (dview_lo, dview_hi))
      (View.empty, View.empty)
      dlist
  in
  View.union (fun _nid node1 _node2 -> Some node1) dview_lo dview_hi

(** retrieve a node to exchange with and a list of nodes to send
    from the union of [view] and [view_str] *)
let make_exchange view view_str xchg_len my_nid my_data distance =
  match oldest view with
  | Some (onid, onode) ->
     let view = View.remove onid view in
     (Some onid,
      Some onode.data,
      (let uview = View.union (* prefer nodes from view *)
                     (fun _nid node _node_rnd -> Some node)
                     (add my_nid my_data view)
                     view_str in
       closest uview onid onode.data xchg_len distance),
      inc_age view)
  | None ->
     (None, None, View.empty, view)

(** respond to an exchange request from [nid] *)
let make_response view view_str xchg_len rnid rndata recvd my_nid my_data distance =
  let uview = add my_nid my_data view in
  let uview = View.union (* prefer nodes from view *)
                (fun _nid node _node_rnd -> Some node)
                uview view_str in
  let uview = View.filter (* remove recvd nodes *)
                (fun nid _node -> not @@ View.mem nid recvd)
                uview in
  closest uview rnid rndata xchg_len distance

(** truncate [view] to [len] nodes *)
let rec truncate ?(view2=View.empty) view len =
  if len <= 0 || View.is_empty view then
    view
  else
    let (nid, node) = View.choose view in
    truncate ~view2:(View.add nid node view2) view (len - 1)

(** merge nodes received during an exchange with current view,
    [my_nid] is the key associated with this node *)
let merge_recvd view view_len recvd xchg_len my_nid my_data distance =
  let recvd = zero_age recvd in
  let recvd = View.remove my_nid recvd in
  let recvd = truncate recvd xchg_len in
  let uview = View.union
               (* prefer nodes from view *)
                (fun _nid node _node_rnd -> Some node)
                view recvd in
  closest uview my_nid my_data view_len distance
