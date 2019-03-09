(** RingCast *)

module View = Map.Make(String)
module PsqID = Psq.Make(String)(Int64)

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

type seenq =
  {
    psq: PsqID.t;
    maxp: Int64.t;
    len: int;
  }

module SeenQ = struct
  let empty len =
    { psq = PsqID.empty; maxp = Int64.min_int; len }
end

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

(** find the closest neighbour with lower ID
    to [(nid, ndata)] from [view]
    using the [distance] function to sort nodes by ID *)
let predecessor view nid ndata distance =
  let dlist =
    List.stable_sort cmp_dist @@
      List.sort (fun _n1 _n2 -> if Random.bool () then 1 else -1) @@
        View.fold
          (fun id node lst ->
            { dist = distance nid ndata id node.data; id; node } :: lst)
          view
          [] in
  List.fold_left
    (fun (succ_nid, succ_node) node ->
      if node.dist < 0 then
        match (succ_nid, succ_node) with
        | (None, None) -> (Some node.id, Some node.node)
        | _ -> (succ_nid, succ_node)
      else
        (succ_nid, succ_node))
    (None, None)
    dlist

(** find the closest neighbour with higher ID
    to [(nid, ndata)] from [view]
    using the [distance] function to sort nodes by ID *)
let successor view nid ndata distance =
  let dlist =
    List.stable_sort cmp_dist @@
      List.sort (fun _n1 _n2 -> if Random.bool () then 1 else -1) @@
        View.fold
          (fun id node lst ->
            { dist = distance nid ndata id node.data; id; node } :: lst)
          view
          [] in
  List.fold_left
    (fun (succ_nid, succ_node) node ->
      if 0 < node.dist then
        match (succ_nid, succ_node) with
        | (None, None) -> (Some node.id, Some node.node)
        | _ -> (succ_nid, succ_node)
      else
        (succ_nid, succ_node))
    (None, None)
    dlist

(** retrieve a node to exchange with and a list of nodes to send
    from the union of [view] and [view_ext] *)
let make_exchange view view_ext xchg_len my_nid my_ndata distance =
  match oldest view with
  | Some (onid, onode) ->
     let view = View.remove onid view in
     (Some onid,
      Some onode.data,
      (let uview = View.union (* prefer nodes from view *)
                     (* TODO: compare profile versions instead *)
                     (fun _nid node _node_rnd -> Some node)
                     (add my_nid my_ndata view)
                     view_ext in
       closest uview onid onode.data xchg_len distance),
      inc_age view)
  | None ->
     (None, None, View.empty, view)

(** respond to an exchange request from [nid] *)
let make_response view view_ext xchg_len rnid rndata recvd my_nid my_ndata distance =
  let uview = add my_nid my_ndata view in
  let uview = View.union (* prefer nodes from view *)
                (fun _nid node _node_rnd -> Some node)
                uview view_ext in
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
let merge_recvd view view_len recvd xchg_len my_nid my_ndata distance =
  let recvd = zero_age recvd in
  let recvd = View.remove my_nid recvd in
  let recvd = truncate recvd xchg_len in
  let uview = View.union
               (* prefer nodes from view *)
                (fun _nid node _node_rnd -> Some node)
                view recvd in
  closest uview my_nid my_ndata view_len distance

(** pick a random node from [view] and
    return [(nid, node, view)]
    where view is the view with node removed *)
let random_node view =
  let view_len = View.cardinal view in
  if 0 < view_len then
    let (rnid, rnode, view, _, _) =
      View.fold
        (fun nid node a ->
          let (rnid, rnode, view, r, n) = a in
          if (n = r)
          then (Some nid, Some node, view, r, n + 1)
          else (rnid, rnode, View.add nid node view, r, n + 1))
        view
        (None, None, View.empty, Random.int (View.cardinal view), 0)
    in
    (rnid, rnode, view)
  else
    (None, None, view)

(** return [len] random nodes from [view] *)
let rec random_nodes ?(nodes = View.empty) view len =
  if 0 < len then
    match random_node view with
    | (Some rnid, Some rnode, view) ->
       let nodes = View.add rnid rnode nodes in
       random_nodes view (len - 1) ~nodes
    | _ -> nodes
  else nodes

(** selects [fanout] nodes a message should be forwarded to *)
let fwd_targets view seen msgid fanout rnid rndata my_nid my_ndata distance =
  if PsqID.mem msgid seen.psq then (* already seen *)
    (View.empty, seen)
  else
    let targets =
      let d = distance my_nid my_ndata rnid rndata in
      let rnd_len =
        if d = 0 then fanout - 2 (* msg from self *)
        else fanout - 1 in
      let targets = random_nodes view rnd_len in
      if 0 < d then (* msg from successor, fwd to predecessor *)
        let (pnid, pnode) = predecessor view my_nid my_ndata distance in
        match (pnid, pnode) with
        | (Some pnid, Some pnode) ->
           View.add pnid pnode targets
        | _ -> targets
      else if d < 0 then (* msg from predecessor fwd to successor *)
        let (snid, snode) = successor view my_nid my_ndata distance in
        match (snid, snode) with
        | (Some snid, Some snode) ->
           View.add snid snode targets
        | _ -> targets
      else (* msg from self, fwd to both successor & predecessor *)
        let (pnid, pnode) = predecessor view my_nid my_ndata distance in
        let (snid, snode) = successor view my_nid my_ndata distance in
        let targets =
          match (pnid, pnode) with
          | (Some pnid, Some pnode) ->
             View.add pnid pnode targets
          | _ -> targets in
        match (snid, snode) with
        | (Some snid, Some snode) ->
           View.add snid snode targets
        | _ -> targets in
    let seen =
      { psq =
          (let psq = PsqID.add msgid seen.maxp seen.psq in
           if PsqID.size seen.psq < seen.len
           then psq
           else match PsqID.pop seen.psq with
                | Some ((_, _), rest) -> rest
                | _ -> psq);
        maxp = Int64.add seen.maxp 1L;
        len = seen.len
      } in
    (targets, seen)
