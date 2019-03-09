open Ringcast
open OUnit2

module R = Ringcast
module RL = Ringcast_lwt

let view_len = 8
let xchg_len = 4
let period = 1.0
let fanout = 5
let seen_len = 2

let recvd_msgs = Hashtbl.create 3

let print_view nid msg view =
  Lwt.ignore_result (Lwt_io.printf "%s # view: %s (%d)\n"
                       nid msg (R.View.cardinal view));
  R.View.iter (fun id n ->
      Lwt.ignore_result (Lwt_io.printf " - %s: %s (%d)\n%!" id n.data n.age))
    view

let print_xchg nid msg xchg =
  Lwt.ignore_result (Lwt_io.printf "%s # xchg: %s (%d)\n"
                       nid msg (R.View.cardinal xchg));
  R.View.iter (fun id n ->
      Lwt.ignore_result (Lwt_io.printf " - %s: %s\n%!" id n.data))
    xchg

let get_view_rnd () =
  add "RA" "113"
    (add "RB" "133"
       (add "RC" "153"
          (add "RD" "173"
             (add "RE" "233"
                (add "RF" "253"
                   (add "RG" "273"
                      View.empty))))))

let distance _nid1 data1 _nid2 data2 =
  let min_nid = 0 in
  let max_nid = 100 in
  let nid1 = int_of_string data1 in
  let nid2 = int_of_string data2 in
  let d = abs (nid2 - nid1) in
  let d = if d <= (max_nid - min_nid) / 2
          then d
          else max_nid + 1 - d in
  if (nid1 - nid2 = d) || (nid1 < nid2 && nid2 - nid1 != d)
  then -1 * d
  else d

let rec read_ch ch c nid rnid rdata =
  let%lwt recvd = Lwt_io.read_value ch in
  print_xchg nid "read_ch" recvd;
  let%lwt view = RL.recv_gossip c rnid rdata recvd in
  print_view nid "read_ch" view;
  read_ch ch c nid rnid rdata

let rec read_msg_ch ch c nid rnid rndata =
  let%lwt msgid = Lwt_io.read_value ch in
  let%lwt msg = Lwt_io.read_value ch in
  let%lwt _ = Lwt_io.printf "%s # read_msg_ch: %s (%s)\n"
                nid msgid (Cstruct.to_string msg) in
  let%lwt _ = RL.recv_msg c rnid rndata msgid msg in
  read_msg_ch ch c nid rnid rndata

let send_msg delay c nid ndata msgid msg =
  let%lwt _ = Lwt_unix.sleep delay in
  RL.recv_msg c nid ndata msgid msg

let recvd_msg msgid =
  let n =
    match Hashtbl.find_opt recvd_msgs msgid with
    | Some n -> n
    | None -> 0 in
  Hashtbl.add recvd_msgs msgid (n + 1)

let check_recvd_msgs delay =
  let%lwt _ = Lwt_unix.sleep delay in
  let%lwt _ = Lwt_io.printf "check_recvd_msgs: %d %d %d\n"
                (Hashtbl.find recvd_msgs "1")
                (Hashtbl.find recvd_msgs "2")
                (Hashtbl.find recvd_msgs "3") in
  assert_equal (Hashtbl.find recvd_msgs "1") 2;
  assert_equal (Hashtbl.find recvd_msgs "2") 1;
  assert_equal (Hashtbl.find recvd_msgs "3") 1;
  Lwt_io.printf "check_recvd_msgs: OK\n"

let () =
  let (read_ch1, write_ch2) = Lwt_io.pipe () in
  let (read_ch2, write_ch1) = Lwt_io.pipe () in
  let (read_msg_ch1, write_msg_ch2) = Lwt_io.pipe () in
  let (read_msg_ch2, write_msg_ch1) = Lwt_io.pipe () in

  let nid1 = "ABC123" in
  let data1 = "180" in
  let view1 = R.add "v1k1" "110"
                (R.add "v1k2" "120"
                   (R.add "v1k3" "130"
                      (R.add "v1k4" "140"
                         (R.add "v1k5" "150"
                            (R.add "v1k6" "160"
                               (R.add "v1k7" "170"
                                  R.View.empty)))))) in
  let nid2 = "DEF456" in
  let data2 = "200" in
  let view2 = R.add "v2k1" "210"
                (R.add "v2k2" "220"
                   (R.add "v2k3" "230"
                      (R.add "v2k4" "240"
                         (R.add "v2k5" "250"
                            (R.add "v2k6" "260"
                               (R.add "v2k6" "270"
                                  R.View.empty)))))) in
  let c1 =
    RL.init nid1 data1 view1 view_len xchg_len period fanout seen_len
      get_view_rnd distance
      (fun _c nid data entries ->
        let%lwt _ = Lwt_io.printf "%s # send_cb: %s (%s)\n" nid1 nid data in
        print_xchg nid1 "send_cb: entries to send" entries;
        let%lwt _ = Lwt_io.write_value write_ch1 entries in
        Lwt_io.read_value read_ch1)
      (fun _c my_nid _my_data my_view recvd ->
        print_view my_nid "recv_cb" my_view;
        print_xchg my_nid "recv_cb" recvd;
        Lwt.return recvd)
      (fun _c my_nid _my_data my_view ->
        print_view my_nid "updated" my_view;
        Lwt.return_unit)
      (fun _c nid ndata msgid msg ->
        let%lwt _ = Lwt_io.printf "%s # send_msg_cb: to %s (%s) msg %s (%s)\n"
                      nid1 nid ndata msgid (Cstruct.to_string msg) in
        if nid = nid2 then
          (let%lwt _ = Lwt_io.write_value write_msg_ch1 msg in
           recvd_msg msgid;
           Lwt.return_unit)
        else
          Lwt.return_unit
      )
  in
  let c2 =
    RL.init nid2 data2 view2 view_len xchg_len period fanout seen_len
      get_view_rnd distance
      (fun _c nid data entries ->
        let%lwt _ = Lwt_io.printf "%s # send_cb: %s (%s)\n" nid2 nid data in
        print_xchg nid2 "send_cb: entries to send" entries;
        let%lwt _ = Lwt_io.write_value write_ch2 entries in
        Lwt_io.read_value read_ch2)
      (fun _c _my_nid _my_data _my_view recvd ->
        Lwt.return recvd)
      (fun _c my_nid _my_data my_view ->
        print_view my_nid "after round" my_view;
        Lwt.return_unit)
      (fun _c nid ndata msgid msg ->
        let%lwt _ = Lwt_io.printf "%s # send_msg_cb: to %s (%s) msg %s (%s)\n"
                      nid2 nid ndata msgid (Cstruct.to_string msg) in
        if nid = nid1 then
          (let%lwt _ = Lwt_io.write_value write_msg_ch2 msg in
           recvd_msg msgid;
           Lwt.return_unit)
        else
          Lwt.return_unit
      )
  in
  let timeout = Lwt_unix.sleep 5.5
  in
  Random.self_init ();
  Lwt_main.run @@
    Lwt.join [
        Lwt.pick [
            RL.run c1;
            RL.run c2;
            read_ch read_ch1 c1 nid1 nid2 data2;
            read_ch read_ch2 c2 nid2 nid1 data1;
            read_msg_ch read_msg_ch1 c1 nid1 nid2 data2;
            read_msg_ch read_msg_ch2 c2 nid2 nid1 data1;
            timeout;
          ];
        send_msg 1.0 c1 nid1 data1
          "1" (Cstruct.of_string
                 (Printf.sprintf "first message from %s" nid1));
        send_msg 4.0 c1 nid1 data1
          "1" (Cstruct.of_string
                 (Printf.sprintf "first message #2 from %s" nid1));
        send_msg 2.0 c1 nid1 data1
          "2" (Cstruct.of_string
                 (Printf.sprintf "second message from %s" nid1));
        send_msg 3.0 c1 nid1 data1
          "3" (Cstruct.of_string
                 (Printf.sprintf "third message from %s" nid1));
        send_msg 4.0 c1 nid1 data1
          "1" (Cstruct.of_string
                 (Printf.sprintf "first message #3 from %s" nid1));
        check_recvd_msgs 5.3
      ]
