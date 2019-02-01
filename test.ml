open OUnit2
open State
open Ai

(** [move_p dir st] is [State.move st dir] *)
let move_p d st = State.move st d

(** [rotate_p dir st] is [State.rotate st dir] *)
let rotate_p d st = State.rotate st d


(** [make_is_alive_tests name st expected_output] constructs an OUnit
    test named [name] that test the quality of [is_alive st] with 
    [expected_output]. *)
let make_is_alive_tests
    (name: string)
    (st: t)
    (expected_output : bool) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (is_alive st))

(** [repeat_apply f a t] is [f] applied to [a] a total of [t] times *)
let rec repeat_apply f a t =
  if t = 0 then a else
    repeat_apply f (f a) (t - 1)

let rec repeat_next a t = 
  if t = 0 then a else
    repeat_next (a |> next |> fst) (t - 1)

let rec repeat_hard_drop a t = 
  if t = 0 then a else
    repeat_hard_drop (a |> hard_drop |> next |> fst) (t - 1)

let death_tests =
  [
    make_is_alive_tests "is alive 1" (start ()) true;
    make_is_alive_tests "is alive 2" (move (start ()) true) true;
    make_is_alive_tests "is alive 3" (move (start ()) false) true;
    make_is_alive_tests "is alive 4" (next (start ()) |> fst) true;
    make_is_alive_tests "is alive rotate 1" (rotate (start ()) true) true;
    make_is_alive_tests "is alive rotate 2" (rotate (start ()) false) true;
    make_is_alive_tests "is alive drop" 
      (start () |> hard_drop |> next |> fst) true;
    make_is_alive_tests "is alive drop 9" 
      (repeat_hard_drop (start ()) 9) true;
    make_is_alive_tests "is alive repeat move down" 
      (repeat_next (start_custom [] [0;1;2]) 40) true;
    make_is_alive_tests "is alive death" 
      (repeat_hard_drop (start ()) 20) false;
    make_is_alive_tests "lock out"
      (repeat_apply (move_p true) 
         (start_custom [(19,0);(19,5)] [1; 2]) 5 |> next |> fst) 
      false;
    make_is_alive_tests "spawn out of bounds"
      ((start_custom [(19,5)] [0; 1; 2]) |> move_p true |> hard_drop) true;
    make_is_alive_tests "block out"
      ((start_custom [(19,5)] [0; 2]) |> next |> fst) false;
  ]

(** [print_tuple (a, b)] is a string of form "([a], [b]) " that represents
    [(a, b)]. *)
let print_tuple (a, b) = 
  "(" ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ") "

(** [print_lst acc lst] is a string of form "([a1], [b1]) ([a2], [b2]) ..." 
    that represents [lst] that is [(a1, b1); (a2, b2); ...]. *)
let rec print_lst acc (lst : (int * int) list) = match lst with
  | [] -> acc
  | h :: t -> print_lst (acc ^ print_tuple h) t

(** [make_pos_tests name st expected_output] constructs an OUnit
    test named [name] that test the quality of [get_curr_piece st] with 
    [expected_output], ignoring the ordering of the list. *)
let make_pos_tests
    (name: string)
    (st: t)
    (expected_output : (int * int) list) : test = 
  name >:: (fun _ ->
      assert_equal 
        (List.sort_uniq Pervasives.compare expected_output)
        (st |> get_curr_piece |> List.sort_uniq Pervasives.compare)
        ~printer: (fun x -> print_lst "" x))

let spawn_tests =
  [
    make_pos_tests "Spawn O"
      (start_custom [] [0; 0]) [(19, 5); (19, 4); (20, 5); (20, 4)];
    make_pos_tests "Spawn O Blocked"
      (start_custom [19,4] [0; 0]) [(20, 5); (20, 4); (21, 5); (21, 4)];
    make_pos_tests "Spawn I"
      (start_custom [] [1; 1; 1]) [(19, 3); (19, 4); (19, 5); (19, 6)];
    make_pos_tests "Spawn I Blocked"
      (start_custom [19,3] [1; 1; 1]) [(20, 3); (20, 4); (20, 5); (20, 6)];
    make_pos_tests "Spawn T"
      (start_custom [] [2; 2]) [(19, 3); (19, 4); (19, 5); (20, 4)];
    make_pos_tests "Spawn T Blocked"
      (start_custom [19,3] [2; 2]) [(20, 3); (20, 4); (20, 5); (21, 4)];
    make_pos_tests "Spawn L"
      (start_custom [] [3; 3]) [(19, 3); (19, 4); (19, 5); (20, 5)];
    make_pos_tests "Spawn L Blocked"
      (start_custom [19,3] [3; 3]) [(20, 3); (20, 4); (20, 5); (21, 5)];
    make_pos_tests "Spawn J"
      (start_custom [] [4; 4]) [(19, 3); (19, 4); (19, 5); (20, 3)];
    make_pos_tests "Spawn J Blocked"
      (start_custom [19,3] [4; 4]) [(20, 3); (20, 4); (20, 5); (21, 3)];
    make_pos_tests "Spawn S"
      (start_custom [] [5; 5]) [(19, 3); (19, 4); (20, 4); (20, 5)];
    make_pos_tests "Spawn S Blocked"
      (start_custom [19,3] [5; 5]) [(20, 3); (20, 4); (21, 4); (21, 5)];
    make_pos_tests "Spawn Z"
      (start_custom [] [6; 6]) [(19, 5); (19, 4); (20, 4); (20, 3)];
    make_pos_tests "Spawn Z Blocked"
      (start_custom [20,3] [6;6]) [(20, 5); (20, 4); (21, 4); (21, 3)];
  ]

let move_laterally_tests =
  [
    make_pos_tests "Move Left"
      ((start_custom [] [0; 0]) |> move_p true) 
      [(19, 4); (19, 3); (20, 4); (20, 3)];
    make_pos_tests "Move Right"
      ((start_custom [] [0; 0]) |> move_p false) 
      [(19, 6); (19, 5); (20, 6); (20, 5)];
    make_pos_tests "Move Left Blocked"
      ((start_custom [19,3] [0; 0]) |> move_p true) 
      [(19, 5); (19, 4); (20, 5); (20, 4)];
    make_pos_tests "Move Right Blocked"
      ((start_custom [19,6] [0; 0]) |> move_p false) 
      [(19, 5); (19, 4); (20, 5); (20, 4)];
    make_pos_tests "Move Left Above"
      ((start_custom [19,4] [0; 0]) |> move_p true) 
      [(21, 4); (21, 3); (20, 4); (20, 3)];
    make_pos_tests "Move Right Above"
      ((start_custom [19,4] [0; 0]) |> move_p false) 
      [(21, 6); (21, 5); (20, 6); (20, 5)];
    make_pos_tests "Move Left Above Blocked"
      ((start_custom [19,4;20,3] [0; 0]) |> move_p true) 
      [(21, 5); (21, 4); (20, 5); (20, 4)];
    make_pos_tests "Move Right Above Blocked"
      ((start_custom [19,4; 20,6] [0; 0]) |> move_p false) 
      [(21, 5); (21, 4); (20, 5); (20, 4)];
  ]

let move_down_tests =
  [
    make_pos_tests "Move Down"
      ((start_custom [] [5; 5]) |> next |> fst) 
      [(19, 5); (19, 4); (18, 4); (18, 3)];
    make_pos_tests "Move Down 10"
      (repeat_next (start_custom [] [5; 5]) 10) 
      [(9, 3); (9, 4); (10, 4); (10, 5)];
    make_pos_tests "Move Down 19"
      (repeat_next (start_custom [] [5; 5]) 19) 
      [(0, 3); (0, 4); (1, 4); (1, 5)];
    make_pos_tests "Move Down 20 (Lock)"
      (repeat_next (start_custom [] [5; 5]) 20) 
      [(19, 3); (19, 4); (20, 4); (20, 5)];
    make_pos_tests "Move Down 20+17 (Lock Conf)"
      (repeat_next (start_custom [] [5; 5]) 37) 
      [(2, 3); (2, 4); (3, 4); (3, 5)];
    make_pos_tests "Move Down 20+17+1 (Lock Conf)"
      (repeat_next (start_custom [] [5; 5; 5]) 38) 
      [(19, 3); (19, 4); (20, 4); (20, 5)];
  ]

let hard_drop_tests =
  [
    make_pos_tests "Hard Drop + Lock"
      ((start_custom [] [5; 5]) |> hard_drop |> next |> fst) 
      [(19, 3); (19, 4); (20, 4); (20, 5)];
    make_pos_tests "Hard Drop + Lock Conf"
      ((start_custom [] [5; 5; 5]) |> 
       hard_drop |> next |> fst |> hard_drop |> next |> fst) 
      [(19, 3); (19, 4); (20, 4); (20, 5)];
    make_is_alive_tests "Hard Drop Death"
      ((start_custom [18,3] [5; 1; 5]) |> hard_drop |> next |> fst)
      false
  ]

let rotate_tests = 
  [
    make_pos_tests "Rotate O"
      ((start_custom [] [0;0]) |> rotate_p true) 
      [(19, 5); (19, 4); (20, 5); (20, 4)];
    make_pos_tests "Rotate O CC"
      ((start_custom [] [0;0]) |> rotate_p false) 
      [(19, 5); (19, 4); (20, 5); (20, 4)];
    make_pos_tests "Rotate I"
      ((start_custom [] [1;1]) |> rotate_p true) 
      [(17, 5); (18, 5); (19, 5); (20, 5)];
    make_pos_tests "Rotate I CC"
      ((start_custom [] [1;1]) |> rotate_p false) 
      [(17, 4); (18, 4); (19, 4); (20, 4)];
    make_pos_tests "Rotate T"
      ((start_custom [] [2;2]) |> rotate_p true) 
      [(18, 4); (19, 4); (19, 5); (20, 4)];
    make_pos_tests "Rotate T CC"
      ((start_custom [] [2;2]) |> rotate_p false) 
      [(18, 4); (19, 4); (19, 3); (20, 4)];
    make_pos_tests "Rotate L"
      ((start_custom [] [3;3]) |> rotate_p true) 
      [(20, 4); (19, 4); (18, 4); (18, 5)];
    make_pos_tests "Rotate L CC"
      ((start_custom [] [3;3]) |> rotate_p false) 
      [(20, 4); (19, 4); (18, 4); (20, 3)];
    make_pos_tests "Rotate J"
      ((start_custom [] [4;4]) |> rotate_p true) 
      [(18, 4); (19, 4); (20, 4); (20, 5)];
    make_pos_tests "Rotate J CC"
      ((start_custom [] [4;4]) |> rotate_p false) 
      [(18, 4); (19, 4); (20, 4); (18, 3)];
    make_pos_tests "Rotate S"
      ((start_custom [] [5;5]) |> rotate_p true) 
      [(19, 5); (19, 4); (20, 4); (18, 5)];
    make_pos_tests "Rotate S"
      ((start_custom [] [5;5]) |> rotate_p false) 
      [(19, 4); (19, 3); (20, 3); (18, 4)];
    make_pos_tests "Rotate Z"
      ((start_custom [] [6;6]) |> rotate_p true) 
      [(18, 4); (19, 4); (19, 5); (20, 5)];
    make_pos_tests "Rotate Z CC"
      ((start_custom [] [6;6]) |> rotate_p false) 
      [(18, 3); (19, 3); (19, 4); (20, 4)];
    make_pos_tests "Rotate I Point 2"
      ((repeat_apply 
          (move_p false) ((start_custom [] [1;1]) |> rotate_p true) 10) 
       |> rotate_p true)
      [(18, 9); (18, 8); (18, 7); (18, 6)];
    make_pos_tests "Rotate I Point 2 CC"
      ((repeat_apply 
          (move_p false) ((start_custom [] [1;1]) |> rotate_p false) 10) 
       |> rotate_p false)
      [(18, 9); (18, 8); (18, 7); (18, 6)];
    make_pos_tests "Rotate I Point 3"
      ((repeat_apply 
          (move_p false) ((start_custom [] [1;1]) |> rotate_p false) 10) 
       |> rotate_p true)
      [(19, 9); (19, 8); (19, 7); (19, 6)];
    make_pos_tests "Rotate I Point 3 CC"
      ((repeat_apply 
          (move_p false) ((start_custom [] [1;1]) |> rotate_p true) 10) 
       |> rotate_p false)
      [(19, 9); (19, 8); (19, 7); (19, 6)];
    make_pos_tests "Rotate I Point 4"
      ((repeat_next 
          ((start_custom [10,3;10,4;10,5;10,6] [1;1]) 
           |> rotate_p true |> rotate_p true) 7) |> rotate_p true)
      [(11, 6); (12, 6); (13, 6); (14, 6)];
    make_pos_tests "Rotate I Point 4 CC"
      ((start_custom [18,3;18,4;18,5;18,6] [1;1]) |> rotate_p false)
      [(19, 3); (20, 3); (21, 3); (22, 3)];
    make_pos_tests "Rotate I Point 5"
      ((start_custom [18,3;18,4;18,5;18,6] [1;1]) |> rotate_p true)
      [(19, 6); (20, 6); (21, 6); (22, 6)];
    make_pos_tests "Rotate I Point 5 CC"
      ((repeat_next 
          ((start_custom [10,3;10,4;10,5;10,6] [1;1]) 
           |> rotate_p true |> rotate_p true) 7) |> rotate_p false)
      [(11, 3); (12, 3); (13, 3); (14, 3)];
    make_pos_tests "Rotate Non-I Point 2"
      ((repeat_apply (move_p false) 
          ((start_custom [] [2;2]) |> rotate_p false) 7) |> rotate_p true)
      [(19, 7); (19, 8); (19, 9); (20, 8)];
    make_pos_tests "Rotate Non-I Point 2 CC"
      ((repeat_apply (move_p false) 
          ((start_custom [] [2;2]) |> rotate_p false) 7) |> rotate_p false)
      [(19, 7); (19, 8); (19, 9); (18, 8)];
    make_pos_tests "Rotate Non-I Point 3"
      ((repeat_next 
          (start_custom [10,3;10,4;10,5] [2;2]) 8) |> rotate_p true)
      [(11, 3); (12, 3); (13, 3); (12, 4)];
    make_pos_tests "Rotate Non-I Point 3 CC"
      ((repeat_next 
          (start_custom [10,3;10,4;10,5] [2;2]) 8) |> rotate_p false)
      [(11, 5); (12, 5); (13, 5); (12, 4)];
    make_pos_tests "Rotate Non-I Point 4"
      ((repeat_next  
          ((start_custom [11,2;12,2;13,2;14,2;15,2;11,5;12,5;13,5] [2;2])
           |> rotate_p false) 7) |> rotate_p true)
      [(14, 5); (14, 4); (14, 3); (15, 4)];
    make_pos_tests "Rotate Non-I Point 4 CC"
      ((repeat_next  
          ((start_custom [11,3;12,3;13,3;11,6;12,6;13,6;14,6;15,6] [2;2])
           |> rotate_p true) 7) |> rotate_p false)
      [(14, 5); (14, 4); (14, 3); (15, 4)];
    make_pos_tests "Rotate Non-I Point 5"
      ((repeat_next  
          ((start_custom [11,3;12,3;13,3;11,6;12,6;13,6;14,6;15,6] [2;2])
           |> rotate_p false |> move_p false) 7) |> rotate_p true)
      [(14, 5); (14, 4); (14, 3); (15, 4)];
    make_pos_tests "Rotate Non-I Point 5 CC"
      ((repeat_next 
          ((start_custom [11,2;12,2;13,2;14,2;15,2;11,5;12,5;13,5] [2;2])
           |> rotate_p true |> move_p true) 7) |> rotate_p false)
      [(14, 5); (14, 4); (14, 3); (15, 4)];
  ]

let hold_tests = 
  [
    make_pos_tests "Hold 1"
      ((start_custom [] [0;1]) |> hold) 
      [(19, 3); (19, 4); (19, 5); (19, 6)];
    make_pos_tests "Hold 2"
      ((start_custom [] [0;1]) |> hold |> hold) 
      [(19, 3); (19, 4); (19, 5); (19, 6)];
    make_pos_tests "Hold Lock"
      ((start_custom [] [0;2;1]) |> hold |> hard_drop |> next |> fst) 
      [(19, 3); (19, 4); (19, 5); (19, 6)];
    make_pos_tests "Hold Lock Hold"
      ((start_custom [] [1;0;2]) |> hold |> hard_drop |> next |> fst |> hold) 
      [(19, 3); (19, 4); (19, 5); (19, 6)];
  ]

(** [make_score_tests name st expected_output] constructs an OUnit
    test named [name] that test the quality of [get_score st] with 
    [expected_output]. *)
let make_score_tests
    (name: string)
    (st: t)
    (expected_output : int) : test = 
  name >:: (fun _ -> assert_equal expected_output (st |> get_score) 
               ~printer:string_of_int)

let score_tests =
  [
    make_score_tests "Start 0" (start ()) 0;
    make_score_tests "Clear 1 Line Preset"
      (
        (start_custom 
           [(0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9)] [])
        |> hard_drop |> next |> fst
      ) 100;
    make_score_tests "Clear 1 Line Manual"
      (
        (start_custom 
           [(0,0);(0,1);(0,2);(0,3);(0,4);(0,6);(0,7);(0,8);(0,9)] [1;1])
        |> rotate_p true |> hard_drop |> next |> fst
      ) 100;
    make_score_tests "Clear 2 Lines Preset"
      ((start_custom [
           (0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9);
           (3,0);(3,1);(3,2);(3,3);(3,4);(3,5);(3,6);(3,7);(3,8);(3,9)
         ] []) |> hard_drop |> next |> fst) 300;
    make_score_tests "Clear 3 Lines Preset"
      ((start_custom [
           (0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9);
           (3,0);(3,1);(3,2);(3,3);(3,4);(3,5);(3,6);(3,7);(3,8);(3,9);
           (4,0);(4,1);(4,2);(4,3);(4,4);(4,5);(4,6);(4,7);(4,8);(4,9);
         ] []) |> hard_drop |> next |> fst) 500;
    make_score_tests "Tetris Manual"
      (
        (start_custom [
            (0,0);(0,1);(0,2);(0,3);(0,4);(0,6);(0,7);(0,8);(0,9);
            (1,0);(1,1);(1,2);(1,3);(1,4);(1,6);(1,7);(1,8);(1,9);
            (2,0);(2,1);(2,2);(2,3);(2,4);(2,6);(2,7);(2,8);(2,9);
            (3,0);(3,1);(3,2);(3,3);(3,4);(3,6);(3,7);(3,8);(3,9)
          ] [1;1]) |> rotate_p true |> hard_drop |> next |> fst
      ) 800;
    make_score_tests "T-Spin Double"
      ((start_custom [
          (17,0);(17,1);(17,2);   (18,3);           (17,6);(17,7);(17,8);(17,9);
          (16,0);(16,1);(16,2);(16,3);       (16,5);(16,6);(16,7);(16,8);(16,9)
         ] [2;2]) |> rotate_p true |> hard_drop |> rotate_p true |> next |> fst
      ) 1200;
  ]

(** [make_compute_pos_tests name bst expected_output] 
    constructs an OUnit test named [name] that test the quality of 
    [ai_pos bst] with [expected_output]. *)
let make_compute_pos_tests
    (name: string)
    (bst: t)
    (expected_output : (int * int) list) : test = 
  name >:: (fun _ ->
      assert_equal (List.sort_uniq Pervasives.compare expected_output)
        (bst |> ai_pos |> List.sort_uniq Pervasives.compare)
        ~printer: (fun x -> print_lst "" x)
    )

let ai_tests = [
  make_compute_pos_tests "compute pos 0" 
    (start_custom [(1, 2); (1, 3); (0, 2); (0, 3);] [1; 1])
    [(0, 4); (0, 5); (0, 6); (0, 7);];
  make_compute_pos_tests "compute pos 1" 
    (start_custom [(1, 1); (1, 2); (0, 1); (0, 2);] [0; 1])
    [(1, 3); (1, 4); (0, 3); (0, 4);];
  make_compute_pos_tests "compute pos 2"
    (start_custom [
        (0,0);(0,1);(0,2);(0,3);(0,4);(0,6);(0,7);(0,8);(0,9);
        (1,0);(1,1);(1,2);(1,3);(1,4);(1,6);(1,7);(1,8);(1,9);
        (2,0);(2,1);(2,2);(2,3);(2,4);(2,6);(2,7);(2,8);(2,9);
        (3,0);(3,1);(3,2);(3,3);(3,4);(3,6);(3,7);(3,8);(3,9)] [1;1]) 
    [(0, 5); (1, 5); (2, 5); (3, 5);];
  make_compute_pos_tests "compute pos 3"
    (start_custom 
       [(0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9)] [1;1])
    [(1, 6); (1, 7); (1, 8); (1, 9);];
  make_compute_pos_tests "compute pos 4"
    (start_custom 
       [(0,0);(0,1);(0,2);(0,3);(0,4);(0,6);(0,7);(0,8);(0,9)] [2;1])
    [(0, 5); (1, 4); (1, 5); (1, 6);];
  make_compute_pos_tests "compute pos 5"
    (start_custom 
       [(0,0);(0,1);(0,2);(0,3);(0,4);(0,6);(0,7);(0,8);(0,9)] [3;1])
    [(0, 5); (1, 5); (1, 6); (1, 7);];
  make_compute_pos_tests "compute pos 6"
    (start_custom 
       [(0,0);(0,1);(0,2);(0,3);(0,4);(0,6);(0,7);(0,8);(0,9)] [4;1])
    [(1, 3); (1, 4); (1, 5); (0, 5);];
  make_compute_pos_tests "compute pos 7"
    (start_custom 
       [(0,0);(0,1);(0,2);(0,3);(0,4);(0,6);(0,7);(0,8);(0,9)] [5;1])
    [(1, 4); (2, 4); (0, 5); (1, 5);];
  make_compute_pos_tests "compute pos 8"
    (start_custom 
       [(0,0);(0,1);(0,2);(0,3);(0,4);(0,6);(0,7);(0,8);(0,9)] [6;1])
    [(0, 5); (1, 5); (1, 6); (2, 6);];
  make_compute_pos_tests "compute pos 9"
    (start_custom [
        (0,0);(0,1);(0,2);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9);
        (1,0);(1,1);(1,2);(1,4);(1,5);(1,6);(1,7);(1,8);(1,9);] [3;1]) 
    [(2, 2); (2, 3); (1, 3); (0, 3);];
  make_compute_pos_tests "compute pos 10"
    (start_custom [
        (0,2);(0,3);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9);
        (1,2);(1,3);(1,4);(1,5);(1,6);(1,7);(1,8);(1,9);] [0;1]) 
    [(1, 0); (1, 1); (0, 0); (0, 1);];
  make_compute_pos_tests "compute pos 11"
    (start_custom [
        (0,3);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9);
        (1,3);(1,4);(1,5);(1,6);(1,7);(1,8);(1,9);] [0;1]) 
    [(1, 1); (1, 2); (0, 1); (0, 2);];
  make_compute_pos_tests "compute pos 12"
    (start_custom [
        (0,3);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9);
        (1,3);(1,4);(1,5);(1,6);(1,7);(1,8);(1,9);] [3;1]) 
    [(1, 2); (0, 0); (0, 1); (0, 2);];
  make_compute_pos_tests "compute pos 13"
    (start_custom [
        (0,3);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9);
        (1,3);(1,4);(1,5);(1,6);(1,7);(1,8);(1,9);
        (2,8);(2,9)] [3;1]) 
    [(1, 2); (0, 0); (0, 1); (0, 2);];
  make_compute_pos_tests "compute pos 14"
    (start_custom [
        (0,3);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9);
        (1,3);(1,4);(1,5);(1,7);(1,8);(1,9);
        (2,3);(2,4);(2,5);(2,6);(2,7);(2,8);(2,9);] [3;1]) 
    [(1, 2); (0, 0); (0, 1); (0, 2);];
  make_compute_pos_tests "compute pos 15"
    (start_custom [
        (0,3);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9);
        (1,2);(1,3);(1,4);(1,5);(1,7);(1,8);(1,9);
        (2,3);(2,4);(2,5);(2,6);(2,7);(2,8);(2,9);] [3;1]) 
    [(1, 0); (2, 0); (2, 1); (2, 2);];
]

(** [make_continuous_tests name bst expected_output] 
    constructs an OUnit test named [name] that test the quality of 
    [continuous bst y v] with [expected_output]. *)
let make_continuous_tests
    (name: string)
    (bst: bool array array)
    (expected_output : int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (continuous bst 0 0 0)
        ~printer: string_of_int
    )

(** [make_hole_tests name bst expected_output] 
    constructs an OUnit test named [name] that test the quality of 
    [holes bst y v] with [expected_output]. *)
let make_hole_tests
    (name: string)
    (bst: bool array array)
    (expected_output : int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (holes bst 0 0)
        ~printer: string_of_int
    )

(** [make_clear_tests name bst expected_output] 
    constructs an OUnit test named [name] that test the quality of 
    [num_clears bst y v] with [expected_output]. *)
let make_clear_tests
    (name: string)
    (bst: bool array array)
    (expected_output : int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (num_clears bst 0 0)
        ~printer: string_of_int
    )

(** [make_bump_tests name bst expected_output] 
    constructs an OUnit test named [name] that test the quality of 
    [bumpiness bst y v] with [expected_output]. *)
let make_bump_tests
    (name: string)
    (bst: bool array array)
    (expected_output : int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (bumpiness bst 0 0)
        ~printer: string_of_int
    )

let optimize_tests = [ 
  make_continuous_tests "continuous 1" 
    (Array.of_list([Array.of_list[true;true;false;true];
                    Array.of_list[true;true;true;true]])) 4;
  make_continuous_tests "continuous 2" 
    (Array.of_list([Array.of_list[true;false;false;false];
                    Array.of_list[false;false;false;true]])) 0;
  make_continuous_tests "continuous 3" 
    (Array.of_list([Array.of_list[true;true;true;true];
                    Array.of_list[true;true;true;true]])) 6;
  make_continuous_tests "continuous 4" 
    (Array.of_list([Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false]])) 0;
  make_continuous_tests "continuous 5" 
    (Array.of_list([Array.of_list[true;false;true;false];
                    Array.of_list[true;false;true;true]])) 1;
  make_clear_tests "clear 1"
    (Array.of_list([Array.of_list[true;false;true;false];
                    Array.of_list[true;false;true;true]])) 0;
  make_clear_tests "clear 2"
    (Array.of_list([Array.of_list[true;true;true;true];
                    Array.of_list[true;false;true;true]])) 1;
  make_clear_tests "clear 3"
    (Array.of_list([Array.of_list[true;true;true;true];
                    Array.of_list[true;true;true;true]])) 2;
  make_clear_tests "clear 4"
    (Array.of_list([Array.of_list[false;false;false;false];
                    Array.of_list[true;false;true;true]])) 0;
  make_clear_tests "clear 5"
    (Array.of_list([Array.of_list[true;false;true;false];
                    Array.of_list[true;true;true;true]])) 1;
  make_bump_tests "bump 1"
    (Array.of_list([Array.of_list[true;false;true;false];
                    Array.of_list[true;true;true;true];
                    Array.of_list[true;true;true;true];
                    Array.of_list[true;true;true;true]])) 0;
  make_bump_tests "bump 2"
    (Array.of_list([Array.of_list[true;false;false;false];
                    Array.of_list[true;false;false;false];
                    Array.of_list[true;false;false;false];
                    Array.of_list[true;false;false;false]])) 4;
  make_bump_tests "bump 3"
    (Array.of_list([Array.of_list[true;false;true;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false]])) 3;
  make_bump_tests "bump 4"
    (Array.of_list([Array.of_list[true;true;true;true];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false]])) 0;
  make_bump_tests "bump 5"
    (Array.of_list([Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false]])) 0;
  make_bump_tests "bump 6"
    (Array.of_list([Array.of_list[true;true;true;true];
                    Array.of_list[false;true;false;false];
                    Array.of_list[true;false;false;true];
                    Array.of_list[false;false;true;false]])) 4;
  make_bump_tests "bump 7"
    (Array.of_list([Array.of_list[true;false;true;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[true;false;true;false]])) 12;

  make_hole_tests "hole 1"
    (Array.of_list([Array.of_list[true;false;true;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[true;false;true;false]])) 4;
  make_hole_tests "hole 2"
    (Array.of_list([Array.of_list[true;false;true;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false]])) 0;
  make_hole_tests "hole 3"
    (Array.of_list([Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[true;true;true;true]])) 12;
  make_hole_tests "hole 4"
    (Array.of_list([Array.of_list[false;false;false;false];
                    Array.of_list[true;false;false;true];
                    Array.of_list[false;true;false;false];
                    Array.of_list[true;true;true;true]])) 9;
  make_hole_tests "hole 5"
    (Array.of_list([Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false];
                    Array.of_list[false;false;false;false]])) 0;
  make_hole_tests "hole 6"
    (Array.of_list([Array.of_list[false;false;false;false];
                    Array.of_list[true;true;true;true];
                    Array.of_list[true;true;true;true];
                    Array.of_list[true;true;true;true]])) 4;

]

let suite =
  "test suite for Tetris"  >::: List.flatten [
    death_tests;
    spawn_tests;
    move_laterally_tests;
    move_down_tests;
    hard_drop_tests;
    rotate_tests;
    hold_tests;
    score_tests;
    ai_tests;
    optimize_tests;
  ]

let _ = run_test_tt_main suite