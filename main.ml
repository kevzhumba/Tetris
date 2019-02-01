(*
    ACADEMIC INTEGRITY DISCLOSURE

    Two of the functions in our submission involve adapted code from online
    sources. Per the CS 1110 academic integrity policy, which our syllabus links
    to, "Questions of the form 'How do I do X in Python' are okay. Questions of
    the form 'How do I implement this specific function' are not." The two
    functions in question are used to read input from the terminal and set
    timeouts. Both of these are general OCaml syntax things and not assignment
    specific implementation problems, so we Googled how to do it and cited the
    source in our code. The methods are named "get_next_char" and "timeout", and
    they have citations in a comment above their documentation. Both functions
    have been modified by us from their original forms to better fit our
    needs.
*)

(* 
    ASCII art was generated with the help of the following two websites:
    https://www.text-image.com/convert/ascii.html
    http://patorjk.com/software/taag/
*)

open Unix

(* https://stackoverflow.com/questions/13410159/ *)
let orig_term_state = Unix.tcgetattr Unix.stdin
let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN 
    { orig_term_state with Unix.c_icanon = false; Unix.c_echo = false }

(** [get_next_char ()] is the next character the user types into the console,
    without waiting for a carriage return. This call blocks execution. *)
let get_next_char () = input_char (in_channel_of_descr stdin)

(* https://www.reddit.com/r/ocaml/comments/3qapbv/ *)
exception Timeout
let () = Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout))

(** [timeout f a time default_value] is [(f a, r)] provided that [f a] returns 
    within [time] seconds, where [r] is the number of seconds remaining after
    execution of [f a] starting from time [time]. If [f a] does not complete in
    time, then the result is [(default_value, 0.0)] *)
let timeout f arg time default_value =
  if time = 0.0 then (default_value, 0.0) else
    let itim = ITIMER_REAL in  
    ignore (setitimer itim {it_interval = 0.0; it_value = time});
    try 
      let res = f arg in 
      let currtime = (getitimer itim).it_value in
      ignore(setitimer itim {it_interval = 0.0; it_value = 0.0});
      (res, currtime)
    with exc -> if exc = Timeout then (default_value, 0.0) else raise exc

(** [show_countdown ()] displays a 3 2 1 countdown before resuming execution.
    This method takes exactly 3 seconds to execute and will clear the screen
    both before and after execution. *)
let show_countdown () = 
  ignore(Sys.command "clear");
  print_endline "GET READY";
  print_newline ();
  print_endline "██████╗ ";
  print_endline "╚════██╗";
  print_endline " █████╔╝";
  print_endline " ╚═══██╗";
  print_endline "██████╔╝";
  print_endline "╚═════╝ ";
  Unix.sleep 1;
  ignore(Sys.command "clear");
  print_endline "GET READY";
  print_newline ();
  print_endline "██████╗ ";
  print_endline "╚════██╗";
  print_endline " █████╔╝";
  print_endline "██╔═══╝ ";
  print_endline "███████╗";
  print_endline "╚══════╝";
  Unix.sleep 1;
  ignore(Sys.command "clear");
  print_endline "GET READY";
  print_newline ();
  print_endline"    ██╗";
  print_endline"   ███║";
  print_endline"   ╚██║";
  print_endline"    ██║";
  print_endline"    ██║";
  print_endline"    ╚═╝";
  Unix.sleep 1;
  (* Ignore inputs during countdown *)
  let _ = timeout get_next_char() 0.01 'a' in
  ignore(Sys.command "clear")

(** [find_el l e] is the element in [l] that contains [e], or the empty string
    if not found. *)
let rec find_el lst el =
  match lst with
  | [] -> ""
  | h :: t -> 
    if (List.nth (String.split_on_char ' ' (h)) 1) = el then h else find_el t el 

(** [construct_list score score_names acc] is a sorted list of scores and the 
    corresponding names, from the highest score to the lowest. *)
let rec construct_list scores scores_and_names acc =
  match scores with
  | [] -> acc
  | h :: t -> let el = find_el scores_and_names (string_of_int h) in 
    if el != "" then el :: construct_list t scores_and_names acc 
    else construct_list t scores_and_names acc

(** [score_num] is the sorted list of high scores from the file, 
    ordered from highest to lowest *)
let score_num = 
  let scores = ref [] in
  let channel = open_in "scoreboard.txt" in
  try
    while true; do
      let numbers = 
        List.nth (String.split_on_char ' ' (input_line channel)) 1 in 
      scores := int_of_string (numbers) :: !scores
    done;
    close_in channel;
    !scores;
  with End_of_file ->
    close_in channel;
    List.rev (List.sort compare (!scores))

(** [display_score l] is the sorted list of high scores and names from the file
    [l], ordered from highest to lowest *)
let display_score scores_lst = 
  let scores_with_name = ref [] in 
  let channel = open_in "scoreboard.txt" in
  try
    while true; do
      scores_with_name := input_line channel :: !scores_with_name
    done;
    close_in channel;
    !scores_with_name;
  with End_of_file ->
    close_in channel;
    construct_list scores_lst !scores_with_name []

(** [print_list lst] prints the last five elements of [lst]. *)
let rec print_list lst acc =
  match lst with  
  | [] -> () 
  | h :: t -> if acc > 4 then print_string "" 
    else print_string (h ^ "\n"); 
    print_list t (acc + 1)

(** [list_to_string lst str] is the string [str] converted from [lst]. *)
let rec list_to_string lst str = 
  match lst with
  | [] -> str
  | h :: t -> list_to_string t (h^"\n"^str)

(** [top_scores] is the top five scores on file. *)
let top_scores = 
  let rec top_scores_lst lst acc scores =
    match lst with  
    | [] -> scores
    | h :: t -> if acc > 4 then scores
      else top_scores_lst t (acc + 1) (h :: scores)
  in list_to_string (top_scores_lst (display_score score_num) 0 []) ""

(** [score_to_file score] writes [score] to scoreboard.txt. *)
let score_to_file name score =
  let out = open_out_gen [Open_append] 0 "scoreboard.txt" in
  output_string out (name ^ ": " ^score);
  close_out out

(** [game_over score] ends the game and displays the players score and the 
    highest scores on file. *)
let game_over score = 
  ignore(Sys.command "clear");
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN orig_term_state;
  print_endline "Enter your name: ";
  let name = read_line() in 
  let string_score = (string_of_int score) ^ "\n" in 
  score_to_file name string_score;
  ignore(Sys.command "clear");
  print_endline " ██████╗  █████╗ ███╗   ███╗███████╗";
  print_endline "██╔════╝ ██╔══██╗████╗ ████║██╔════╝";
  print_endline "██║  ███╗███████║██╔████╔██║█████╗  ";
  print_endline "██║   ██║██╔══██║██║╚██╔╝██║██╔══╝  ";
  print_endline "╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗";
  print_endline " ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝";
  print_newline ();
  print_endline " ██████╗ ██╗   ██╗███████╗██████╗ ";
  print_endline "██╔═══██╗██║   ██║██╔════╝██╔══██╗";
  print_endline "██║   ██║██║   ██║█████╗  ██████╔╝";
  print_endline "██║   ██║╚██╗ ██╔╝██╔══╝  ██╔══██╗";
  print_endline "╚██████╔╝ ╚████╔╝ ███████╗██║  ██║";
  print_endline " ╚═════╝   ╚═══╝  ╚══════╝╚═╝  ╚═╝";
  print_newline ();
  print_endline "===============";
  print_endline "YOUR SCORE";
  print_endline (string_of_int score);
  print_endline "===============";
  print_endline "HIGH SCORES";
  print_list (display_score score_num) 0;
  print_newline ()
(* let out = open_out "scoreboard.txt" in output_string out top_scores *)

(** [print_paused ()] prints the word "Paused" on screen, clearing the screen
    before hand. Execution is then blocked until a key is pressed. *)
let print_paused () = 
  ignore(Sys.command "clear");
  print_endline "██████╗  █████╗ ██╗   ██╗███████╗███████╗██████╗ ";
  print_endline "██╔══██╗██╔══██╗██║   ██║██╔════╝██╔════╝██╔══██╗";
  print_endline "██████╔╝███████║██║   ██║███████╗█████╗  ██║  ██║";
  print_endline "██╔═══╝ ██╔══██║██║   ██║╚════██║██╔══╝  ██║  ██║";
  print_endline "██║     ██║  ██║╚██████╔╝███████║███████╗██████╔╝";
  print_endline "╚═╝     ╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚══════╝╚═════╝ ";
  print_newline ();
  print_endline "          Press any key to continue";
  ignore(get_next_char ())

(** [game_loop st t ot] is [()], but also calls itself with the new state of
    the game after whatever relevant operation is performed, based on keyboard
    input. [t] dictates how long to wait before performing the default option,
    and [ot] dictates what the timer should be reset to after the default
    option is performed. *)
let rec game_loop st time =
  ignore (Sys.command "clear");
  State.print_state st;
  let input_char_tuple = timeout get_next_char() time '|' in
  let comp_orig_time st = 
    let level = State.get_level st in
    let level_dif = 
      if level > 15 then float_of_int 14 else float_of_int (level - 1) in
    (0.8 -. (level_dif *. 0.007)) ** level_dif
  in
  let start_time = Unix.gettimeofday () in
  let shrink_time ot = 
    let new_t = (ot +. start_time -. (Unix.gettimeofday ())) in 
    if new_t < 0. then 0.0 else new_t in
  let next_move new_state new_time = 
    game_loop new_state (shrink_time new_time)
  in
  if State.is_alive st then 
    match input_char_tuple with 
    (* PLAYER COMMANDS *)
    | ('j', t) -> (* MOVE PIECE LEFT *)
      next_move (State.move st true) t
    | ('k', t) -> (* MOVE PIECE RIGHT *)
      next_move (State.move st false) t
    | ('l', t) -> (* ROTATE PIECE CLOCKWISE *)
      next_move (State.rotate st true) t
    | ('o', t) -> (* ROTATE PIECE COUNTERCLOCKWISE *)
      next_move (State.rotate st false) t
    | (';', _) -> (* HARD DROP PIECE *)
      let new_st = State.hard_drop st |> State.next |> fst in
      next_move new_st (new_st |> comp_orig_time)
    | ('h', _) -> (* HOLD PIECE *)
      let new_st = State.hold st in
      next_move new_st (new_st |> comp_orig_time)
    | ('p', t) -> (* PAUSE GAME *)
      print_paused ();
      (* RESUME GAME *)
      show_countdown ();
      game_loop st t
    | ('q', _) -> (* QUIT GAME *) 
      game_over (State.get_score st)
    | ('i', _) 
    (* OTHER POSSIBILITIES *)
    | (_, 0.0) -> (* GAME TICK *)
      let new_st = st |> State.next |> fst in
      next_move new_st (new_st |> comp_orig_time)
    | (_, t) -> (* UNRECOGNIZED *)
      next_move st t
  else game_over (State.get_score st)

type winners = Player1Win | Player2Win | NoWin | AIWin

(** [game_over_multi winner] ends the game and displays the winning 
    player. *)
let game_over_multi winner = 
  let print_wins () = 
    print_newline ();
    print_endline "           ██╗    ██╗██╗███╗   ██╗███████╗";
    print_endline "           ██║    ██║██║████╗  ██║██╔════╝";
    print_endline "           ██║ █╗ ██║██║██╔██╗ ██║███████╗";
    print_endline "           ██║███╗██║██║██║╚██╗██║╚════██║";
    print_endline "           ╚███╔███╔╝██║██║ ╚████║███████║";
    print_endline "            ╚══╝╚══╝ ╚═╝╚═╝  ╚═══╝╚══════╝" in
  ignore(Sys.command "clear");
  (match winner with
   | Player1Win -> begin
      print_endline "██████╗ ██╗      █████╗ ██╗   ██╗███████╗██████╗      ██╗";
      print_endline "██╔══██╗██║     ██╔══██╗╚██╗ ██╔╝██╔════╝██╔══██╗    ███║";
      print_endline "██████╔╝██║     ███████║ ╚████╔╝ █████╗  ██████╔╝    ╚██║";
      print_endline "██╔═══╝ ██║     ██╔══██║  ╚██╔╝  ██╔══╝  ██╔══██╗     ██║";
      print_endline "██║     ███████╗██║  ██║   ██║   ███████╗██║  ██║     ██║";
      print_endline "╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝     ╚═╝";
      print_wins ()
     end
   | Player2Win -> begin
       print_endline (
         "██████╗ ██╗      █████╗ ██╗   ██╗███████╗██████╗     ██████╗ \n" ^
         "██╔══██╗██║     ██╔══██╗╚██╗ ██╔╝██╔════╝██╔══██╗    ╚════██╗\n" ^
         "██████╔╝██║     ███████║ ╚████╔╝ █████╗  ██████╔╝     █████╔╝\n" ^
         "██╔═══╝ ██║     ██╔══██║  ╚██╔╝  ██╔══╝  ██╔══██╗    ██╔═══╝ \n" ^
         "██║     ███████╗██║  ██║   ██║   ███████╗██║  ██║    ███████╗\n" ^
         "╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝    ╚══════╝");
       print_wins ()
     end
   | NoWin -> begin
       print_endline "\t\t████████╗██╗███████╗";
       print_endline "\t\t╚══██╔══╝██║██╔════╝";
       print_endline "\t\t   ██║   ██║█████╗  ";
       print_endline "\t\t   ██║   ██║██╔══╝  ";
       print_endline "\t\t   ██║   ██║███████╗";
       print_endline "\t\t   ╚═╝   ╚═╝╚══════╝"
     end
   | AIWin -> print_endline (
    " ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗   ██╗████████╗███████╗██████╗ \n" ^
    "██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║   ██║╚══██╔══╝██╔════╝██╔══██╗\n" ^
    "██║     ██║   ██║██╔████╔██║██████╔╝██║   ██║   ██║   █████╗  ██████╔╝\n" ^
    "██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║   ██║   ██║   ██╔══╝  ██╔══██╗\n" ^
    "╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ╚██████╔╝   ██║   ███████╗██║  ██║\n" ^
    " ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝      ╚═════╝    ╚═╝   ╚══════╝╚═╝  ╚═╝");
     print_wins ()
  );
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN orig_term_state

(** [game_loop_multi st1 st2 t1 ot1 t2 ot2] is [()], but also calls itself with 
    the new state of each game after whatever relevant operation is performed, 
    based on keyboard input. Facilitates two games at once for multiplayer.
    [t1] and [t2] dictate how long to wait before 
    performing the default option, and [ot1] and [ot2] dictate what the timer 
    should be reset to after the default option is performed for each game,
    respectively. *)
let rec game_loop_multi st1 st2 time1 orig_time1 time2 orig_time2 =
  ignore(Sys.command "clear");
  State.print_state_multi st1 st2;
  let is_p1_next = time1 < time2 in
  let input_char_tuple = 
    timeout get_next_char() 
      (if is_p1_next then time1 else time2) 
      (if time1 = time2 then '~' else if is_p1_next then '|' else '`') in
  let start_time = Unix.gettimeofday () in
  let shrink_time ot ct = 
    let new_t = (ot +. start_time -. ct) in 
    if new_t < 0. then 0.0 else new_t in
  let next_move ns1 ns2 new_time to_reset1 to_reset2 = 
    let new_time1, new_time2 = 
      if is_p1_next then new_time, (time2 +. new_time -. time1) else
        (time1 +. new_time -. time2), new_time in
    let new_curr_time = Unix.gettimeofday () in
    let new_loop_time1 = 
      if to_reset1 then orig_time1 else (shrink_time new_time1 new_curr_time) in
    let new_loop_time2 = 
      if to_reset2 then orig_time2 else (shrink_time new_time2 new_curr_time) in
    game_loop_multi 
      ns1 ns2 new_loop_time1 orig_time1 new_loop_time2 orig_time2
  in
  if State.is_alive st1 && State.is_alive st2 then 
    match input_char_tuple with 
    (* INVARIANT: t <= time1 and t <= time2 *)
    (* PLAYER 1 COMMANDS *)
    | ('a', t) -> (* MOVE PIECE LEFT *)
      next_move (State.move st1 true) st2 t false false
    | ('s', t) -> (* MOVE PIECE RIGHT *)
      next_move (State.move st1 false) st2 t false false
    | ('d', t) -> (* ROTATE PIECE CLOCKWISE *)
      next_move (State.rotate st1 true) st2 t false false
    | ('e', t) -> (* ROTATE PIECE COUNTERCLOCKWISE *)
      next_move (State.rotate st1 false) st2 t false false
    | ('f', t) -> (* HARD DROP PIECE *)
      let new_st1, num_lines = (st1 |> State.hard_drop |> State.next) in
      next_move new_st1 (st2 |> State.add_junk num_lines) t true false
    | ('q', t) -> (* HOLD PIECE *)
      next_move (State.hold st1) st2 t true false
    | ('w', t) -> (* SOFT DROP *)
      let new_st1, num_lines = (st1 |> State.next) in
      next_move new_st1 (st2 |> State.add_junk num_lines) t true false
    (* PLAYER 2 COMMANDS *)
    | ('j', t) -> (* MOVE PIECE LEFT *)
      next_move st1 (State.move st2 true) t false false
    | ('k', t) -> (* MOVE PIECE RIGHT *)
      next_move st1 (State.move st2 false) t false false
    | ('l', t) -> (* ROTATE PIECE CLOCKWISE *)
      next_move st1 (State.rotate st2 true) t false false
    | ('o', t) -> (* ROTATE PIECE COUNTERCLOCKWISE *)
      next_move st1 (State.rotate st2 false) t false false
    | (';', t) -> (* HARD DROP PIECE *)
      let new_st2, num_lines = (st2 |> State.hard_drop |> State.next) in
      next_move (st1 |> State.add_junk num_lines) new_st2 t true false
    | ('h', t) -> (* HOLD PIECE *)
      next_move st1 (State.hold st2) t true false
    | ('i', t) -> (* SOFT DROP *)
      let new_st2, num_lines = (st2 |> State.next) in
      next_move (st1 |> State.add_junk num_lines) new_st2 t true false
    | ('p', t) -> (* PAUSE GAME *)
      print_paused ();
      (* RESUME GAME *)
      show_countdown ();
      next_move st1 st2 t false false
    | ('0', _) -> (* QUIT GAME *) 
      game_over_multi NoWin
    (* OTHER POSSIBILITIES *)
    | ('|', 0.0) -> (* GAME TICK 1 *)
      let new_st1, num_lines = (st1 |> State.next) in
      next_move new_st1 (st2 |> State.add_junk num_lines) 0. true false
    | ('`', 0.0) -> (* GAME TICK 2 *)
      let new_st2, num_lines = (st2 |> State.next) in
      next_move (st1 |> State.add_junk num_lines) new_st2 0. false true
    | ('~', 0.0) -> (* GAME TICK BOTH *)
      let new_st1, num_lines1 = (st1 |> State.next) in
      let new_st2, num_lines2 = (st2 |> State.next) in
      next_move 
        (new_st1 |> State.add_junk num_lines2) 
        (new_st2 |> State.add_junk num_lines1) 0. true true
    | (_, t) -> (* UNRECOGNIZED *)
      next_move st1 st2 t false false
  else game_over_multi (
      if State.is_alive st1 then Player1Win else 
      if State.is_alive st2 then Player2Win else 
        NoWin)

(** [game_loop_ai st1 st2 t1 ot1 t2 ot2] is [()], but also calls itself with the 
    new state of the game after whatever relevant operation is performed, based 
    on keyboard input. Has the player playing against an AI. 
    [t1] and [t2] dictate how long to wait before performing 
    the default option, and [ot1] and [ot2] dictate what the timer should be 
    reset to after the default option is performed. *)
let rec game_loop_ai st1 st2 time1 orig_time1 time2 orig_time2 =
  ignore(Sys.command "clear");
  State.print_state_multi st1 st2;
  let is_p1_next = time1 < time2 in
  let input_char_tuple = 
    timeout get_next_char() 
      (if is_p1_next then time1 else time2) 
      (if time1 = time2 then '~' else if is_p1_next then '|' else '`') in
  let start_time = Unix.gettimeofday () in
  let shrink_time ot ct = 
    let new_t = (ot +. start_time -. ct) in 
    if new_t < 0. then 0.0 else new_t in
  let next_move ns1 ns2 new_time to_reset1 to_reset2 = 
    let new_time1, new_time2 = 
      if is_p1_next then new_time, (time2 +. new_time -. time1) else
        (time1 +. new_time -. time2), new_time in
    let new_curr_time = Unix.gettimeofday () in
    let new_loop_time1 = 
      if to_reset1 then orig_time1 else (shrink_time new_time1 new_curr_time) in
    let new_loop_time2 = 
      if to_reset2 then orig_time2 else (shrink_time new_time2 new_curr_time) in
    game_loop_ai 
      ns1 ns2 new_loop_time1 orig_time1 new_loop_time2 orig_time2
  in
  if State.is_alive st1 && State.is_alive st2 then 
    match input_char_tuple with 
    (* INVARIANT: t <= time1 and t <= time2 *)
    (* PLAYER 1 COMMANDS *)
    | ('j', t) -> (* MOVE PIECE LEFT *)
      next_move (State.move st1 true) st2 t false false
    | ('k', t) -> (* MOVE PIECE RIGHT *)
      next_move (State.move st1 false) st2 t false false
    | ('l', t) -> (* ROTATE PIECE CLOCKWISE *)
      next_move (State.rotate st1 true) st2 t false false
    | ('o', t) -> (* ROTATE PIECE COUNTERCLOCKWISE *)
      next_move (State.rotate st1 false) st2 t false false
    | (';', t) -> (* HARD DROP PIECE *)
      let new_st1, num_lines = (st1 |> State.hard_drop |> State.next) in
      next_move new_st1 (st2 |> State.add_junk num_lines) t true false
    | ('h', t) -> (* HOLD PIECE *)
      next_move (State.hold st1) st2 t true false
    | ('p', t) -> (* PAUSE GAME *)
      print_paused ();
      (* RESUME GAME *)
      show_countdown ();
      next_move st1 st2 t false false
    | ('q', _) -> (* QUIT GAME *) 
      game_over_multi NoWin
    | ('i', t) -> (* SOFT DROP *)
      let new_st1, num_lines = (st1 |> State.next) in
      next_move new_st1 (st2 |> State.add_junk num_lines) t true false
    (* OTHER POSSIBILITIES *)
    | ('|', 0.0) -> (* GAME TICK 1 *)
      let new_st1, num_lines = (st1 |> State.next) in
      next_move new_st1 (st2 |> State.add_junk num_lines) 0. true false
    | ('`', 0.0) -> (* GAME TICK 2 *)
      let new_st2, num_lines = (st2 |> State.compute_next_ai_move) in
      next_move (st1 |> State.add_junk num_lines) new_st2 0. false true
    | ('~', 0.0) -> (* GAME TICK BOTH *)
      let new_st1, num_lines1 = (st1 |> State.next) in
      let new_st2, num_lines2 = (st2 |> State.compute_next_ai_move) in
      next_move 
        (new_st1 |> State.add_junk num_lines2) 
        (new_st2 |> State.add_junk num_lines1) 0. true true
    | (_, t) -> (* UNRECOGNIZED *)
      next_move st1 st2 t false false
  else game_over_multi (
      if State.is_alive st1 then Player1Win else 
      if State.is_alive st2 then AIWin else 
        NoWin)

(** type [game_sel] represents which type of game was selected *)
type game_sel = SingleP | MultiP | MultiAI

(** [setup_game ()] is [()], but also begins the game loop. *)
let setup_game () = 
  let out = open_out "scoreboard.txt" in 
  output_string out top_scores;
  close_out out;
  ignore(Sys.command "clear");
  print_endline "                CS 3110 Presents                 ";
  print_newline ();
  print_endline " ██████████████████████████████████████████████  ";
  print_endline "███       █     ██       ██     ████  █      ███ ";
  print_endline "█████  ████  ████████  ████  █ █████  ██  ██████ ";
  print_endline "█████  ████    ██████  ████     ████  ███  █████ ";
  print_endline "█████  ████  ████████  ████  ██  ███  ████  ████ ";
  print_endline "█████  ████  ████████  ████  ███  ██  █████  ███ ";
  print_endline "█████  ████       ███  ████  ████  █  █      ███ ";
  print_endline "████████████████████████████████████████████████ ";
  print_endline " ██████████████████████████████████████████████  ";
  print_endline "               ██████████████████                ";
  print_endline "               ██████████████████                ";
  print_endline "               ██████████████████                ";
  print_endline "               ██████████████████                ";
  print_endline "               ██████████████████                ";
  print_endline "               ██████████████████                ";
  print_endline "               ██████████████████                ";
  print_newline ();
  print_newline ();
  print_string "Make sure your window is large enough to see the ";
  print_endline "words \"CS 3110 Presents\" at the top.";
  print_string "Adjust the coloring so you can ";
  print_endline "see the following seven numbers:";
  ANSITerminal.(print_string [yellow] "one ");
  ANSITerminal.(print_string [cyan] "two ");
  ANSITerminal.(print_string [green] "three ");
  ANSITerminal.(print_string [red] "four ");
  ANSITerminal.(print_string [white] "five ");
  ANSITerminal.(print_string [magenta] "six ");
  ANSITerminal.(print_string [blue] "seven ");
  print_endline "◄ You should see one through seven here";      
  print_newline ();
  print_endline "Press any key to continue.";
  let _ = get_next_char () in
  (* CHOOSE YOUR GAME *)
  ignore(Sys.command "clear");
  print_newline ();
  print_endline " ██████╗██╗  ██╗ ██████╗  ██████╗ ███████╗███████╗";
  print_endline "██╔════╝██║  ██║██╔═══██╗██╔═══██╗██╔════╝██╔════╝";
  print_endline "██║     ███████║██║   ██║██║   ██║███████╗█████╗  ";
  print_endline "██║     ██╔══██║██║   ██║██║   ██║╚════██║██╔══╝  ";
  print_endline "╚██████╗██║  ██║╚██████╔╝╚██████╔╝███████║███████╗";
  print_endline " ╚═════╝╚═╝  ╚═╝ ╚═════╝  ╚═════╝ ╚══════╝╚══════╝";
  print_newline ();
  print_newline ();
  print_endline "\t\tSingle Player Marathon";
  print_endline "\t\tPress L";
  print_newline ();
  print_newline ();
  print_endline "\t\tMultiplayer Versus";
  print_endline "\t\tPress A";
  print_newline ();
  print_newline ();
  print_endline "\t\tSingle Player Versus AI";
  print_endline "\t\tPress H";
  let rec get_choice () = 
    match get_next_char () with
    | 'l' -> SingleP
    | 'a' -> MultiP
    | 'h' -> MultiAI 
    | _ -> (get_choice ()) 
  in
  match get_choice () with
  | SingleP ->
    ignore (Sys.command "clear");
    print_string "Controls:\tJ / K - Left / Right\tL - Rotate Clockwise";
    print_endline "\t; - Hard Drop";
    print_endline "     \t\tI - Soft Drop 1 Line\tO - Rotate Counterclockwise";
    print_endline "     \t\tH - Hold\tP - Pause / Resume\tQ - Quit";
    print_endline "Press Any Key to Start";
    let _ = get_next_char () in
    show_countdown ();
    let start_state = State.start () in
    ignore(Sys.command "clear");
    (*State.print_state start_state; *)
    game_loop start_state 1.0
  | MultiP -> 
    ignore (Sys.command "clear");
    print_endline "\tPLAYER 1";
    print_endline "\t========";
    print_string "Controls:\tA / S - Left / Right\tD - Rotate Clockwise";
    print_endline "\tF - Hard Drop";
    print_string "     \t\tW - Soft Drop 1 Line\tE - Rotate Counterclockwise";
    print_endline "\tQ - Hold";
    print_endline "\tPLAYER 2";
    print_endline "\t========";
    print_string "Controls:\tJ / K - Left / Right\tL - Rotate Clockwise";
    print_endline "\t; - Hard Drop";
    print_string "     \t\tI - Soft Drop 1 Line\tO - Rotate Counterclockwise";
    print_endline "\tH - Hold";
    print_newline ();
    print_endline "     \t\t\tP - Pause / Resume\t0 - Quit";
    print_endline "Press Any Key to Start";
    let _ = get_next_char () in
    show_countdown ();
    let start_state1 = State.start () in
    let start_state2 = State.start () in
    game_loop_multi start_state1 start_state2 1.0 1.0 1.0 1.0
  | MultiAI ->
    ignore (Sys.command "clear");
    print_string "Controls:\tJ / K - Left / Right\tL - Rotate Clockwise";
    print_endline "\t; - Hard Drop";
    print_endline "     \t\tI - Soft Drop 1 Line\tO - Rotate Counterclockwise";
    print_endline "     \t\tH - Hold\tP - Pause / Resume\tQ - Quit";
    print_endline "Press Any Key to Start";
    let _ = get_next_char () in
    show_countdown ();
    let start_state1 = State.start () in
    let start_state2 = State.start () in
    game_loop_ai start_state1 start_state2 1.0 1.0 1.0 1.0
let () = setup_game ()