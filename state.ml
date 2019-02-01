(** type [piece] represents the types of seven possible tetriminos. *)
type piece = PieceT | PieceO | PieceI | PieceS | PieceZ | PieceJ | PieceL

(** List of all pieces *)
let piece_list = [PieceT; PieceO ; PieceI ; PieceS ; PieceZ ; PieceJ ; PieceL]

(** type [space] represents the state of a block on the board. *)
type space = Empty | Mino of piece

(** type [orientation] represents the orientation of a tetrimino on the 
    board. *)
type orientation = North | South | East | West

type t = {
  game_board: space array array; 
  is_alive: bool; 
  curr_piece: (int * int) list;
  curr_piece_type: piece;
  curr_ghost: (int * int) list;
  piece_spawn: piece list;
  hold_piece: piece option;
  current_score: int;
  lines_cleared: int;
  can_hold: bool;
  next_piece: piece;
  curr_orient: orientation;
  level: int;
  is_back_to_back: bool;
  junk_lines: int;
  text_to_disp: string;
  text_start_time: float;
}

let get_score st = st.current_score

let get_level st = (st.lines_cleared / 10) + 1

(** [loc_no_overlap st lst] is [true] iff every coordinate in [lst] does not
    overlap with a locked down piece in [st]. *)
let loc_no_overlap st lst = 
  List.for_all (fun (a, b) -> st.game_board.(a).(b) = Empty) lst 

(** [is_valid_loc st lst] is [true] iff every coordinate is [lst] is a valid
    location on the gameboard and [loc_no_overlap st lst] is [true]. *)
let is_valid_loc st lst = 
  List.for_all (fun x -> snd x >= 0) lst &&
  List.for_all (fun x -> fst x >= 0) lst &&
  List.for_all (fun x -> snd x < 10) lst &&
  List.for_all (fun x -> fst x < (Array.length st.game_board)) lst &&
  loc_no_overlap st lst

(** [calc_ghost st] is [st] with an updated calculation for where the ghost
    piece should render.
    Requires: [st.curr_piece] is non-empty *)
let calc_ghost st = 
  let rec ghost_step ls = 
    let test_cord = List.rev_map (fun (a, b) -> (a - 1, b)) ls in
    if is_valid_loc st test_cord then ghost_step test_cord else
      ls
  in
  {st with curr_ghost = ghost_step st.curr_piece}

(** type [dir] represents the direction of keyboard input. *)
type dir = Left | Down | Right

(** [move_dir dir st] is a tuple of the state after [curr_piece] is moved 
    in [st] in the direction specified by [dir] and [true]. Otherwise, it is 
    ([st], [false]). *)
let move_dir dir st = 
  let gen_new tup = match dir with
    | Right -> (fst tup), (snd tup + 1)
    | Down -> (fst tup - 1), (snd tup)
    | Left -> (fst tup), (snd tup - 1) in
  let new_list = List.map gen_new st.curr_piece in
  if is_valid_loc st new_list && dir <> Down then 
    (* A successful move operation laterally requires new ghost_piece calc *)
    (calc_ghost {st with curr_piece = new_list}, true) else
  if is_valid_loc st new_list then
    (* Moving down does not change the ghost piece *)
    ({st with curr_piece = new_list}, true) else 
    (st, false)

let move st is_left = fst (move_dir (if is_left then Left else Right) st)

(** [fill arr lines hole acc] is the list with length [lines+1] filled of 
    white space with an empty space in [hole] *)
let rec fill lines hole acc = 
  if lines < 0 then acc else if lines <> hole 
  then fill (lines-1) hole ((Mino (PieceL)):: acc) 
  else fill (lines-1) hole (Empty::acc)

(** [fill_rows lines acc lst] is the matrix of junk lines of size lines x 10, 
    lines must be less than or equal to 8 *)
let rec fill_rows lines acc lst = 
  if lines = 0 then acc else 
    fill_rows (lines-1) (acc@[(Array.of_list lst)]) lst

(** [new_list lst lines] is the complete list of junk lines of length [lines]*)
let rec new_list lst lines =  
  if lines = 0 then lst else if lines <= 8 then 
    new_list (fill_rows lines lst (fill 9 (Random.int 10) [])) 0 else
    new_list (fill_rows lines lst (fill 9 (Random.int 10) [])) (lines-8)

(** [fill_junk st] is the state with junk lines *) 
let rec fill_junk st = 
  let junk_array = Array.of_list (new_list [] st.junk_lines) in 
  let temp_array = Array.append junk_array st.game_board in 
  {st with game_board = (Array.sub temp_array 0 40); junk_lines = 0}

(** [shuffle lst] is a randomly shuffled list of [lst]. *)
let shuffle (lst : 'a list) : 'a list =
  QCheck.Gen.(generate1 (shuffle_l lst))

(** [piece_coord p] is the spawn coordinates of the piece p*)
let piece_coord = function
  | PieceO ->
    [(21, 4); (21, 5); (20, 4); (20, 5)]
  | PieceI ->
    [(20, 3); (20, 4); (20, 5); (20, 6)]
  | PieceS -> 
    [(21, 4); (21, 5); (20, 3); (20, 4)]
  | PieceZ -> 
    [(21, 3); (21, 4); (20, 4); (20, 5)]
  | PieceL -> 
    [(21, 5); (20, 3); (20, 4); (20, 5)]
  | PieceT -> 
    [(21, 4); (20, 3); (20, 4); (20, 5)]
  | PieceJ -> 
    [(21, 3); (20, 3); (20, 4); (20, 5)]

(** [spawn st p] is [st] with [is_alive = false] if [p] cannot fit in 
    the location to which it will be added. Otherwise, it is [st] with 
    [curr_piece] equal to the coordinates of [p] when [p] is added to 
    the board. *)
let spawn st = 
  let spawn_helper lst = 
    let new_spawn = shuffle piece_list in 
    if loc_no_overlap st lst then begin
      match st.piece_spawn with
      | [] -> 
        { st with 
          curr_piece = lst; 
          curr_piece_type = List.hd new_spawn; 
          piece_spawn= new_spawn;
          next_piece = List.hd new_spawn; 
          curr_orient = North 
        }
      | a :: [] -> 
        { st with 
          curr_piece = lst; 
          curr_piece_type = List.hd st.piece_spawn; 
          piece_spawn= new_spawn; 
          next_piece = List.hd new_spawn; 
          curr_orient = North 
        }
      | h :: t ->
        { st with 
          curr_piece = lst; 
          curr_piece_type = List.hd st.piece_spawn; 
          piece_spawn= t;
          next_piece = List.hd t; 
          curr_orient = North
        } end |> move_dir Down |> fst |> calc_ghost
    else { st with is_alive = false } (* BLOCK OUT loss condition *)
  in
  piece_coord (List.hd st.piece_spawn) |> spawn_helper

(** [get_piece_corordinate pi] is a list of coordinates for [pi] 
    in side boards. *)
let get_piece_coordinate = function
  | PieceO -> [(1, 1); (1, 2); (2, 1); (2, 2)]
  | PieceI -> [(2, 0); (2, 1); (2, 2); (2, 3)]
  | PieceS -> [(1, 2); (1, 3); (2, 1); (2, 2)]
  | PieceZ -> [(1, 1); (1, 2); (2, 2); (2, 3)]
  | PieceL -> [(1, 3); (2, 1); (2, 2); (2, 3)]
  | PieceT -> [(1, 2); (2, 1); (2, 2); (2, 3)]
  | PieceJ -> [(1, 1); (2, 1); (2, 2); (2, 3)]

(** [show_col_car pce] prints the piece [pce] onto the board *)
let show_col_car = function
  | PieceO -> ANSITerminal.(print_string [yellow] "█")
  | PieceI -> ANSITerminal.(print_string [cyan] "█")
  | PieceS -> ANSITerminal.(print_string [green] "█")
  | PieceZ -> ANSITerminal.(print_string [red] "█")
  | PieceL -> ANSITerminal.(print_string [white] "█")
  | PieceT -> ANSITerminal.(print_string [magenta] "█")
  | PieceJ -> ANSITerminal.(print_string [blue] "█")

(** [show_char st line char2] prints the line [line] of the gameboard of state 
    [st]*) 
let rec show_char st line char2 = 
  let char = 9 - char2 in
  (match st.game_board.(line).(char) with
   | Empty -> begin
       if List.mem (line, char) st.curr_piece then 
         (show_col_car st.curr_piece_type) else
       if List.mem (line, char) st.curr_ghost then
         print_char '.' else
         print_char ' '
     end
   | Mino c -> show_col_car c);
  print_char '|';
  if char2 = 0 then () else show_char st line (char2 - 1)

(** [show_next_char st line char] prints the hold and next pieces of state [st] 
    in the hold and next boxes*)
let rec show_next_char st line char1 = 
  let char = 3 - char1 in
  let co_hold_lst = match st.hold_piece with
    | None -> []
    | Some h -> get_piece_coordinate h in 
  (let co_lst = get_piece_coordinate st.next_piece in 
   if List.mem (18 - line, char) co_lst then
     (show_col_car st.next_piece) else 
   if List.mem (12 - line, char) co_hold_lst then 
     match st.hold_piece with 
     | None ->  print_char ' '
     | Some h -> show_col_car h
   else print_char ' ');
  print_char '|';
  if char1 = 0 then () else show_next_char st line (char1 - 1)

(** [show_next_line st line] prints the next header for the next box onto the 
    terminal*)
let rec show_next_line st line = 
  match line with
  | 19 -> print_string "    Next: "
  | _ ->  begin 
      print_string "    |";
      show_next_char st line 3;
    end

(** [show_next_line st line] prints the hold header for the hold box onto the 
    terminal*)
let rec show_hold_line st line = 
  match line with
  | 13 -> print_string "    Hold: "
  | _ ->  begin 
      print_string "    |";
      show_next_char st line 3;
    end

let print_state st = 
  let rec show_line st line = 
    print_char '|';
    show_char st line 9;
    if line > 14 then show_next_line st line else 
    if line > 8 && line < 14 then show_hold_line st line;
    print_newline ();
    if line = 0 then () else show_line st (line - 1)
  in
  let score_string = "SCORE: " ^ string_of_int (st.current_score) in
  print_endline score_string;
  print_endline "|-------------------|";
  show_line (st) 19;
  print_endline "|-------------------|";
  if (Unix.gettimeofday ()) < 2.0 +. st.text_start_time then
    print_endline st.text_to_disp else
    ()

let print_state_multi st1 st2 = 
  let rec show_line st1 st2 line = 
    print_char '|';
    show_char st1 line 9;
    if line = 19 then (show_next_line st1 line; 
                       print_string "              |"; 
                       show_char st2 line 9;
                       show_next_line st2 line) else
    if line > 14 then 
      (show_next_line st1 line; 
       print_string "           |"; 
       show_char st2 line 9;
       show_next_line st2 line) else 
    if line = 13 then (show_hold_line st1 line;
                       print_string "              |";
                       show_char st2 line 9;
                       show_hold_line st2 line) else 
    if line > 8 && line < 13 then 
      (show_hold_line st1 line;
       print_string "           |";
       show_char st2 line 9;
       show_hold_line st2 line) else
    if line >=0 then (print_string "                        |";
                      show_char st2 line 9);
    print_newline ();
    if line = 0 then () else show_line st1 st2 (line - 1)
  in
  let score_string1 = "SCORE: " ^ string_of_int (st1.current_score) in
  let score_string2 = "SCORE: " ^ string_of_int (st2.current_score) in
  print_endline 
    (score_string1 ^ "                                     " ^ score_string2);
  print_endline 
    ("|-------------------|                        |-------------------|");
  show_line st1 st2 19;
  print_endline 
    ("|-------------------|                        |-------------------|");
  let junk_string1 = 
    "INCOMING JUNK: " ^ string_of_int (st1.junk_lines) ^ " Line(s)" in
  let junk_string2 = 
    "INCOMING JUNK: " ^ string_of_int (st2.junk_lines) ^ " Line(s)" in
  print_endline 
    (junk_string1 ^ "\t\t\t" ^ junk_string2);
  let rec pad_space num acc = 
    if num = 0 then acc else pad_space (num - 1) (acc ^ " ") in
  let first_text = 
    if (Unix.gettimeofday ()) < 2.0 +. st1.text_start_time then
      "\t" ^ st1.text_to_disp ^ 
      (pad_space (20 - (String.length st1.text_to_disp)) "") else
      "\t" in
  if (Unix.gettimeofday ()) < 2.0 +. st2.text_start_time then
    print_endline (first_text ^ "\t\t\t" ^ st2.text_to_disp) else
    ()

(** [match_piece t] generates a new [piece] with id [t]. *)
let match_piece = function
  | 0 -> PieceO 
  | 1 -> PieceI 
  | 2 -> PieceT 
  | 3 -> PieceL 
  | 4 -> PieceJ 
  | 5 -> PieceS 
  | _ -> PieceZ

(** [generate_piece ()] randomly generates a new [piece]. *)
let generate_piece () = 
  match_piece (Random.int 7)

let start () = 
  let configure_length length =
    Unix.time () |> int_of_float |> Random.init;
    let start_piece_spawn = shuffle piece_list in 
    let origin_st = {
      game_board = Array.init length (fun _ -> Array.init 10 (fun _ -> Empty)); 
      is_alive =  true; 
      curr_piece = [];
      curr_piece_type = List.hd start_piece_spawn;
      curr_ghost = [];
      piece_spawn = start_piece_spawn;
      hold_piece = None;
      current_score = 0;
      lines_cleared = 0;
      can_hold = true;
      next_piece = List.hd start_piece_spawn;
      curr_orient = North;
      level = 1;
      is_back_to_back = false;
      junk_lines = 0;
      text_to_disp = "";
      text_start_time = 0.0;
    } in
    (spawn origin_st) |> calc_ghost in
  configure_length 40

let start_custom lst bucket = 
  let st = start () in 
  let rec add_cord = function
    | [] -> st
    | h :: t -> (st.game_board.(fst h)).(snd h) <- Mino PieceI; add_cord t
  in
  if is_valid_loc st lst then 
    let new_st = add_cord lst in
    let new_bucket = List.map match_piece bucket in
    match new_bucket with
    | h :: h2 :: t ->
      { new_st with 
        curr_piece_type = h;
        curr_piece = piece_coord h;
        next_piece = h2;
        piece_spawn = h2 :: t
      } |> move_dir Down |> fst |> calc_ghost
    | _ -> new_st
  else 
    st

let get_curr_piece st = st.curr_piece

(** [deep_copy a] returns a 2nd level deep copy of [a]. That is, it returns a
    new array filled with copies of the elements of [a], provided each element
    of [a] is an array itself. *)
let deep_copy arr = 
  let rec dc_help acc row = 
    if row = 0 then acc else
      dc_help ((Array.copy arr.(row - 1)) :: acc) (row - 1)
  in
  arr |> Array.length |> dc_help [] |> Array.of_list

(** [set_array st arr lst] is the array after the piece represented by [lst] in
    [st] is locked in place. *)
let rec set_array st arr = function
  | [] -> arr
  | h :: t -> 
    let row = Array.get arr (fst h) in 
    Array.set row (snd h) (Mino st.curr_piece_type);
    Array.set arr (fst h) row; set_array st arr t

(** [get_element a x y] is the element in [a] at position [y,x] provided that
    [x] and [y] are non-negative; if they are, [0] is substituted instead. *)
let get_element (arr: 'a array array) x y = 
  match y < 0, x < 0 with
  | true, true -> Array.get (Array.get arr 0) 0
  | true, false -> Array.get (Array.get arr 0) x 
  | false, true -> Array.get (Array.get arr y) 0 
  | false, false -> Array.get (Array.get arr y) x 

(** [is_filled st c] is true iff [c] in [st] is not empty. *)
let is_filled st coords = [coords] |> is_valid_loc st |> not

(** [tspin st] is [true] iff the locking piece in [st] performed
    a t-spin. *)
let tspin st =
  let tspin_helper st = begin
    let fold_func = match st.curr_orient with
      | North -> 
        fun (o1, o2) (n1, n2) -> 
          (if n1 - 1 < o1 then n1 - 1 else o1), if n2 < o2 then n2 else o2
      | East -> 
        fun (o1, o2) (n1, n2) -> 
          (if n1 < o1 then n1 else o1), if n2 - 1 < o2 then n2 - 1 else o2
      | _ -> 
        fun (o1, o2) (n1, n2) -> 
          (if n1 < o1 then n1 else o1), if n2 < o2 then n2 else o2
    in
    let c1, c2 = List.fold_left fold_func (40,10) st.curr_piece in
    let coord_ll = is_filled st (c1, c2) in 
    let coord_lu = is_filled st (c1 + 2, c2) in
    let coord_ru = is_filled st (c1 + 2, c2 + 2) in
    let coord_rl = is_filled st (c1, c2 + 2) in
    match st.curr_orient with
    | North -> coord_lu && coord_ru && (coord_ll || coord_rl)
    | East -> coord_ru && coord_rl && (coord_ll || coord_lu)
    | South -> coord_ll && coord_rl && (coord_lu || coord_ru)
    | West -> coord_ll && coord_lu && (coord_ru || coord_rl)
  end in
  if st.curr_piece_type = PieceT then tspin_helper st else false

(** [lock st] is the state after the current piece in [st] is locked 
    in place. *)
let lock st =
  let lock_out = List.for_all (fun (a, b) -> a > 19) st.curr_piece in
  let new_gameboard = set_array st (deep_copy st.game_board) st.curr_piece in 
  let is_tspin = tspin st in
  ({ st with 
     game_board = new_gameboard; 
     can_hold = true; 
     is_alive = not lock_out && st.is_alive (* LOCK OUT loss condition *)
   }, is_tspin)

(** [list_clear acc lst] is the list of rows that don't need to be cleared. *)
let rec list_clear acc lst = match lst with
  | [] -> acc
  | h :: t -> if Array.exists (fun a -> a = Empty) h then 
      list_clear (h :: acc) t 
    else list_clear acc t

(** [remove_rows arr acc num_acc lst] is the list representing the array with 
    rows not in [lst] removed. *)
let rec remove_rows arr acc num_acc lst= 
  match lst with
  | [] -> acc
  | h :: t -> if h = (Array.get arr num_acc) then 
      remove_rows arr acc (num_acc + 1) lst else if 
      num_acc = 20 then remove_rows arr acc 0 t else 
      remove_rows arr (h :: acc) (0) t

(** [disp_text t st] is [st] except the text [t] will display every time the
    state is printed for the next two seconds. Will override any text already
    being printed for this state. *)
let disp_text t st = 
  { st with 
    text_start_time = Unix.gettimeofday (); 
    text_to_disp = t }

type clears = 
  | NoClear
  | SingleClear
  | DoubleClear
  | TripleClear
  | TetrisClear
  | TspinNoClear
  | TspinSingleClear
  | TspinDoubleClear
  | TspinTripleClear

(** [clear_type st tspin n] is the type of clear after all the clearable rows in
    [st] are computed, provided that [tspin] is whether or not the last locking
    piece performed a t-spin and [n] is how many rows were cleared. *)
let clear_type st is_tspin = function
  | 1 -> if is_tspin then TspinSingleClear else SingleClear
  | 2 -> if is_tspin then TspinDoubleClear else DoubleClear
  | 3 -> if is_tspin then TspinTripleClear else TripleClear
  | 4 -> TetrisClear
  | _ -> if is_tspin then TspinNoClear else NoClear

(** [compute_score type] is the number of points awarded for a clear of type
    [type] divided by the current level *)
let compute_score = function
  | NoClear -> 0
  | SingleClear -> 100
  | DoubleClear -> 300
  | TripleClear -> 500
  | TetrisClear -> 800
  | TspinNoClear -> 400
  | TspinSingleClear -> 800
  | TspinDoubleClear -> 1200
  | TspinTripleClear -> 1600

(** [clear st] is the state after all the clearable rows in state [st] are 
    cleared, the score is updated, combos are displayed, and lines of junk
    are pushed, together in a tuple with the lines of junk to send to the
    opponent, if applicable. *)
let clear (st, is_tspin) = 
  let clearable_rows = list_clear [] (Array.to_list st.game_board) in 
  let score_type = clear_type st is_tspin (40 - List.length clearable_rows) in
  let new_score, is_back = match score_type with
    | NoClear -> 0, st.is_back_to_back
    | SingleClear -> (compute_score score_type), false
    | TspinNoClear -> (compute_score score_type), st.is_back_to_back
    | _ -> (if st.is_back_to_back then 
              ((3 * compute_score score_type) / 2) else 
              compute_score score_type), true in
  let add_btb t = 
    if st.is_back_to_back && is_back then "Back-to-Back " ^ t else t in
  let score_add_btb s = 
    if st.is_back_to_back && is_back then s + 1 else s in
  let show_text, junk_to_send, cleared_lines = match score_type with
    | NoClear -> "", 0, 0
    | SingleClear -> "", 0, 1
    | DoubleClear -> add_btb "Double", score_add_btb 1, 2
    | TripleClear -> add_btb "Triple", score_add_btb 2, 3
    | TetrisClear -> add_btb "TETRIS", score_add_btb 4, 4
    | TspinNoClear -> add_btb "T-Spin", 0, 0
    | TspinSingleClear -> add_btb "T-Spin Single", score_add_btb 2, 1
    | TspinDoubleClear -> add_btb "T-Spin Double", score_add_btb 4, 2
    | TspinTripleClear -> add_btb "T-Spin Triple", score_add_btb 6, 3 in
  let temp_gameboard = 
    (remove_rows (deep_copy (st.game_board)) [] 0 clearable_rows) 
    |> Array.of_list in
  let empty_length = 
    (Array.length st.game_board) - (Array.length temp_gameboard) in 
  let new_gameboard = 
    Array.append temp_gameboard 
      (Array.make empty_length (Array.make 10 Empty)) in  
  let junk_result = st.junk_lines - junk_to_send in
  let new_junk_lines = if junk_result > 0 then junk_result else 0 in
  let new_push_lines = if junk_result < 0 then (- junk_result) else 0 in
  ({ st with 
     game_board = new_gameboard; 
     current_score = st.current_score + new_score;
     is_back_to_back = is_back; 
     junk_lines = new_junk_lines;
     lines_cleared = st.lines_cleared + cleared_lines
   } |> disp_text show_text |> fill_junk, new_push_lines)

let next st = match (move_dir Down st) with
  | (nst, true) -> nst, 0
  | (nst, false) -> 
    let cleared_state, lines = nst |> lock |> clear in
    (cleared_state |> spawn), lines

let add_junk l st = { st with junk_lines = st.junk_lines + l }

let is_alive st = st.is_alive

let hold st = 
  let piece_hold = st.hold_piece in
  let piece_curr = st.curr_piece_type in 
  let piece_next = st.next_piece in 
  let spawn_piece = st.piece_spawn in 
  let new_spawn = shuffle piece_list in
  match piece_hold with 
  | None -> begin match spawn_piece with 
      | [] -> let new_piece = piece_coord piece_next in 
        { st with curr_piece = new_piece; 
                  curr_piece_type = piece_next; 
                  hold_piece = Some piece_curr; 
                  curr_orient = North;
                  can_hold = false;
        } 
      | a :: [] -> let new_piece = piece_coord a in 
        { st with 
          curr_piece = new_piece; curr_piece_type = a; 
          hold_piece = Some piece_curr; can_hold = false; 
          curr_orient = North;
          piece_spawn= new_spawn; next_piece = List.hd new_spawn
        } 
      | h :: a :: t -> let new_piece = piece_coord piece_next in 
        { st with 
          curr_piece = new_piece; 
          curr_piece_type = piece_next; 
          hold_piece = Some piece_curr; 
          curr_orient = North;
          can_hold = false; next_piece = a; piece_spawn = a :: t
        }
    end |> move_dir Down |> fst |> calc_ghost
  | Some h -> 
    if st.can_hold = true then let new_piece = piece_coord h in 
      { st with 
        curr_piece = new_piece; 
        curr_piece_type = h; 
        hold_piece = Some piece_curr; 
        can_hold = false;
        curr_orient = North
      } |> move_dir Down |> fst |> calc_ghost else
      calc_ghost st

let hard_drop st = 
  let rec keep_drop ist = match (move_dir Down ist) with
    | (inst, true) -> keep_drop inst
    | (inst, false) -> inst in
  st |> keep_drop

let rotate st is_clockwise = 
  let dir_comp = function
    | North -> if is_clockwise then East else West
    | East  -> if is_clockwise then South else North
    | South -> if is_clockwise then West else East
    | West  -> if is_clockwise then North else South
  in
  (* Generates the new coordinates *)
  let gen_new_cord (c1, c2) = 
    let c_shift (a, b) = (a + c1, b + c2) in
    match st.curr_piece_type with
    | PieceO -> failwith "Rotate RI failure"
    | PieceI -> begin
        match (dir_comp st.curr_orient) with
        | North -> List.map c_shift [(2,0); (2,1); (2,2); (2,3)]
        | East -> List.map c_shift [(0,2); (1,2); (2,2); (3,2)]
        | South -> List.map c_shift [(1,0); (1,1); (1,2); (1,3)]
        | West -> List.map c_shift [(0,1); (1,1); (2,1); (3,1)]
      end
    | PieceT -> begin
        match (dir_comp st.curr_orient) with
        | North -> List.map c_shift [(1,0);(1,1);(1,2);(2,1)]
        | East -> List.map c_shift [(2,1);(1,1);(0,1);(1,2)]
        | South -> List.map c_shift [(0,1);(1,0);(1,1);(1,2)]
        | West -> List.map c_shift [(0,1);(1,1);(2,1);(1,0)]
      end
    | PieceL -> begin
        match (dir_comp st.curr_orient) with
        | North -> List.map c_shift [(1,0);(1,1);(1,2);(2,2)]
        | East -> List.map c_shift [(0,1);(0,2);(1,1);(2,1)]
        | South -> List.map c_shift [(0,0);(1,0);(1,1);(1,2)]
        | West -> List.map c_shift [(2,0);(2,1);(1,1);(0,1)]
      end
    | PieceJ -> begin
        match (dir_comp st.curr_orient) with
        | North -> List.map c_shift [(1,0);(1,1);(1,2);(2,0)]
        | East -> List.map c_shift [(0,1);(1,1);(2,1);(2,2)]
        | South -> List.map c_shift [(1,0);(1,1);(1,2);(0,2)]
        | West -> List.map c_shift [(0,0);(0,1);(1,1);(2,1)]
      end
    | PieceS -> begin
        match (dir_comp st.curr_orient) with
        | North -> List.map c_shift [(1,0);(1,1);(2,1);(2,2)]
        | East -> List.map c_shift [(1,1);(2,1);(0,2);(1,2)]
        | South -> List.map c_shift [(0,0);(0,1);(1,1);(1,2)]
        | West -> List.map c_shift [(0,1);(1,1);(1,0);(2,0)]
      end
    | PieceZ -> begin
        match (dir_comp st.curr_orient) with
        | North -> List.map c_shift [(2,0);(2,1);(1,1);(1,2)]
        | East -> List.map c_shift [(0,1);(1,1);(1,2);(2,2)]
        | South -> List.map c_shift [(1,1);(0,1);(1,0);(0,2)]
        | West -> List.map c_shift [(0,0);(1,0);(1,1);(2,1)]
      end
  in
  (* Runs through the list of offsets and tests each, returning state *)
  let rec test_cord rot_points (cords : (int * int) list) =
    match rot_points with
    | [] -> st
    | h :: t ->
      let new_cord = List.map (fun (a, b) -> (a + fst h, b + snd h)) cords in
      if is_valid_loc st new_cord then 
        {st with curr_piece = new_cord; curr_orient = dir_comp st.curr_orient} 
        |> calc_ghost
      else
        test_cord t cords
  in
  (* Finds the pivot point and tests all necessary rotations, returning state *)
  let rotate_help f rot1 rot2 = 
    List.fold_left f (40,10) st.curr_piece |> gen_new_cord |> 
    test_cord (if is_clockwise then rot1 else rot2)
  in
  (* Helper functions for finding pivot points *)
  (* Min of list *)
  let take_min a b = if a < b then a else b in
  (* Min of list minus 1 *)
  let take_min_min1 a b = if a - 1 < b then a - 1 else b in
  match st.curr_piece_type with
  | PieceO -> {st with curr_orient = dir_comp st.curr_orient}
  | PieceI -> begin
      match st.curr_orient with
      | North -> rotate_help
                   (fun (o1, o2) (n1, n2) -> (n1 - 2, take_min n2 o2)) 
                   [(0,0);(0,-2);(-1,1);(-1,-2);(2,1)] 
                   [(0,0);(0,-1);(0,2);(2,-1);(-1,2)]
      | East -> rotate_help
                  (fun (o1, o2) (n1, n2) -> (take_min n1 o1), n2 - 2) 
                  [(0,0);(0,-1);(0,2);(2,-1);(-1,2)]
                  [(0,0);(0,2);(0,-1);(1,2);(-2,-1)]
      | South -> rotate_help
                   (fun (o1, o2) (n1, n2) -> (n1 - 1, take_min n2 o2)) 
                   [(0,0);(0,2);(0,-1);(1,2);(-2,-1)]
                   [(0,0);(0,1);(0,-2);(-2,-1);(1,-2)]
      | West -> rotate_help
                  (fun (o1, o2) (n1, n2) -> (take_min n1 o1), n2 - 1) 
                  [(0,0);(0,1);(0,-2);(-2,1);(1,-2)]
                  [(0,0);(0,-2);(0,1);(-1,-2);(2,1)]
    end
  | _ ->
    match st.curr_orient with
    | North -> rotate_help
                 (fun (o1, o2) (n1, n2) -> 
                    take_min_min1 n1 o1, take_min n2 o2)
                 [(0,0);(0,-1);(1,-1);(-2,0);(-2,-1)]
                 [(0,0);(0,1);(1,1);(-2,0);(-2,1)]
    | East -> rotate_help
                (fun (o1, o2) (n1, n2) ->
                   take_min n1 o1, take_min_min1 n2 o2)
                [(0,0);(0,1);(-1,1);(2,0);(2,1)]
                [(0,0);(0,1);(-1,1);(2,0);(2,1)]
    | South -> rotate_help
                 (fun (o1, o2) (n1, n2) -> 
                    (take_min n1 o1), take_min n2 o2)
                 [(0,0);(0,1);(1,1);(-2,0);(-2,1)]
                 [(0,0);(0,-1);(1,-1);(-2,0);(-2,-1)]
    | West -> rotate_help
                (fun (o1, o2) (n1, n2) -> 
                   (take_min n1 o1), take_min n2 o2)
                [(0,0);(0,-1);(-1,-1);(2,0);(2,-1)]
                [(0,0);(0,-1);(-1,-1);(2,0);(2,-1)]

(** Ai moves. *)
include Ai

(** [turn_board_to_bool board] is the [bool] representation of [board] 
    based on [space] <> [Empty] (i.e. if [space] <> [Empty], the space
    is filled with [true].) *)
let turn_board_to_bool board = 
  Array.map (fun x -> Array.map 
                (fun p -> if p = Empty then false else true) x) board

(** [merge_piece_to_board st piece] is the state after locking [piece] 
    on [st]. *)
let merge_piece_to_board st pos = 
  let new_board = deep_copy st.game_board in
  let rec merge_helper = function
    | [] -> new_board
    | (a, b) :: t -> 
      new_board.(a).(b) <- (Mino st.curr_piece_type); 
      merge_helper t
  in
  merge_helper pos

(** [possible_pos bst piece is_left acc] is the possible positions of [piece] 
    in [bst] if moved in the direction represented by [is_left] horizontally. 
    [is_left] is true if the piece is moved to the left and is false if it is 
    moved to the right. Result represented by [acc]. *)
let rec possible_pos board_state dir acc = 
  let new_state, success = move_dir dir board_state in
  match success with
  | false -> acc
  | true -> 
    let new_curr_piece = new_state.curr_piece in
    let new_acc = new_curr_piece :: acc in
    possible_pos new_state dir new_acc

(** [compute_pos_dir bst piece] is the list of possible lockdown positions 
    for [piece] in [bst]. *)
let compute_pos_dir board_state = 
  let left_list = 
    (let start_pos_left = 
       possible_pos board_state Left [board_state.curr_piece] in 
     List.fold_left (fun acc x -> x :: acc) [] 
       (List.rev_map 
          (fun x -> let new_board = {board_state with curr_piece = x} in 
            new_board |> hard_drop |> get_curr_piece) start_pos_left)) in
  let start_pos_right = possible_pos board_state Right [] in 
  List.fold_left (fun acc x -> x :: acc) left_list
    (List.rev_map 
       (fun x -> let new_board = {board_state with curr_piece = x} in  
         new_board |> hard_drop |> get_curr_piece) start_pos_right)

(** [get_rotate st acc] is the possible positions after performing rotation 
    operations on [st]. *)
let rec get_rotate board_state acc = 
  if board_state.curr_piece_type = PieceO then acc else
  if List.length acc = 4 then acc else
    let new_state = rotate board_state true in
    let new_pos = new_state.curr_piece in 
    get_rotate new_state (new_pos :: acc)

(** [compute_pos board_state] is a list of all the positions that
    [board_state.curr_piece] could land in [board_state].
    Requires: [board_state] is not empty. *)
let compute_pos board_state = 
  let four_dirs = get_rotate board_state [board_state.curr_piece] in
  List.flatten 
    (List.rev_map 
       (fun x -> let new_board = {board_state with curr_piece = x} in
         compute_pos_dir new_board) four_dirs)

let ai_pos st = 
  let pos_cand = compute_pos st in
  let game_board_cand = List.map 
      (fun x -> (merge_piece_to_board st x), x) pos_cand in
  let bool_board = List.map 
      (fun x -> (turn_board_to_bool (fst x)), snd x) game_board_cand in
  let score_pos_list = List.map 
      (fun x -> (compute_score (fst x)), snd x) bool_board in
  let score_list = List.map (fun x -> fst x) score_pos_list in
  let maximum = List.fold_left max (List.hd score_list) score_list in
  (* print_endline (print_lst_lst "" pos_cand);
     print_endline (print_lst_float "" score_list);
     print_float maximum; *)
  List.assoc maximum score_pos_list

let compute_next_ai_move st = 
  let new_pos = ai_pos st in
  {st with curr_piece = new_pos} |> next
