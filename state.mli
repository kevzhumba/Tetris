(** Information about the state of the game. *)

(** The abstract type of values representing state *)
type t 

(** [get_score st] is the current score in [st]. *)
val get_score: t -> int

(** [get_level st] is the current level in [st]. *)
val get_level: t -> int

(** [start ()] is the state at the start of the game *)
val start : unit -> t

(** [move st is_left] is the new state after the tetris piece is moved
    left or right in state [st] *)
val move : t -> bool -> t

(** [print_state st] prints the gameboard represented by state [st] *)
val print_state : t -> unit

(** [next st] is the next state after the game has ticked one move down and
    the number of lines of junk to send to the other player if playing
    multiplayer. *)
val next : t -> t * int

(** [is_alive st] is [true] iff the game represented by [st] is still going *)
val is_alive : t -> bool

(** [hard_drop st] is the state after the current piece has been hard dropped
    to the bottom of the matrix, but not locked. *)
val hard_drop : t -> t

(** [rotate st is_clockwise] is the new state after the current piece has been
    rotated clockwise if [is_clockwise] is [true] or counterclockwise otherwise.
    If the rotation is invalid, the result is just [st] *)
val rotate : t -> bool -> t

(** [hold st] is the state after the current piece is held if it can be. The 
    hold command can only be used once per piece lock. *)
val hold : t -> t

(** [start_custom t p] is a new game state initialized with unspecified minos
    filling all the coordinates specified in [t] and the first pieces to spawn
    specified by their IDs in [p], where a tetrimino's ID is given by the order
    they are defined in the 2009 Official Tetris Design Guidelines,
    zero-indexed. Useful for potential future game modes like Puzzle Mode where
    the board spawns with junk already in place for the player to clear. Also
    used in testing. 
    Requires: [p] has length greater than or equal to 2 for the pieces
              to spawn as desired; otherwise, pieces will spawn randomly
              as normal. 
              [t] must not contain any coordinates where the first piece spawns
              and drops if [p] has length 1 or less, as the blocks in [t] may be 
              added to the matrix after the first piece spawns. *)
val start_custom : (int * int) list -> int list -> t

(** [get_curr_piece t] is the coordinates of the current piece in [t]. *)
val get_curr_piece : t -> (int * int) list

(** [print_state_multi s1 s2] prints the multiplayer gameboard represented by 
    state [s1] and [s2]*)
val print_state_multi : t -> t -> unit

(** [add_junk lines st] is [st] after [lines] lines of junk have been sent
    to the game board.
    Requires: [l] is non-negative. *)
val add_junk : int -> t -> t

(** [ai_pos st] is the optimal position for the current piece in current game 
    state [st]. It is also used in testing. *)
val ai_pos: t -> (int * int) list

(** [compute_next_ai_move t] is the resulting state after the AI has decided
    where to lock down the current piece, along with the number of lines of
    junk to send to the opponent player after the current piece is locked. *)
val compute_next_ai_move : t -> t * int

