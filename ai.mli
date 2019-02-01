(** The logic for the artificial intelligence *)

(** [compute_score b] is the score given by [b], where [b] assumes that a piece
    has already locked down. *)
val compute_score : bool array array -> float

(** [continuous board y x v] is the number of connected side by side pairs of
    tiles in gameboard [board] *) 
val continuous : bool array array -> int -> int -> int -> int

(**[bumpiness board y v] is the variation of column heights in [board]. 
   bumpiness = |height of col 1 - height of col 2| + 
   |height of col 2 - height of col 3| + ...  *)
val bumpiness: bool array array -> int -> int -> int

(** [holes_sum board y] is the sum of holes in a given column [y] in board 
    [board]. *)
val holes: bool array array -> int -> int -> int

(** [num_clears board index sum] is the number of full rows in [board]. 
    In other words, it is how many rows would be cleared given [board]. *)
val num_clears: bool array array -> int -> int -> int
