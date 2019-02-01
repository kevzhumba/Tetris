(** [height board y] is the height of column [y] where height is defined 
    as the height of the tallest filled space in the column. *)
let rec height board y = 
  let rec height_sum x = 
    if x < 0 then 0 else
    if board.(x).(y) then (x + 1) else
      height_sum (x - 1)
  in
  height_sum (Array.length board.(0) - 1)
(** [aggregate_height board y sum] is sum of the heights of all columns in 
    [board]. *)
let rec aggregate_height board y sum = 
  if y < Array.length board.(0) then 
    aggregate_height board (y+1) ((height board y) + sum) 
  else sum

(** [is_row_full row] determines whether a given row [row] is 
    completely full. *)
let is_row_full row = 
  let rec row_fill row x acc = 
    if x < Array.length row then
      if row.(x) = true then row_fill row (x+1) (acc+1) 
      else row_fill row (x+1) acc
    else acc
  in 
  if (row_fill row 0 0) = Array.length row then true else false

let rec num_clears board index sum =
  if index < Array.length board then 
    if is_row_full board.(index) then num_clears board (index + 1) (sum + 1) 
    else num_clears board (index + 1) sum
  else sum

(** [holes_sum board y] is the sum of holes in a given column [y]. *)
let rec holes_sum board y = 
  let rec holes_summer x found acc = 
    if x < 0 then acc else
    if found then 
      if board.(x).(y) then holes_summer (x - 1) found acc else
        holes_summer (x - 1) found (acc + 1)
    else if board.(x).(y) then holes_summer (x - 1) true acc else
      holes_summer (x - 1) found acc
  in
  holes_summer (Array.length board - 1) false 0


let rec holes board y sum = 
  if y < Array.length board.(0) then 
    holes board (y+1) ((holes_sum board y) + sum) 
  else sum

(** [abs_diff h1 h2] is the absolute difference between value [h1] and [h2]*)
let abs_diff h1 h2 = abs(h1 - h2)

let rec bumpiness board y v = 
  if y < Array.length board.(0) - 1 then 
    let h1 = height board y in
    let h2 = height board (y + 1) in
    bumpiness board (y + 1) (abs_diff h1 h2 + v)
  else v

let rec continuous board y x v = 
  if y = Array.length board then v else if x = Array.length board.(y) 
  then continuous board (y+1) 0 v else
  if (x = 0) && board.(y).(x) then (continuous board (y) (x+1) (v))
  else if board.(y).(x) = true then 
    match board.(y).(x-1) with
    | true ->
      (continuous board (y) (x+1) (v+1) )
    | false ->
      (continuous board (y) (x+1) (v))
  else continuous board y (x+1) v

let compute_score board = 
  let clear_score = float_of_int (num_clears board 0 0) in 
  let hole_score = float_of_int (holes board 0 0) in
  let bumpiness_score = float_of_int (bumpiness board 0 0) in
  let continuity_score = float_of_int (continuous board 0 0 0 ) in
  (0.76 *. clear_score) +. (-0.36 *. hole_score) +. 
  (-0.18 *. bumpiness_score) +. (0.10 *. continuity_score)

(* Algorithm takes inspiration from
   https://codemyroad.wordpress.com/2013/04/14/tetris-ai-the-near-perfect-player
*)
