datatype slot = Free | Booked of string

(* Create n free slots *)
fun makeSlots 0 = []
  | makeSlots n = Free :: makeSlots (n-1)

(* Show all slots *)
fun show [] _ = ()
  | show (Free::rest) i = 
      (print (Int.toString i ^ ": FREE\n"); show rest (i+1))
  | show (Booked name::rest) i = 
      (print (Int.toString i ^ ": " ^ name ^ "\n"); show rest (i+1))

(* Book slot at index *)
fun book [] _ _ = []
  | book (Free::rest) 0 name = Booked name :: rest
  | book (slot::rest) 0 name = slot :: rest
  | book (slot::rest) i name = slot :: book rest (i-1) name

(* Cancel slot at index *)
fun cancel [] _ = []
  | cancel (Booked _::rest) 0 = Free :: rest
  | cancel (slot::rest) 0 = slot :: rest
  | cancel (slot::rest) i = slot :: cancel rest (i-1)

(* Example usage *)
(* val slots = makeSlots 3;
show slots 0;

val slots1 = book slots 1 "Tawan";
show slots1 0;

val slots2 = cancel slots1 1;
show slots2 0; *)