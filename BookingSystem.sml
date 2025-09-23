(* Booking System in Standard ML *)

datatype slot = Free | Booked of string
type system = slot list

(* Initialize N slots as Free *)
fun initSystem 0 = []
  | initSystem n = Free :: initSystem (n-1)

(* Show system state *)
fun showSystem [] _ = ()
  | showSystem (Free::rest) idx =
      (print ("[" ^ Int.toString idx ^ "] FREE\n");
       showSystem rest (idx+1))
  | showSystem (Booked name::rest) idx =
      (print ("[" ^ Int.toString idx ^ "] BOOKED by " ^ name ^ "\n");
       showSystem rest (idx+1))

(* Book a slot *)
fun bookSlot idx name sys =
  let
    fun helper [] _ = []
      | helper (Free::rest) 0 = Booked name :: rest
      | helper (Booked x::rest) 0 = Booked x :: rest
      | helper (x::rest) i = x :: helper rest (i-1)
  in
    helper sys idx
  end

(* Cancel a slot *)
fun cancelSlot idx sys =
  let
    fun helper [] _ = []
      | helper (Booked x::rest) 0 = Free :: rest
      | helper (Free::rest) 0 = Free :: rest
      | helper (x::rest) i = x :: helper rest (i-1)
  in
    helper sys idx
  end

(* Demo run *)
(*
val sys0 = initSystem 5
val _ = showSystem sys0 0

val sys1 = bookSlot 2 "Alice" sys0
val _ = showSystem sys1 0

val sys2 = cancelSlot 2 sys1
val _ = showSystem sys2 0
*)

(*
- val sys0 = initSystem 3;
- showSystem sys0 0;
[0] FREE
[1] FREE
[2] FREE

- val sys1 = bookSlot 1 "Alice" sys0;
- showSystem sys1 0;
[0] FREE
[1] BOOKED by Alice
[2] FREE

- val sys2 = cancelSlot 1 sys1;
- showSystem sys2 0;
[0] FREE
[1] FREE
[2] FREE
*)
