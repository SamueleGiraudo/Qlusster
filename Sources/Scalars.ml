(* Author: Samuele Giraudo
 * Creation: (jul. 2020), jul. 2023
 * Modifications: jul. 2023, aug. 2023
 *)

(* The type of the scalars. *)
type scalars = Scalar of float

(* Returns the value of the scalar s. *)
let value s =
    let Scalar value = s in
    value

(* Returns the addition of the scalars s and s'. *)
let addition s s' =
    Scalar (value s +. value s')

(* Returns the multiplication of the scalars s and s'. *)
let multiplication s s' =
    Scalar (value s *. value s')

(* Returns the exponentiation of the scalars s and s'. *)
let exponentiation s s' =
    Scalar (value s ** value s')

(* Returns a string representation of the scalar s. For instance, if the value of the scalar
 * is 1.25, the returned string is "'1.25". If the value is -0.75, the returned string is
 * "'-0.75". *)
let to_string s =
    "'" ^ (s |> value |> string_of_float)

(* Returns the scalar specified by the string str. This string starts with ' and by dropping
 * this first character, it specifies a signed floating number in decimal. *)
let from_string str =
    let v = String.sub str 1 ((String.length str) - 1) |> float_of_string in
    Scalar v

(* Returns the rounded integer version of the float x. *)
let float_to_rounded_int x =
   int_of_float (Float.round x)

