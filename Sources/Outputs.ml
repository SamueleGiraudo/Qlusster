(* Author: Samuele Giraudo
 * Creation: jul. 2023
 * Modifications: jul. 2023, jan. 2024
 *)

module S = Strings

(* Prints the string str as an error. *)
let print_error str =
    "?? " ^ str |> S.csprintf S.Red |> print_string;
    flush stdout

(* Prints the string str as an information. *)
let print_information_1 str =
    ">> " ^ str |> S.csprintf S.Blue |> print_string;
    flush stdout

(* Prints the string str as an information. *)
let print_information_2 str =
    ">> " ^ str |> S.csprintf S.Magenta |> print_string;
    flush stdout

(* Prints the string str as an information. *)
let print_information_3 str =
    ">> " ^ str |> S.csprintf S.Yellow |> print_string;
    flush stdout

(* Prints the string str as a success. *)
let print_success str =
    "!! " ^ str |> S.csprintf S.Green |> print_string;
    flush stdout

