(* Author: Samuele Giraudo
 * Creation: (mau 2021), may 2022
 * Modifications: may 2022, aug. 2022, nov. 2022
 *)

(* The different kinds of errors a program can contain. *)
type kinds =
    |SyntaxError of Lexer.error_kind

(* The type to represent information about an error. *)
type errors = {

    (* Some information about the subexpression where the error appears. *)
    information: Information.information;

    (* The kind of the error. *)
    kind: kinds
}

(* Returns the error obtained from the lexer error err. The kind of the returned error is
 * SyntaxError. *)
let syntax_error_from_lexer err =
    let info = Information.construct (Lexer.error_to_position err) in
    let kind = SyntaxError (Lexer.error_to_error_kind err) in
    {information = info; kind = kind}

(* Returns a string representation of the error err. *)
let to_string err =
    Information.to_string err.information ^ ": " ^
    match err.kind with
        |SyntaxError err -> Lexer.error_kind_to_string err

