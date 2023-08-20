(* Author: Samuele Giraudo
 * Creation: (mau 2021), may 2022
 * Modifications: may 2022, aug. 2022, nov. 2022, jul. 2023
 *)

(* The extension of Qlusster files. *)
let extension = ".qlu"

(* Returns the expression specified by the Qlusster file at path path. The exception
 * Lexer.Error is raised when there are syntax errors in the program. *)
let path_to_expression path =
    Lexer.value_from_file_path path Parser.expression Lexer.read

