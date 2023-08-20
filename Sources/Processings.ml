(* Author: Samuele Giraudo
 * Creation: may 2021
 * Modifications: may 2021, jun. 2021, aug. 2021, nov. 2021, dec. 2021, jan. 2022,
 * mar. 2022, may 2022, aug. 2022, nov. 2022, jul. 2023
 *)

(* An expression checking contains an option on an expression and a list of errors. When
 * this list of errors not empty, the option on the expression is None. *)
type processings = {
    expression: Expressions.expressions option;
    errors: Errors.errors list;
}

(* Returns the expression of the processing pr. The expression of pr must be different from
 * None. *)
let expression pr =
    assert (Option.is_some pr.expression);
    Option.get pr.expression

(* Returns the list of the errors of the processing pr. *)
let errors pr =
    pr.errors

(* Tests if there are errors in the processing pr. *)
let has_errors pr =
    pr.errors <> []

(* Returns the processing of the expression contained in the file at path path. *)
let process_path path =
    try
        let e = Files.path_to_expression path in
        {expression = Some e; errors = []}
    with
        |Lexer.Error err ->
            {expression = None; errors = [Errors.syntax_error_from_lexer err]}

