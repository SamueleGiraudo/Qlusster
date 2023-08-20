(* Author: Samuele Giraudo
 * Creation: (jul. 2020), may 2021
 * Modifications: (jul. 2020, aug. 2020, dec. 2020, jan. 2021), may 2021, jun. 2021,
 * aug. 2021, nov. 2021, dec. 2021, jan. 2022, mar. 2022, may 2022, aug. 2022, nov. 2022,
 * apr. 2023, jul. 2023, aug. 2023 *)

(* The three kinds of operations. *)
type operations =
    |Addition
    |Multiplication
    |Exponentiation

(* A type to represent expressions. *)
type expressions =
    (* Point. *)
    |Point of (Information.information * Scalars.scalars)

    (* Segment. *)
    |Segment of (Information.information * expressions * expressions)

    (* Pointwise operation between two signals. *)
    |Pointwise of (Information.information * operations * expressions * expressions)

    (* Looping a signal. *)
    |Loop of (Information.information * expressions * expressions)

    (* Stretching a signal. *)
    |Stretch of (Information.information * expressions * expressions)

    (* Concatenating two signals. *)
    |Concatenation of (Information.information * expressions * expressions)

    (* Duration of a signal. *)
    |Duration of (Information.information * expressions)

(* Returns the function realizing each of the three operations. *)
let operation_to_function op =
    match op with
        |Addition -> (+.)
        |Multiplication -> ( *. )
        |Exponentiation -> ( ** )

