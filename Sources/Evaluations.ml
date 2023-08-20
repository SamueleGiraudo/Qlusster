(* Author: Samuele Giraudo
 * Creation: (may 2021), jul. 2023
 * Modifications: jul. 2023, aug. 2023
 *)

(* Returns the sound specified by the expression e. *)
let compute e =
    let rec aux e =
        match e with
            |Expressions.Point (_, sc) -> sc |> Scalars.value |> Sounds.point
            |Expressions.Segment (_, e1, e2) ->
                Sounds.segment (Sounds.value (aux e1) 0.0) (Sounds.value (aux e2) 0.0)
            |Expressions.Pointwise (_, op, e1, e2) ->
                Sounds.pointwise_operation
                    (Expressions.operation_to_function op)
                    (aux e1)
                    (aux e2)
            |Expressions.Loop (_, e1, e2) ->
                Sounds.loop (Sounds.value (aux e1) 0.0) (aux e2)
            |Expressions.Stretch (_, e1, e2) ->
                Sounds.stretch (Sounds.value (aux e1) 0.0) (aux e2)
            |Expressions.Concatenation (_, e1, e2) -> Sounds.concatenate (aux e1) (aux e2)
            |Expressions.Duration (_, e1) -> e1 |> aux |> Sounds.duration |> Sounds.point
    in
    aux e

