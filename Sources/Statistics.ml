(* Author: Samuele Giraudo
 * Creation: may 2022
 * Modifications: may 2022, aug. 2022, nov. 2022, dec. 2022, jul. 2023, aug. 2023
 *)

(* A type to collect some statistics about an expression. *)
type statistics = {
    nb_points: int;
    nb_segments: int;
    nb_additions: int;
    nb_multiplications: int;
    nb_exponentiations: int;
    nb_loops: int;
    nb_stretches: int;
    nb_concatenations: int;
    nb_durations: int;
    height: int
}

(* Returns the size of the statistics st. This is the total number of nodes which are
 * counte by st. *)
let size st =
    st.nb_points +
    st.nb_segments +
    st.nb_additions +
    st.nb_multiplications +
    st.nb_exponentiations +
    st.nb_loops +
    st.nb_stretches +
    st.nb_concatenations +
    st.nb_durations

(* Returns a string representation of the statistics st. *)
let to_string st =
     let string_if condition str =
        if condition then str else ""
    in
    string_if (st.nb_points >= 1) (Printf.sprintf "Nb. points: %d\n" st.nb_points)
    ^
    string_if (st.nb_segments >= 1) (Printf.sprintf "Nb. segments: %d\n" st.nb_segments)
    ^
    string_if (st.nb_additions >= 1) (Printf.sprintf "Nb. additions: %d\n" st.nb_additions)
    ^
    string_if (st.nb_multiplications >= 1)
        (Printf.sprintf "Nb. multiplications: %d\n" st.nb_multiplications)
    ^
    string_if (st.nb_exponentiations >= 1)
        (Printf.sprintf "Nb. exponentiations: %d\n" st.nb_exponentiations)
    ^
    string_if (st.nb_loops >= 1) (Printf.sprintf "Nb. loops: %d\n" st.nb_loops)
    ^
    string_if (st.nb_stretches >= 1) (Printf.sprintf "Nb. stretches: %d\n" st.nb_stretches)
    ^
    string_if (st.nb_concatenations >= 1)
        (Printf.sprintf "Nb. concatenations: %d\n" st.nb_concatenations)
    ^
    string_if (st.nb_durations >= 1) (Printf.sprintf "Nb. durations: %d\n" st.nb_durations)
    ^
    Printf.sprintf "Nb. nodes: %d\n" (size st)
    ^
    Printf.sprintf "Height: %d\n" st.height

(* Returns the empty statistics. *)
let empty =
    {nb_points = 0;
    nb_segments = 0;
    nb_additions = 0;
    nb_multiplications = 0;
    nb_exponentiations = 0;
    nb_loops = 0;
    nb_stretches = 0;
    nb_concatenations = 0;
    nb_durations = 0;
    height = 0}

(* Returns the statistics obtained by merging the statistics st1 and st2. *)
let merge st1 st2 =
    {nb_points = st1.nb_points + st2.nb_points;
    nb_segments = st1.nb_segments + st2.nb_segments;
    nb_additions = st1.nb_additions + st2.nb_additions;
    nb_multiplications = st1.nb_multiplications + st2.nb_multiplications;
    nb_exponentiations = st1.nb_exponentiations + st2.nb_exponentiations;
    nb_loops = st1.nb_loops + st2.nb_loops;
    nb_stretches = st1.nb_stretches + st2.nb_stretches;
    nb_concatenations = st1.nb_concatenations + st2.nb_concatenations;
    nb_durations = st1.nb_durations + st2.nb_durations;
    height = max st1.height st2.height}

(* Returns the statistics collected from the expression e. *)
let compute e =
    let rec aux e =
        match e with
            |Expressions.Point _ -> {empty with nb_points = 1}
            |Expressions.Segment (_, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with nb_segments = 1 + st.nb_segments; height = 1 + st.height}
            |Expressions.Pointwise (_, Expressions.Addition, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with nb_additions = 1 + st.nb_additions; height = 1 + st.height}
            |Expressions.Pointwise (_, Expressions.Multiplication, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with
                    nb_multiplications = 1 + st.nb_multiplications;
                    height = 1 + st.height}
            |Expressions.Pointwise (_, Expressions.Exponentiation, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with
                    nb_exponentiations = 1 + st.nb_exponentiations;
                    height = 1 + st.height}
            |Expressions.Loop (_, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with nb_loops = 1 + st.nb_loops; height = 1 + st.height}
            |Expressions.Stretch (_, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with nb_stretches = 1 + st.nb_stretches; height = 1 + st.height}
            |Expressions.Concatenation (_, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with
                    nb_concatenations = 1 + st.nb_concatenations;
                    height = 1 + st.height}
            |Expressions.Duration (_, e1) ->
                let st = aux e1 in
                {st with nb_durations = 1 + st.nb_durations; height = 1 + st.height}
    in
    aux e

