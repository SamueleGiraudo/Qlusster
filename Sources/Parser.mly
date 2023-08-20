(* Author: Samuele Giraudo
 * Creation: may 2021
 * Modifications: may 2021, jun. 2021, aug. 2021, nov. 2021, dec. 2021, jan. 2022,
 * mar. 2022, may 2022, aug. 2022, nov. 2022, apr. 2023, jul. 2023, aug. 2023
 *)

%token L_PAR R_PAR
%token SEGMENT
%token ADDITION
%token MULTIPLICATION
%token EXPONENTIATION
%token LOOP
%token STRETCH
%token CONCATENATION
%token DURATION
%token <Scalars.scalars> SCALAR
%token EOF

%start <Expressions.expressions> expression

%%

expression:
    |e=expression_1 EOF {e}

expression_1:
    (* 'FLOATING_DECIMAL_NUMBER *)
    |x=SCALAR {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Point (info, x)
    }

    (* (('segment EXP_1) EXP_2) *)
    |L_PAR L_PAR SEGMENT e1=expression_1 R_PAR e2=expression_1 R_PAR {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Segment (info, e1, e2)
    }

    (* (('addition EXP_1) EXP_2) *)
    |L_PAR L_PAR ADDITION e1=expression_1 R_PAR e2=expression_1 R_PAR {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Pointwise (info, Expressions.Addition, e1, e2)
    }

    (* (('multiplication EXP_1) EXP_2) *)
    |L_PAR L_PAR MULTIPLICATION e1=expression_1 R_PAR e2=expression_1 R_PAR {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Pointwise (info, Expressions.Multiplication, e1, e2)
    }

    (* (('exponentiation EXP_1) EXP_2) *)
    |L_PAR L_PAR EXPONENTIATION e1=expression_1 R_PAR e2=expression_1 R_PAR {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Pointwise (info, Expressions.Exponentiation, e1, e2)
    }

    (* (('loop EXP_1) EXP_2) *)
    |L_PAR L_PAR LOOP e1=expression_1 R_PAR e2=expression_1 R_PAR {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Loop (info, e1, e2)
    }

    (* (('stretch EXP_1) EXP)_2 *)
    |L_PAR L_PAR STRETCH e1=expression_1 R_PAR e2=expression_1 R_PAR {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Stretch (info, e1, e2)
    }

    (* (('concatenation EXP_1) EXP_2) *)
    |L_PAR L_PAR CONCATENATION e1=expression_1 R_PAR e2=expression_1 R_PAR {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Concatenation (info, e1, e2)
    }

    (* ('duration EXP) *)
    |L_PAR DURATION e=expression_1 R_PAR {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Duration (info, e)
    }

