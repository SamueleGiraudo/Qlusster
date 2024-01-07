(* Author: Samuele Giraudo
 * Creation: may 2021
 * Modifications: may 2021, jun. 2021, aug. 2021, nov. 2021, dec. 2021, jan. 2022,
 * mar. 2022, may 2022, aug. 2022, nov. 2022, apr. 2023, jul. 2023, aug. 2023, dec. 2023
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

%token FENCE_3
%token FENCE_4
%token ABSOLUTE_VALUE
%token OPPOSITE
%token SUBTRACTION
%token DIFFERENCE
%token VERTICAL
%token INVERSE
%token MAXIMUM
%token MINIMUM
%token SQUEEZE
%token REVERSE
%token COMPLEMENTARY

%token EOF

%start <Expressions.expressions> expression

%%

expression:
    |e=expression_1 EOF {e}


expression_1:
    |e=expression_2 {e}

    |SEGMENT e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Segment (info, e1, e2)
    }

    |ADDITION e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Pointwise (info, Expressions.Addition, e1, e2)
    }

    |MULTIPLICATION e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Pointwise (info, Expressions.Multiplication, e1, e2)
    }

    |EXPONENTIATION e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Pointwise (info, Expressions.Exponentiation, e1, e2)
    }

    |LOOP e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Loop (info, e1, e2)
    }

    |STRETCH e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Stretch (info, e1, e2)
    }

    |CONCATENATION e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Concatenation (info, e1, e2)
    }

    |DURATION e=expression_1 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Duration (info, e)
    }

    |e=sugar {e}


expression_2:
    |L_PAR e=expression_1 R_PAR {e}

    |x=SCALAR {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Point (info, x)
    }


sugar:

    |FENCE_3 e1=expression_1 e2=expression_2 e3=expression_2 e4=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.fence info [e1; e2; e3; e4]
    }

    |FENCE_4 e1=expression_1 e2=expression_2 e3=expression_2 e4=expression_2
    e5=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.fence info [e1; e2; e3; e4; e5]
    }

    |ABSOLUTE_VALUE e=expression_1 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.absolute_value info e
    }

    |OPPOSITE e=expression_1 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.opposite info e
    }

    |SUBTRACTION e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.subtraction info e1 e2
    }

    |DIFFERENCE e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.difference info e1 e2
    }

    |VERTICAL e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.vertical info e1 e2
    }

    |INVERSE e=expression_1 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.inverse info e
    }

    |MAXIMUM e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.maximum info e1 e2
    }

    |MINIMUM e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.minimum info e1 e2
    }

    |SQUEEZE e1=expression_1 e2=expression_2 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.squeeze info e1 e2
    }

    |REVERSE e=expression_1 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.reverse info e
    }

    |COMPLEMENTARY e=expression_1 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Sugars.complementary info e
    }

