(* Author: Samuele Giraudo
 * Creation: (aug. 2016), apr. 2020
 * Modifications: apr. 2020, may 2020, jul. 2020, aug. 2020, dec. 2020, jan. 2021, may 2021,
 * aug. 2021, nov. 2021, dec. 2021, jan. 2022, may 2022, aug. 2022, nov. 2022, dec. 2022,
 * jan. 2023, jul. 2023, aug. 2023, dec. 2023, jan. 2024
 *)

(* Qlusster - A program to program music mixing additive and granular synthesis. *)

let name = "Qlusster"

let logo = "[]|_|/"

let description = "A minimalist granular synthesizer machine."

(*let version = "0.0001" and version_date = "2020-08-27" *)
(*let version = "0.0010" and version_date = "2020-12-12" *)
(*let version = "0.0011" and version_date = "2021-01-01" *)
(*let version = "0.0100" and version_date = "2021-05-30" *)
(*let version = "0.0101" and version_date = "2021-08-20" *)
(*let version = "0.0110" and version_date = "2021-12-29"*)
(*let version = "0.0111" and version_date = "2022-01-01"*)
(*let version = "0.1000" and version_date = "2022-05-10"*)
(*let version = "0.1001" and version_date = "2022-05-30"*)
(*let version = "0.1010" and version_date = "2022-08-27"*)
(*let version = "0.1011" and version_date = "2022-11-20"*)
(*let version = "0.1100" and version_date = "2022-12-25"*)
(*let version = "0.1101" and version_date = "2023-01-01"*)
(*let version = "0.1110" and version_date = "2023-07-17"*)
(*let version = "0.1111" and version_date = "2023-07-31"*)
(*let version = "1.0000" and version_date = "2023-08-07"*)
(*let version = "1.0001" and version_date = "2023-08-20"*)
let version = "1.0010" and version_date = "2024-01-07"

let author = "Samuele Giraudo"

(*let email = "samuele.giraudo@univ-eiffel.fr"*)
let email = "giraudo.samuele@uqam.ca"

(* Returns a string of information about the Aclove program. *)
let information =
    Printf.sprintf "%s\n%s\n%s\nCopyright (C) 2022--2024 %s\nWritten by %s [%s]\n\
        Version: %s (%s)\n"
        logo name description author author email version version_date

(* Returns the help string about the arguments of the program. *)
let help_string =
      "Usage:\n    ./qlusster [--help] [--version] --file PATH [--verbose LVL] \
      [--bunch START LEN] [--write] [--draw] [--play] \nwhere:\n"
    ^ "    + `--help` prints the short help (the present text).\n"
    ^ "    + `--version` prints the version and other information.\n"
    ^ "    + `--file PATH` sets PATH as the path to the Qlusster program to consider, \
             contained in a " ^ Files.extension ^ " file.\n"
    ^ "    + `--verbose LVL` enables the verbose mode at level `LVL`, from 0 (nothing) to \
             2 (full). By default, the level is 1.\n"
    ^ "    + `--bunch START LEN` specifies the part of the generated signal to consider, \
             with its starting time START and length LEN in seconds.\n"
    ^ "    + `--write` creates the PCM file specified by the program.\n"
    ^ "    + `--draw` creates the SVG and PNG files specified by the program.\n"
    ^ "    + `--play` plays the signal specified by the program.\n"

(* Prints the error message msg followed by a line break, and halts the execution. *)
let error msg =
    "Error: " ^ msg ^ "\n" |> Outputs.print_error;
    exit 1

(* Returns the bunch specified by the standard input. *)
let read_bunch () =
    try
        match Arguments.option_values "--bunch" with
            |[] -> Bunches.construct None None
            |[x1] -> Bunches.construct (Some (float_of_string x1)) None
            |x1 :: x2 :: _ ->
                Bunches.construct (Some (float_of_string x1)) (Some (float_of_string x2))
    with
        |Failure _ -> error "after --bunch, there must be 0, 1, or 2 float arguments."

;;

(* Main expression. *)

(* Creation of the buffer directory if it does not exist. *)
let cmd = Printf.sprintf "mkdir -p %s" Buffers.path_directory in
Sys.command cmd |> ignore;

(* Version. *)
if Arguments.exists "--version" then begin
    Outputs.print_success information;
    exit 0
end;

(* Help. *)
if Arguments.exists "--help" then begin
    Outputs.print_information_1 help_string;
    exit 0
end;

(* Reads the Qlusster program file path. *)
let path =
    match Arguments.option_value "--file" with
        |None -> error "the option --file must be given and followed by one path."
        |Some path -> path
in

(* Checks the existence of the file at path path. *)
if Sys.file_exists path |> not then
    error "there is no file %s.";

(* Checks if the file has the right extension. *)
if not (Paths.has_extension Files.extension path) then
    error (Printf.sprintf "the file %s has not %s as extension." path Files.extension);

(* Reads the verbosity level. *)
let verbosity =
    if Arguments.exists "--verbose" |> not then
        1
    else
        match Arguments.bounded_integer_option_value 0 2 "--verbose" with
            |None -> error "one integer between 0 and 2 must follow the --verbose argument."
            |Some lvl -> lvl
in

(* Detecting a bunch specification. *)
let bunch = read_bunch () in

(* Writing a PCM file. *)
if Arguments.exists "--write" then
    Executions.interpret_path_and_write_sound verbosity path bunch;

(* Writing an SVG and PNG file. *)
if Arguments.exists "--draw" then
    Executions.interpret_path_and_draw_sound verbosity path bunch;

(* Playing. *)
if Arguments.exists "--play" then
    Executions.interpret_path_and_play verbosity path bunch;

exit 0

