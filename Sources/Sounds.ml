(* Author: Samuele Giraudo
 * Creation: (jul. 2015), apr. 2020
 * Modifications: apr. 2020, may 2020, jul. 2020, aug. 2020, dec. 2020, jan. 2021, may 2021
 * jun. 2021, aug. 2021, nov. 2021, dec. 2021, jan. 2022, may 2022, aug. 2022, nov. 2022,
 * dec. 2022, aug. 2023
 *)

(* Functional representation of a sound. *)
type sounds = {
    (* This map encodes the wave of the sound. This associates with each time in seconds a
     * value. *)
    wave: float -> float;

    (* The duration of the sound in seconds. *)
    duration: float
}

(* Returns the wave of the sound s. *)
let wave s =
    s.wave

(* Returns the duration of the sound s in seconds. *)
let duration s =
    s.duration

(* Returns the floating number x if its finite and 0.0 otherwise. *)
let finite_or_zero x =
    if Float.is_finite x then x else 0.0

(* Returns the value of the wave of the sound s at coordinate x, expressed in seconds. This
 * returns 0.0 if x is outside s. *)
let value s x =
    let x = finite_or_zero x in
    if 0.0 <= x && x <= s.duration then s.wave x else 0.0

(* Returns the factor of the sound s starting at time start in seconds and having as
 * duration duration in seconds. *)
let factor s start duration =
    assert (start >= 0.0);
    assert (start +. duration <= s.duration);
    {wave = (fun x -> s.wave (x +. start)); duration = duration}

(* Returns the sound of duration 0 having y as value at the origin. *)
let point y =
    {wave = Fun.const y; duration = 0.0}

(* Returns the sound whose wave is a segment connecting the point of abscissa 0 and ordinate
 * y1 to the point of abscissa 1.0 and ordinate y2. *)
let segment y1 y2 =
    let y1 = finite_or_zero y1 and y2 = finite_or_zero y2 in
    {wave = (fun x -> y1 +. x *. (y2 -. y1)); duration = 1.0}

(* Returns the sound obtained by performing point by point the binary operation op on the
 * waves of the sounds s1 and s2. *)
let pointwise_operation op s1 s2 =
    {wave = (fun x -> op (value s1 x) (value s2 x)); duration = max s1.duration s2.duration}

(* Returns the sound obtained by concatenating the sounds s1 and s2. *)
let concatenate s1 s2 =
    let wave x =
        if x <= s1.duration then s1.wave x else s2.wave (x -. s1.duration)
    in
    {wave = wave; duration = s1.duration +. s2.duration}

(* Returns the sound obtained by reversing the sound s. *)
let reverse s =
    {s with wave = (fun x -> s.wave (s.duration -. x))}

(* Returns the sound obtained by looping k times the sound s. *)
let rec loop k s =
    let k = finite_or_zero k in
    if s.duration = 0.0 then
        point (value s 0.0)
    else if k < 0.0 then
        loop (Float.abs k) (reverse s)
    else
        let wave x =
            s.wave (s.duration *. (fst (Float.modf (x /. s.duration))))
        in
        {wave = wave; duration = k *. s.duration}

(* Returns the sound obtained by stretching with a factor k the sound s. *)
let rec stretch k s =
    let k = finite_or_zero k in
    if k = 0.0 then
        point (value s 0.0)
    else if k < 0.0 then
        stretch (Float.abs k) (reverse s)
    else
        {wave = (fun x -> s.wave (x /. k)); duration = k *. s.duration}

