module Fractal.Generator
open System.Numerics


(*
 * Partition the image vertically
 * into n pieces (create the ith piece)
 *)
let coordGen (n:int) (i:int) = 
    i*(w/n), ((i+1)*(w/n))-1, 0, h-1, i ./ n

(*
 * Transform the cartesian coordinates
 * into a point on the complex plane,
 * while accountint for the fact that
 * the x dimension will need to be scaled back
 * since the image is rendered in pieces
 *)
let mapPx (x:int) = (x .* cplxWidth ./ w) + cplxXShift
let mapPy (y:int) = (y .* cplxHeight ./ h) - cplxYShift
let transform (x':float) (x:int, y:int) = x - int (x' .* w), y, Complex (mapPx x, mapPy y)

let phi i z =
    if i = iMax then 
        0.0
    else
        let logzn = (Complex.Abs >> log) z
        let nu = log (logzn / (log 2.0)) / log 2.0
        float i + 1.0 - nu

(* 
 * Iterate through the given coordinates,
 * returning a list of tuples containing
 * the coordinates and the escape iteration.
 *)
let fractalGen (x0:int, x1:int, y0:int, y1:int, x':float) = 
    let inline poly (z:Complex) (c:Complex) = 
        z*z + c
    let testEscape (x, y, c) =
        let rec escape z i =
            if (Complex.Abs z < bRadius && i < iMax) then
                escape (poly z c) (i+1)
            else
                phi i z
        x, y, (escape Complex.Zero 0)

    seq { for x in x0..x1 do for y in y0..y1 -> x, y }
        |> Seq.map (transform x' >> testEscape)