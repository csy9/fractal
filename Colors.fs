module Fractal.Colors
open System.Drawing
open System.Numerics

(*
 * Convert HSL to RGB
 *)
let toRGB c =
    let normalize (h:float, s:float, l:float) =
        let v  = if l < 0.5 then l * (s+1.0) else l + s - (l*s)
        if v > 0.0 then
            let m = 2.0*l - v
            let sv = (v - m) / v
            let h = h * 6.0
            let sextant = int h
            let fract = h - float sextant
            let vsf = v * sv * fract
            let mid1 = m + vsf
            let mid2 = v - vsf

            match sextant with
            | 0 -> v, mid1, m
            | 1 -> mid2, v, m
            | 2 -> m, v, mid1
            | 3 -> m, mid2, v
            | 4 -> mid1, m, v
            | 5 -> v, m, mid2
            | _ -> 0.0, 0.0, 0.0
        else
            l, l, l
    normalize c |> mapToTriple ((.*) 255 >> int)

(*
 * Interpolate between two colors
 *)
let interpolate (c1: Color) (c2: Color) (f: float) =
    let unpack (c: Color) = [int c.R; int c.G; int c.B]
    let s = List.zip (unpack c1) (unpack c2)
    let bases = List.map (fun (a,b) -> min a b) s
    let distances = List.map (fun (a,b) -> int (abs (a - b) .* f)) s

    let color = List.zip bases distances |> List.map (fun (b,d) -> b+d)
    match color with
    | [r;g;b] -> (r, g, b)
    | _ -> (0, 0, 0)
    |> Color.FromArgb
    

// Array of colors to choose from
let colorPalette =
//    let hbase = rand.Next(1000) ./ 1000
    Array.init (pSize+2) (fun i -> i ./ (pSize+2))
    |> Array.map (fun n -> (n+0.6)%1.0, 0.8*n+0.1, n)
    |> Array.map (toRGB >> Color.FromArgb)

let mapColor (i:float) =
    let n = (floor >> int) (i ./ iMax .* pSize)
    interpolate colorPalette.[n] colorPalette.[n + 1] (i%1.0)

// Map an iteration value to a color
let mapColors s =
    Seq.map (fun (x, y, i) -> x, y, mapColor i) s