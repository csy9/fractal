[<AutoOpen>]
module Fractal.Utils

// Dimensions of the output image
let w = 3840
let h = 2160

// Maximum iteration depth
let iMax = 800

// Color palette size
let pSize = 500

// Bailout radius
let bRadius = 2.0

// Complex coordinate mappings
let cplxWidth = 0.04
let cplxHeight = 0.02
let cplxXShift = -0.85
let cplxYShift = 0.212

// RNG
let rand = System.Random ()

// Easy float arithmetic
let inline (./) x y = (float x) / (float y)
let inline (.*) x y = (float x) * (float y)

// Map function to a triple
let mapToTriple f (a, b, c) =
    f a, f b, f c

// Get third elem from triple
let thrd (a, b, c) = c