module Fractal.Main
open System
open System.Drawing
open Generator
open Colors

(*
 * Function to map coordinate to bmp-ready data
 *)
let generateImg = (fractalGen >> mapColors)

(*
 * Save data to the supplied bitmap
 *)
let writeData (data, bmp:Bitmap) =
    Seq.iter bmp.SetPixel data
    bmp

(*
 * Process and save the given bitmap, coords tuple
 *)
let processSection (data, bmp) =
    ((data |> generateImg), bmp) |> writeData

(*
 * Combine two bitmap images, side by side
 *)
let combineBitmaps (a:Bitmap) (b:Bitmap) =
    let c = new Bitmap(a.Width + b.Width, a.Height, Imaging.PixelFormat.Format24bppRgb)
    use g = Graphics.FromImage c
    g.DrawImage (a, 0, 0)
    g.DrawImage (b, a.Width, 0)
    c

(*
 * Prepare n bmp images
 *)
let generateBmps n =
    Seq.init n (fun _ -> new Bitmap(w/n, h, Imaging.PixelFormat.Format24bppRgb))

(*
 * Spin up n threads to create pieces of the final image
 *)
let threadGen (n:int) =
    generateBmps n
    |> Seq.zip (Seq.init n (coordGen n))
    |> Seq.map (fun s -> async { return processSection s })
    |> Async.Parallel
    
[<EntryPoint>]
let main argv =
    let stopWatch = Diagnostics.Stopwatch.StartNew()

    printfn "Rendering fractal..."
    let img =
        threadGen 16
        |> Async.RunSynchronously
        |> Seq.reduce combineBitmaps

    printfn "Saving image..."
    img.Save (@"C:\Users\casey.oneill\Desktop\fractal.png",
        Imaging.ImageFormat.Png)

    printfn "Save complete.\nTotal run time: %f." 
        stopWatch.Elapsed.TotalSeconds
    Console.ReadKey () |>  ignore
    0