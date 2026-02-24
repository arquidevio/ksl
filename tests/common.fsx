module Common

#r "paket: nuget Expecto ~> 10"

open Expecto
open System.IO
open Yzl

let tmpDir () =
  let outDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
  Directory.CreateDirectory outDir |> ignore
  outDir

let inline prepareFile< ^a when (^a or Node): (static member ToYzl: ^a -> Node)> fileName (yzl: ^a) =
  let tmpDir = tmpDir ()
  let testOutputPath = Path.Combine(tmpDir, fileName)
  printfn "cat %s" testOutputPath
  (testOutputPath, yzl |> Yzl.render) |> File.WriteAllText
  testOutputPath, tmpDir
