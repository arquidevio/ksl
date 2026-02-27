#load "common.fsx"
#load "../fsx/Render.fsx"

open Common
open Expecto
open Ksl
open Ksl.Builder.Mold
open System.IO

[ test "CreateEnv - creates file when it does not exist" {
    let tmpDir = tmpDir ()
    let filePath = Path.Combine(tmpDir, ".env")

    dir "." [ File.env filePath [ "FOO", "bar"; "BAZ", "qux" ] ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllLines filePath

    "Should create file with key=value lines"
    |> Expect.containsAll actual [ "FOO=bar"; "BAZ=qux" ]
  }

  test "CreateEnv - retains insertion order" {
    let tmpDir = tmpDir ()
    let filePath = Path.Combine(tmpDir, ".env")

    dir "." [ File.env filePath [ "Z_KEY", "last"; "A_KEY", "first"; "M_KEY", "middle" ] ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllLines filePath

    "Should write keys in insertion order"
    |> Expect.equal actual [| "Z_KEY=last"; "A_KEY=first"; "M_KEY=middle" |]
  }

  test "CreateEnv - does not overwrite existing file" {
    let tmpDir = tmpDir ()
    let filePath = Path.Combine(tmpDir, ".env")
    File.WriteAllText(filePath, "EXISTING=value\n")

    dir "." [ File.env filePath [ "NEW_KEY", "new_value" ] ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText filePath

    "Should leave existing file unchanged"
    |> Expect.equal actual "EXISTING=value\n"
  }

  test "CreateEnv - handles empty map" {
    let tmpDir = tmpDir ()
    let filePath = Path.Combine(tmpDir, ".env")

    dir "." [ File.env filePath [] ]
    |> RenderTo.fileSystem tmpDir

    "Should create the file"
    |> Expect.isTrue (File.Exists filePath)

    let actual = File.ReadAllLines filePath

    "Should create empty file" |> Expect.isEmpty actual
  } ]
|> testList "Env Tests"
|> runTestsWithCLIArgs [] [||]
