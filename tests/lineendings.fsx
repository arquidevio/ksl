#load "common.fsx"
#load "../fsx/Render.fsx"

open Common
open Expecto
open Ksl
open Ksl.Builder.Mold
open System.IO
open Yzl

let hasNoCarriageReturn (filePath: string) =
  File.ReadAllBytes(filePath) |> Array.contains (byte '\r') |> not

[ test "File.fromString - normalizes CRLF input to LF" {
    let tmpDir = tmpDir ()
    let filePath = Path.Combine(tmpDir, "file.txt")

    dir "." [ File.fromString filePath "line1\r\nline2\r\nline3\r\n" ]
    |> RenderTo.fileSystem tmpDir

    "Should not contain carriage return bytes"
    |> Expect.isTrue (hasNoCarriageReturn filePath)
  }

  test "File.fromString - preserves LF endings" {
    let tmpDir = tmpDir ()
    let filePath = Path.Combine(tmpDir, "file.txt")

    dir "." [ File.fromString filePath "line1\nline2\nline3\n" ]
    |> RenderTo.fileSystem tmpDir

    "Should not contain carriage return bytes"
    |> Expect.isTrue (hasNoCarriageReturn filePath)
  }

  test "File.fromLines - produces LF endings" {
    let tmpDir = tmpDir ()
    let filePath = Path.Combine(tmpDir, "file.txt")

    dir "." [ File.fromLines filePath [ "line1"; "line2"; "line3" ] ]
    |> RenderTo.fileSystem tmpDir

    "Should not contain carriage return bytes"
    |> Expect.isTrue (hasNoCarriageReturn filePath)
  }

  test "File.fromYzl - produces LF endings" {
    let tmpDir = tmpDir ()
    let filePath = Path.Combine(tmpDir, "config.yaml")

    dir "." [ File.fromYzl filePath [ "name" .= "myapp"; "version" .= "1.0" ] ]
    |> RenderTo.fileSystem tmpDir

    "Should not contain carriage return bytes"
    |> Expect.isTrue (hasNoCarriageReturn filePath)
  }

  test "File.env - produces LF endings" {
    let tmpDir = tmpDir ()
    let filePath = Path.Combine(tmpDir, ".env")

    dir "." [ File.env filePath [ "FOO", "bar"; "BAZ", "qux" ] ]
    |> RenderTo.fileSystem tmpDir

    "Should not contain carriage return bytes"
    |> Expect.isTrue (hasNoCarriageReturn filePath)
  }

  test "File.env - empty list creates file with no carriage returns" {
    let tmpDir = tmpDir ()
    let filePath = Path.Combine(tmpDir, ".env")

    dir "." [ File.env filePath [] ]
    |> RenderTo.fileSystem tmpDir

    "Should not contain carriage return bytes"
    |> Expect.isTrue (hasNoCarriageReturn filePath)
  }

  test "File.mergeYzl - produces LF endings after merge" {
    let testOutputPath, tmpDir =
      [ "name" .= "original"; "value" .= 123 ] |> prepareFile "test.yaml"

    dir "." [ File.mergeYzl testOutputPath [ ![ "extra" .= "added" ] ] ]
    |> RenderTo.fileSystem tmpDir

    "Should not contain carriage return bytes after merge"
    |> Expect.isTrue (hasNoCarriageReturn testOutputPath)
  }

  test "File.empty - creates an empty file with no carriage returns" {
    let tmpDir = tmpDir ()
    let filePath = Path.Combine(tmpDir, "placeholder")

    dir "." [ File.empty filePath ]
    |> RenderTo.fileSystem tmpDir

    "Should not contain carriage return bytes"
    |> Expect.isTrue (hasNoCarriageReturn filePath)
  }

  test "File.mergeEnv - produces LF endings after env merge" {
    let tmpDir = tmpDir ()
    let filePath = Path.Combine(tmpDir, ".env")

    dir "." [ File.env filePath [ "FOO", "original"; "BAR", "keep" ] ]
    |> RenderTo.fileSystem tmpDir

    dir "." [ File.mergeEnv filePath [ "FOO", "updated" ] ]
    |> RenderTo.fileSystem tmpDir

    "Should not contain carriage return bytes after env merge"
    |> Expect.isTrue (hasNoCarriageReturn filePath)
  }

  test "File.noYamlPaths - produces LF endings after node removal" {
    let testOutputPath, tmpDir =
      [ "name" .= "test"; "debug" .= true; "version" .= "1.0" ]
      |> prepareFile "test.yaml"

    dir "." [ [ ".debug" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    "Should not contain carriage return bytes after node removal"
    |> Expect.isTrue (hasNoCarriageReturn testOutputPath)
  } ]
|> testList "Line Endings Tests"
|> runTestsWithCLIArgs [] [||]
|> exit
