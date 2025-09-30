#r "paket: nuget Expecto ~> 10"

#load "../fsx/Render.fsx"

open Expecto
open Ksl
open Ksl.Builder.Mold
open System.IO
open Yzl

let items = Yzl.seq
let item = Yzl.str


let testA =
    test "MergeYzl should use Yzl magic indentation" {


        let outDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())

        Directory.CreateDirectory outDir |> ignore

        let yaml =
            items
                [ [ item
                        !|-"""
                                 - one: A
                                   other: []
                               """ ] ]
            |> Yzl.render

        let testOutputPath = Path.Combine(outDir, "test.yaml")
        (testOutputPath, yaml) |> File.WriteAllText
        File.ReadAllText testOutputPath |> printfn "%s"

        dir
            "."
            [ File.mergeYzl "test.yaml"
              <| fun () ->
                  ![ items
                         [ [ item
                                 !|-"""
                                 - test: value
                                   some: []
                               """ ] ] ] ]
        |> RenderTo.fileSystem outDir

        let actual = File.ReadAllText testOutputPath

        let expected =
            """items:
- item: |-
    - one: A
      other: []
- item: |-
    - test: value
      some: []
"""

        "Should have correct content" |> Expect.equal actual expected

    }

runTestsWithCLIArgs [] [||] ([ testA ] |> testList "Tests")
