#load "common.fsx"
#load "../fsx/Render.fsx"

open Common
open Expecto
open Ksl.Kustomize
open System.IO
open Yzl

[ test "setImage - adds images section when none exists" {
    let testOutputPath, tmpDir =
      [ "resources" .= [ "deployment.yaml" ] ]
      |> prepareFile "kustomization.yaml"

    Kustomize.setImage tmpDir "myapp" (Some "myregistry/myapp") "v1.0"

    let actual = File.ReadAllText testOutputPath

    let expected =
      "resources:
- deployment.yaml
images:
- name: myapp
  newName: myregistry/myapp
  newTag: v1.0
"

    "Should create images section with new entry" |> Expect.equal actual expected
  }

  test "setImage - updates existing image entry" {
    let testOutputPath, tmpDir =
      [ "images"
        .= [ ![ "name" .= "myapp"; "newName" .= "myregistry/myapp"; "newTag" .= "v0.9" ] ] ]
      |> prepareFile "kustomization.yaml"

    Kustomize.setImage tmpDir "myapp" (Some "myregistry/myapp") "v1.0"

    let actual = File.ReadAllText testOutputPath

    let expected =
      "images:
- name: myapp
  newName: myregistry/myapp
  newTag: v1.0
"

    "Should update newTag of existing image entry" |> Expect.equal actual expected
  }

  test "setImage - appends entry when images exists but name not found" {
    let testOutputPath, tmpDir =
      [ "images" .= [ ![ "name" .= "otherapp"; "newTag" .= "v2.0" ] ] ]
      |> prepareFile "kustomization.yaml"

    Kustomize.setImage tmpDir "myapp" (Some "myregistry/myapp") "v1.0"

    let actual = File.ReadAllText testOutputPath

    let expected =
      "images:
- name: otherapp
  newTag: v2.0
- name: myapp
  newName: myregistry/myapp
  newTag: v1.0
"

    "Should append new image entry to existing list" |> Expect.equal actual expected
  } ]
|> testList "Kustomize Tests"
|> runTestsWithCLIArgs [] [||]
