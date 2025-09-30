namespace Ksl.Kustomize

#r "paket:
      nuget Fake.Core.Process ~> 6
      nuget Yzl ~> 2"

#load "Yaml.fsx"

open Fake.Core

module Types =
    type KustomizePart =
        | Generator of path: string
        | Resource of path: string
        | Transformer of path: string
        | Component of path: string
        | PatchFile of path: string
        | Image of name: string * newName: string * newTag: string

    type Mod =
        | Add of part: KustomizePart * kustomizationDir: string
        | Remove of part: KustomizePart * kustomizationDir: string

    /// Add part to a Kustomization file - can be consumed by Kustomize.modify
    let (+>>) (part: KustomizePart) (kustomizationDir: string) =
        Add(part, kustomizationDir), sprintf "Kustomization %s --> +%A" kustomizationDir part

    /// Remove part from a Kustomiation file - can be consumed by Kustomize.modify
    let (->>) (part: KustomizePart) (kustomizationDir: string) =
        Remove(part, kustomizationDir), sprintf "Kustomization %s --> -%A" kustomizationDir part

[<RequireQualifiedAccess>]
module Kustomize =
    open Yzl
    open Ksl.Yaml
    open Types

    let private create workingDir args =
        RawCommand("kustomize", Arguments.OfArgs args)
        |> CreateProcess.fromCommand
        |> CreateProcess.withWorkingDirectory workingDir

    let private runKustomize workingDir args =
        create workingDir args |> Proc.run |> ignore

    let private runKustomizeWithResult workingDir args =
        let result =
            create workingDir args
            |> CreateProcess.redirectOutput
            |> CreateProcess.ensureExitCode
            |> Proc.run

        result.Result.Output

    let setImage workingDir name (newName: string option) newTag =
        runKustomize
            workingDir
            [ "edit"
              "set"
              "image"
              sprintf "%s=%s:%s" name (newName |> Option.defaultValue "*") newTag ]

    let addResource workingDir resourcePath =
        runKustomize workingDir [ "edit"; "add"; "resource"; resourcePath ]

    let removeResource workingDir resourcePath =
        runKustomize workingDir [ "edit"; "remove"; "resource"; resourcePath ]

    let addGenerator workingDir (generatorPath: string) =
        let kpath = workingDir + "/" + "kustomization.yaml"
        kpath |> Yaml.EditInPlace2 ![ "generators" .= [ generatorPath ] ]

    let addComponent workingDir (componentPath: string) =
        let kpath = workingDir + "/" + "kustomization.yaml"
        kpath |> Yaml.EditInPlace2 ![ "components" .= [ componentPath ] ]

    let addTransformer workingDir (transformerPath: string) =
        let kpath = workingDir + "/" + "kustomization.yaml"
        kpath |> Yaml.EditInPlace2 ![ "transformers" .= [ transformerPath ] ]

    let addPatchFile workingDir (patchPath: string) =
        let kpath = workingDir + "/" + "kustomization.yaml"
        kpath |> Yaml.EditInPlace2 ![ "patches" .= [ ![ "path" .= patchPath ] ] ]

    let modify (spec: Mod) =
        match spec with
        | Add(Component path, dir) -> addComponent dir path
        | Add(Generator path, dir) -> addGenerator dir path
        | Add(Resource path, dir) -> addResource dir path
        | Remove(Resource path, dir) -> removeResource dir path
        | Add(PatchFile path, dir) -> addPatchFile dir path
        | Add(Transformer path, dir) -> addTransformer dir path
        | Add(Image(name, newName, newTag), dir) -> setImage dir name (Some(newName)) newTag
        | x -> failwithf $"Kustomize: operation {x} is not supported"

    let fix workingDir =
        runKustomize workingDir [ "edit"; "fix"; "--vars" ]

    let build workingDir =
        runKustomizeWithResult workingDir [ "build" ]
