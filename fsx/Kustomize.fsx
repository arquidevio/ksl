namespace Ksl.Kustomize

#r "paket: nuget Yzl ~> 2"

#load "Yaml.fsx"

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

  let private kustomization rootDir = rootDir + "/" + "kustomization.yaml"

  let setImage workingDir name (newName: string option) (newTag: string) =
    let kpath = workingDir |> kustomization
    let imageFields =
      [ yield "name" .= name
        match newName with
        | Some n -> yield "newName" .= n
        | None -> ()
        yield "newTag" .= newTag ]
    let imageNode = !imageFields
    try
      kpath |> Yaml.editInPlaceAtPath imageNode $"images.[name={name}]"
    with _ ->
      kpath |> Yaml.editInPlace [ ![ "images" .= [ imageNode ] ] ]

  let addResource workingDir (resourcePath: string) =
    workingDir
    |> kustomization
    |> Yaml.editInPlace [ ![ "resources" .= [ resourcePath ] ] ]

  let removeResource workingDir resourcePath =
    let kpath = workingDir |> kustomization
    Yaml.removeNode kpath $"resources.[{resourcePath}]" |> ignore

  let addGenerator workingDir (generatorPath: string) =
    workingDir
    |> kustomization
    |> Yaml.editInPlace [ ![ "generators" .= [ generatorPath ] ] ]

  let addComponent workingDir (componentPath: string) =
    workingDir
    |> kustomization
    |> Yaml.editInPlace [ ![ "components" .= [ componentPath ] ] ]

  let addTransformer workingDir (transformerPath: string) =
    workingDir
    |> kustomization
    |> Yaml.editInPlace [ ![ "transformers" .= [ transformerPath ] ] ]

  let addPatchFile workingDir (patchPath: string) =
    workingDir
    |> kustomization
    |> Yaml.editInPlace [ ![ "patches" .= [ ![ "path" .= patchPath ] ] ] ]

  let modify (spec: Mod) =
    match spec with
    | Add(Component path, dir) -> addComponent dir path
    | Add(Generator path, dir) -> addGenerator dir path
    | Add(Resource path, dir) -> addResource dir path
    | Remove(Resource path, dir) -> removeResource dir path
    | Add(PatchFile path, dir) -> addPatchFile dir path
    | Add(Transformer path, dir) -> addTransformer dir path
    | Add(Image(name, newName, newTag), dir) -> setImage dir name (Some newName) newTag
    | x -> failwithf $"Kustomize: operation {x} is not supported"
