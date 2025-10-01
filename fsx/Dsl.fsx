namespace Ksl

#r "paket: nuget Yzl ~> 2"

open System.Text.RegularExpressions

[<AutoOpen>]
module Types =

  module Utils =
    let (|ParseRegex|_|) regex str =
      let m = Regex(regex).Match str

      if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
      else
        None

  type AppParameters =
    {
      /// App's source code repository URL (Git SSH format).
      GitUrl: string
      /// App's logical path.
      AppPath: string
      /// App's logical path chunks.
      AppPathChunks: string list
      /// App template name.
      ///
      /// This parameter can be used to handle multiple app types. They could be different specs or various Kustomize base apps.
      Template: string
      /// App traits
      ///
      /// List of app features. They can be used as components to further customize apps.
      Features: string list
    }

  exception GitUrlInvalidFormat of url: string

  let gitPath (sourceRepoUrl: string) : string =
    match sourceRepoUrl with
    | Utils.ParseRegex "^git@(.*):(.*)\.git$" [ _; appPath ] -> appPath
    | _ -> raise (GitUrlInvalidFormat sourceRepoUrl)

  type AppDirPath = AppDirPath of string

  /// Conventions specify various parts of app manifest generation
  type Conventions =
    {
      /// Defines app name
      AppName: AppParameters -> string
      /// Defines app manifests directory path (relative to the manifests repo root)
      AppDirPath: AppParameters -> AppDirPath
      /// Defines app Docker image name
      AppImageName: AppParameters -> string
      /// Defines app namespace
      AppNamespace: AppParameters -> string
    }

  /// App context feeds app-related data to the manifest template
  type AppContext =
    {
      /// App name
      Name: string
      /// App parameters provided by the user
      Params: AppParameters
      /// App manifests directory path (relative to the manifests repo root) derived by convention
      Path: string
      /// App image name derived by convention
      ImageName: string
      /// App namespace derived by convention
      Namespace: string
    }

  type EncryptionContext =
    { CertPath: string; SecretName: string }

  type SecretContext =
    { EncryptWith: EncryptionContext -> Map<string, string>
      Keys: string list
      Environment: string }

  /// The current directory context.
  type DirContext =
    {
      /// <summary>Prepends the given path with the right number of "../" assuming the root dir as the base.</summary>
      /// <remarks>Has a corresponding operator - ../</remarks>
      RelativeToRoot: string -> string
      /// Prepends the given path with the right number of "../" assuming the first argument value as the base.
      RelativeTo: string -> string -> string
      /// The current working directory relative to the root
      Path: string
    }

  /// Prepends the given path with the right number of "../" assuming the manifest repo root dir as the base.
  let (../) (d: DirContext) (path: string) = d.RelativeToRoot path

  /// Prepends the given path with the right number of "../" assuming taking an explicit base path rather than root.
  let (../+) (d: DirContext) (basePath: string) (path: string) = d.RelativeTo basePath path

[<AutoOpen>]
module Conventions =
  open Types
  open System.Text.RegularExpressions

  type Conventions with
    member x.Apply(app: AppParameters) =
      { Name = app |> x.AppName
        Params = app
        Path =
          let (AppDirPath path) = app |> x.AppDirPath
          path
        ImageName = app |> x.AppImageName
        Namespace = app |> x.AppNamespace }

    static member Default =
      { AppName = fun app -> app.AppPath.Split "/" |> Array.last
        AppDirPath = fun app -> AppDirPath app.AppPath
        AppImageName = fun app -> gitPath app.GitUrl
        AppNamespace = fun app -> app.AppPath.Replace("/", "-") }

module Dsl =
  open Yzl

  type MoldSpec =
    | Many of MoldSpec list
    | Dir of path: string * MoldSpec list
    | DirWithContext of path: string * (DirContext -> MoldSpec list)
    | File of path: string * content: (unit -> string)
    | KustomizeResource of kustomizationPath: string * resourcePath: string
    | KustomizeGenerator of kustomizationPath: string * generatorPath: string
    | KustomizePatchFile of kustomizationPath: string * patchPath: string
    | KustomizeComponent of kustomizationPath: string * componentPath: string
    | KustomizeTransformer of kustomizationPath: string * transformerPath: string
    | KustomizeImage of kustomizationPath: string * name: string * newName: string * newTag: string
    | MergeYzl of filePath: string * func: (unit -> Node list)
    | MergeEnv of filePath: string * map: (string * string) list
    | NoYamlPath of filePath: string * jsonPointer: string
    | NoPath of filePath: string

[<AutoOpen>]
module Builder =

  module Mold =
    open Yzl
    open Dsl

    /// Ensures the root element for creating an app specification.
    let app specs = Many specs

    /// Ensures a directory.
    let dir (path: string) (spec: MoldSpec list) = Dir(path, spec)

    /// Ensures a directory providing the current dir context to the inner specifications.
    let dirC (path: string) (spec: DirContext -> MoldSpec list) = DirWithContext(path, spec)

    [<RequireQualifiedAccess>]
    module File =

      /// Ensures a file. The content is provided as a string list. The elements then are concatenated with EOL.
      let fromLines (path: string) (content: string list) =
        File(path, fun () -> content |> String.concat "\n")

      /// Ensures a file. The content string is directly rendered as provided.
      let fromString (path: string) (content: string) = File(path, fun () -> content)

      /// Ensures a file. It defers the invocation of the content function until the spec execution.
      ///
      /// Useful if any parameters need to be dynamically fed when the spec gets applied.
      let fromFunc (path: string) (content: unit -> string) = File(path, content)

      /// Ensures an empty file.
      let empty (path: string) = File(path, fun () -> "")

      /// Ensures a YAML file specified with Yzl.
      let fromYzl (path: string) (yaml: NamedNode list) = File(path, fun () -> Yzl.render yaml)

      /// Ensures a patch of an existing YAML file.
      let mergeYzl (path: string) (nodes: Node list) = MergeYzl(path, fun () -> nodes)

      /// Ensures a patch of an existing .env file.
      let mergeEnv (path: string) (keyValues: (string * string) list) = MergeEnv(path, keyValues)

      let noYamlPath (filePath: string) (jsonPointer: string) = NoYamlPath(filePath, jsonPointer)

    [<RequireQualifiedAccess>]
    module Kustomize =

      /// Ensures a "kustomization.yaml" file with the content provided as an Yzl spec in the current directory.
      let file (yaml: NamedNode list) =
        File("kustomization.yaml", fun () -> Yzl.render yaml)

      /// Ensures a resource in Kustomization file in the current directory.
      let resource path = KustomizeResource(".", path)

      /// Ensures a generator in Kustomization file in the current directory.
      let generator path = KustomizeGenerator(".", path)

      /// Ensures a patch file in Kustomization file in the current directory.
      let patchFile path = KustomizePatchFile(".", path)

      /// Ensures a component in Kustomization file in the current directory.
      let comp path = KustomizeComponent(".", path)

      /// Ensures a transformer in Kustomization file in the current directory.
      let transformer path = KustomizeTransformer(".", path)

      /// Ensures an image in Kustomization file in the current directory.
      let image name newName newTag =
        KustomizeImage(".", name, newName, newTag)
