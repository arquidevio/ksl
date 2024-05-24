namespace Ksl

#load "Dsl.fsx"
#load "Yaml.fsx"
#load "Kustomize.fsx"

open System.IO
open Ksl.Yaml
open Ksl.Kustomize
open Ksl.Kustomize.Types
open Ksl.Dsl
open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module RenderTo =

  let fileSystem (targetDir:string) m =
    let rec exec m pwd =
      
      let fullPath (path:string) = if Path.IsPathRooted(path) then path else pwd + "/" + path

      let edit (spec, msg) = 
        printfn $"%s{msg}"
        Kustomize.modify spec

      let ensureDir fullP =
         if (not (Directory.Exists(fullP)))
         then
           printfn $"Dir --> %s{fullP}"
           Directory.CreateDirectory(fullP) |> ignore

      let getCwdFullPath path =
        match pwd with
        | "" | "." -> path
        | pwd -> pwd + "/" + path

      match m with
      | Many ms -> ms |> Seq.iter (fun m -> exec m pwd)
      | File (path, content) ->
         let fullP = fullPath path
         
         if (not (IO.File.Exists(fullP)))
         then
           printf $"File --> %s{fullP} "
           IO.File.WriteAllText(fullP, content ())
           let fileInfo = FileInfo(fullP)
           if fileInfo.Name = "kustomization.yaml"
           then Kustomize.fix fileInfo.DirectoryName
           printfn "OK"

      | Dir (path, ms) ->
         let fullP = getCwdFullPath path
         ensureDir fullP
         ms |> Seq.iter(fun x -> exec x fullP)
      | DirWithContext (path, ms) ->
         let fullP = getCwdFullPath path
         let pathInRepo = Path.GetRelativePath(targetDir, fullP)
         ensureDir fullP
         ms {
           Path = pathInRepo
           RelativeToRoot = fun p -> Path.GetRelativePath(pathInRepo, p)
           RelativeTo = fun basePath p -> Path.Combine(Path.GetRelativePath(pathInRepo, basePath), p)
         } |> Seq.iter(fun x -> exec x fullP)
      | NoPath path -> 
        if DirectoryInfo(path).Exists then Directory.Delete(path, true)
        else if FileInfo(path).Exists then IO.File.Delete(path)
      
      | KustomizeResource (dir, resourcePath) -> Resource resourcePath +>> fullPath dir |> edit
      | NoKustomizeResource (dir, resourcePath) -> Resource resourcePath ->> fullPath dir |> edit
      | KustomizeGenerator (dir, generatorPath) -> Generator generatorPath +>> fullPath dir |> edit
      | KustomizePatchFile (dir, patchPath) -> PatchFile patchPath +>> fullPath dir |> edit
      | KustomizeComponent (dir, componentPath) -> Component componentPath +>> fullPath dir |> edit
      | KustomizeTransformer (dir, transformerPath) -> Transformer transformerPath +>> fullPath dir |> edit
      | KustomizeImage (dir, name, newName, newTag) -> Image(name, newName, newTag) +>> fullPath dir |> edit

      | MergeYzl (path, func) ->
        let fullP = fullPath path
        printfn $"YamlMerge --> %s{fullP}"
        fullP |> Yaml.EditInPlace2 (func ())

      | MergeEnv (path, map) ->
        let fullP = fullPath path
        printfn $"EnvMerge --> %s{fullP}"

        let vars = 
          fullP |> IO.File.ReadAllLines
                |> Seq.map (fun x -> 
                     x.Split('=', 2, StringSplitOptions.RemoveEmptyEntries &&& StringSplitOptions.TrimEntries)
                     |> Seq.toList
                     |> function | [a; b] -> (a,b) | [a] -> (a,"") |_ -> ("","")
                     |> KeyValuePair)
                |> Dictionary<string,string>

        map |> Seq.iter (fun (k ,v) -> vars.[k] <- v)
        let output = vars |> Seq.map (fun (KeyValue(k,v)) -> $"{k}={v}")
        IO.File.WriteAllLines (fullP, output)
    exec m targetDir
