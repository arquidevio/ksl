namespace Ksl.Yaml

#r "paket: 
      nuget Yzl ~> 2
      nuget YamlDotNet ~> 16"

open System.IO
open YamlDotNet.RepresentationModel
open YamlDotNet.Serialization
open YamlDotNet.Core
open Yzl
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Yaml =
  open System.Text.RegularExpressions

  let private withStyle (style: ScalarStyle) (node: YamlScalarNode) =
    node.Style <- style
    node

  /// Converts Yzl tree into YamlDotNet
  let fromYzl (sourceStart: Mark) (node: Node) =
    let rec traverse =
      function
      | MapNode xs ->
        YamlMappingNode(
          seq {
            for Named(Name name, x) in xs do
              yield (YamlScalarNode name, traverse x) |> KeyValuePair<YamlNode, YamlNode>
          }
        )
        :> YamlNode
      | SeqNode xs ->
        YamlSequenceNode(
          seq {
            for x in xs do
              yield traverse x
          }
        )
        :> YamlNode
      | Scalar s ->
        let scalar =
          match s with
          | Bool x -> x |> string |> YamlScalarNode
          | Int x -> x |> string |> YamlScalarNode
          | Float x -> x |> string |> YamlScalarNode
          | Str s ->
            let format (p: string) =
              let indent = String.replicate (sourceStart.Column |> int) "  "

              p.Trim().Split "\n"
              |> Seq.mapi (fun i x ->
                match i, x with
                | 0, x -> x
                | i, x -> $"{indent}{x.Trim()}")
              |> String.concat "\n"

            match s with
            | Plain p -> p |> format |> YamlScalarNode
            | FoldedStrip p -> p |> format |> YamlScalarNode |> withStyle ScalarStyle.Folded
            | FoldedKeep p -> p |> format |> YamlScalarNode |> withStyle ScalarStyle.Folded
            | Folded p -> p |> format |> YamlScalarNode |> withStyle ScalarStyle.Folded
            | Literal p -> p |> format |> YamlScalarNode |> withStyle ScalarStyle.Literal
            | LiteralKeep p -> p |> format |> YamlScalarNode |> withStyle ScalarStyle.Literal
            | LiteralStrip p -> p |> format |> YamlScalarNode |> withStyle ScalarStyle.Literal
            | DoubleQuoted p -> p |> format |> YamlScalarNode |> withStyle ScalarStyle.DoubleQuoted
            | SingleQuoted p -> p |> format |> YamlScalarNode |> withStyle ScalarStyle.SingleQuoted

        scalar :> YamlNode
      | _ -> failwithf "Do not know"

    traverse node

  let (|MapNode|SeqNode|ScalarNode|) (node: YamlNode) =
    match node with
    | :? YamlMappingNode as x -> MapNode x
    | :? YamlSequenceNode as x -> SeqNode x
    | :? YamlScalarNode as x -> ScalarNode x
    | _ -> failwithf "Node %s is not supported" (node.NodeType |> string)

  let mergeFromYzl (source: Node) (target: YamlNode) =

    let kv (node: NamedNode) =
      let (Named(Name key, value)) = node
      YamlScalarNode key, value

    let map =
      function
      | Yzl.Core.MapNode _ -> YamlMappingNode() :> YamlNode
      | Yzl.Core.SeqNode _ -> YamlSequenceNode() :> YamlNode
      | Yzl.Core.Scalar _ -> YamlScalarNode() :> YamlNode
      | NoNode as z -> failwithf "Not supported %A" z

    let rec traverse (source: Node) (target: YamlNode) (parentMapWithKey: (YamlScalarNode * YamlMappingNode) option) =
      match source, target with
      | Yzl.Core.MapNode xs, MapNode m ->
        xs
        |> Seq.map kv
        |> Seq.iter (fun (k, v) ->
          let newOrExisting =
            if not <| m.Children.ContainsKey k then
              v |> map
            else
              m.Children.[k]

          m.Children.[k] <- traverse v newOrExisting (Some(k, m)))

        m :> YamlNode
      | Yzl.Core.MapNode xs, _ ->
        let m = YamlMappingNode()

        xs
        |> Seq.map kv
        |> Seq.iter (fun (k, v) ->
          let newOrExisting =
            if not <| m.Children.ContainsKey k then
              v |> map
            else
              m.Children.[k]

          m.Children.[k] <- traverse v newOrExisting (Some(k, m)))

        m :> YamlNode
      | Yzl.Core.SeqNode xs, SeqNode s ->
        xs
        |> Seq.iter (fun n ->
          let xn = n |> fromYzl target.Start

          if not <| s.Children.Contains xn then
            s.Children.Add xn
          else
            parentMapWithKey |> Option.iter (fun (k, m) -> m.Children.[k] <- xn))

        s :> YamlNode
      | Yzl.Core.Scalar _ as z, ScalarNode _ -> z |> fromYzl target.Start
      | Yzl.Core.Scalar _ as z, null -> z |> fromYzl target.Start
      | a, b -> failwithf "Merge failed. Node type mismatch: %A vs %A" a b

    traverse source target None

  let loadFile (path: string) =
    use file = File.OpenRead path
    use reader = new StreamReader(file)
    let yaml = YamlStream()
    yaml.Load reader
    yaml

  let private serializer = SerializerBuilder().Build()

  let saveFile (path: string) (yaml: YamlStream) =
    use file = File.Open(path, FileMode.Create)
    use writer = new StreamWriter(file)
    serializer.Serialize(writer, yaml.Documents.[0].RootNode)

  let editInPlace (nodes: Node list) (filePath: string) =
    let stream = loadFile filePath

    for n in nodes do
      stream.Documents.[0].RootNode |> mergeFromYzl n |> ignore

    stream |> saveFile filePath

  let removeNode (filePath: string) (jsonPaths: string list) =
    let stream = loadFile filePath
    let doc = stream.Documents.[0]

    let parseSegment (seg: string) =
      // Match [key=value] or [key="value"] or [value] (scalar match)
      let predicateMatch =
        Regex.Match(seg, @"^\[([^=]+)=(?:""([^""]*)""|'([^']*)'|([^\]]+))\]$")

      let scalarMatch = Regex.Match(seg, @"^\[(?:""([^""]*)""|'([^']*)'|([^\]]+))\]$")

      if predicateMatch.Success then
        let key = predicateMatch.Groups.[1].Value

        let value =
          if predicateMatch.Groups.[2].Success then
            predicateMatch.Groups.[2].Value
          elif predicateMatch.Groups.[3].Success then
            predicateMatch.Groups.[3].Value
          else
            predicateMatch.Groups.[4].Value

        None, Some(key, value)
      elif scalarMatch.Success then
        // Scalar match - no key, just value
        let value =
          if scalarMatch.Groups.[1].Success then
            scalarMatch.Groups.[1].Value
          elif scalarMatch.Groups.[2].Success then
            scalarMatch.Groups.[2].Value
          else
            scalarMatch.Groups.[3].Value

        None, Some(null, value) // null key indicates scalar match
      else
        Some seg, None

    let rec navigate (node: YamlNode) segments =
      match segments with
      | [] -> failwith "Empty path"
      | [ None, Some(predKey, predVal) ] ->
        // Just a predicate means we're in a sequence already
        match node with
        | :? YamlSequenceNode as sequence ->
          let idx =
            sequence.Children
            |> Seq.findIndex (fun child ->
              match child with
              | :? YamlMappingNode as m when predKey <> null ->
                // Object predicate
                match m.Children.TryGetValue(YamlScalarNode predKey) with
                | true, v when (v :?> YamlScalarNode).Value = predVal -> true
                | _ -> false
              | :? YamlScalarNode as s when predKey = null ->
                // Scalar predicate
                s.Value = predVal
              | _ -> false)

          sequence.Children.RemoveAt idx
        | _ -> failwith "Predicate requires sequence"
      | (Some key, None) :: rest ->
        if List.isEmpty rest then
          // Last segment - try to remove
          match node with
          | :? YamlMappingNode as mapping -> mapping.Children.Remove(YamlScalarNode key) |> ignore
          | :? YamlSequenceNode as sequence -> sequence.Children.RemoveAt(int key)
          | _ -> failwithf "Cannot remove from %s" (node.GetType().Name)
        else
          // Keep navigating
          let next =
            match node with
            | :? YamlMappingNode as mapping -> mapping.Children.[YamlScalarNode key]
            | :? YamlSequenceNode as sequence -> sequence.Children.[int key]
            | _ -> failwith "Invalid path"

          navigate next rest
      | (None, Some(predKey, predVal)) :: rest when predKey = null ->
        // Scalar sequence matching
        if List.isEmpty rest then
          // Last segment - remove the scalar
          match node with
          | :? YamlSequenceNode as sequence ->
            // Debug: see what's actually in the sequence
            sequence.Children
            |> Seq.iter (fun c ->
              printfn
                "Child type: %s, value: %A"
                (c.GetType().Name)
                (match c with
                 | :? YamlScalarNode as s -> s.Value
                 | _ -> "not scalar"))

            let idx =
              sequence.Children
              |> Seq.findIndex (fun child ->
                match child with
                | :? YamlScalarNode as s when s.Value = predVal -> true
                | _ -> false)

            sequence.Children.RemoveAt idx
          | _ -> failwith "Scalar predicate requires sequence"
        else
          // Navigate to matching scalar
          let next =
            match node with
            | :? YamlSequenceNode as sequence ->
              sequence.Children
              |> Seq.find (fun child ->
                match child with
                | :? YamlScalarNode as s when s.Value = predVal -> true
                | _ -> false)
            | _ -> failwith "Scalar predicate requires sequence"

          navigate next rest

      | (None, Some(predKey, predVal)) :: rest ->
        // Find matching item in current sequence
        let next =
          match node with
          | :? YamlSequenceNode as sequence ->
            sequence.Children
            |> Seq.find (fun child ->
              match child with
              | :? YamlMappingNode as m ->
                match m.Children.TryGetValue(YamlScalarNode predKey) with
                | true, v when (v :?> YamlScalarNode).Value = predVal -> true
                | _ -> false
              | _ -> false)
          | _ -> failwith "Predicate requires sequence"

        navigate next rest
      | (Some key, Some(predKey, predVal)) :: rest ->
        // Key followed by predicate - key should point to sequence
        let sequence =
          match node with
          | :? YamlMappingNode as mapping -> mapping.Children.[YamlScalarNode key] :?> YamlSequenceNode
          | _ -> failwith "Expected mapping"

        let next =
          sequence.Children
          |> Seq.find (fun child ->
            match child with
            | :? YamlMappingNode as m ->
              match m.Children.TryGetValue(YamlScalarNode predKey) with
              | true, v when (v :?> YamlScalarNode).Value = predVal -> true
              | _ -> false
            | _ -> false)

        navigate next rest
      | (None, None) :: _ -> failwithf "Unreachable"

    for jp in jsonPaths do
      let segments =
        Regex.Split(jp, @"\.(?![^\[]*\])")
        |> Array.filter (fun s -> s <> "")
        |> Array.map parseSegment
        |> Array.toList

      navigate doc.RootNode segments

    saveFile filePath stream
