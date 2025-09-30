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

    let private withStyle (style: ScalarStyle) (node: YamlScalarNode) =
        node.Style <- style
        node

    /// Converts Yzl tree into YamlDotNet
    let FromYzl (sourceStart: Mark) (node: Node) =
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

    let MergeFromYzl (source: Node) (target: YamlNode) =

        let kv (node: NamedNode) =
            let (Named(Name key, value)) = node
            YamlScalarNode key, value

        let map =
            function
            | Yzl.Core.MapNode _ -> YamlMappingNode() :> YamlNode
            | Yzl.Core.SeqNode _ -> YamlSequenceNode() :> YamlNode
            | Yzl.Core.Scalar _ -> YamlScalarNode() :> YamlNode
            | NoNode as z -> failwithf "Not supported %A" z

        let rec traverse
            (source: Node)
            (target: YamlNode)
            (parentMapWithKey: (YamlScalarNode * YamlMappingNode) option)
            =
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
                    let xn = n |> FromYzl target.Start

                    if not <| s.Children.Contains xn then
                        s.Children.Add xn
                    else
                        parentMapWithKey |> Option.iter (fun (k, m) -> m.Children.[k] <- xn))

                s :> YamlNode
            | Yzl.Core.Scalar _ as z, ScalarNode _ -> z |> FromYzl target.Start
            | Yzl.Core.Scalar _ as z, null -> z |> FromYzl target.Start
            | a, b -> failwithf "Merge failed. Node type mismatch: %A vs %A" a b

        traverse source target None

    let LoadFile (path: string) =
        use file = File.OpenRead path
        use reader = new StreamReader(file)
        let yaml = YamlStream()
        yaml.Load reader
        yaml

    let private serializer = SerializerBuilder().Build()

    let SaveFile (path: string) (yaml: YamlStream) =
        use file = File.Open(path, FileMode.Create)
        use writer = new StreamWriter(file)
        serializer.Serialize(writer, yaml.Documents.[0].RootNode)

    let EditInPlace2 (func: Node) (filePath: string) =
        let stream = LoadFile filePath
        stream.Documents.[0].RootNode |> MergeFromYzl func |> ignore
        stream |> SaveFile filePath
