#load "common.fsx"
#load "../fsx/Render.fsx"

open Common
open Expecto
open Ksl
open Ksl.Builder.Mold
open System.IO
open Yzl

let items = Yzl.seq
let item = Yzl.str


[ test "MergeYzl should use Yzl magic indentation" {

    let testOutputPath, tmpDir =
      items
        [ [ item
              !|-"
               - one: A
                 other: []
               " ] ]
      |> prepareFile "test.yaml"

    dir
      "."
      [ File.mergeYzl
          "test.yaml"
          [ ![ items
                 [ [ item
                       !|-"
                    - test: value
                      some: []
                    " ] ] ] ] ]
    |> RenderTo.fileSystem tmpDir

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

  test "Remove node - simple key" {
    let testOutputPath, tmpDir =
      items [ [ "name" .= "test"; "value" .= "123" ] ] |> prepareFile "test.yaml"

    dir "." [ [ "items.0.value" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "items:
- name: test
"

    "Should remove value field" |> Expect.equal actual expected
  }

  test "Remove node - by index" {
    let testOutputPath, tmpDir =
      items [ [ "name" .= "first" ]; [ "name" .= "second" ]; [ "name" .= "third" ] ]
      |> prepareFile "test.yaml"

    dir "." [ [ "items.1" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "items:
- name: first
- name: third
"

    "Should remove second item" |> Expect.equal actual expected
  }

  test "Remove node - predicate with multiple matches" {
    let testOutputPath, tmpDir =
      items
        [ [ "name" .= "prod"; "port" .= "8080" ]
          [ "name" .= "dev"; "port" .= "3000" ] ]
      |> prepareFile "test.yaml"

    dir "." [ [ "items.[name=dev]" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "items:
- name: prod
  port: 8080
"

    "Should remove dev item" |> Expect.equal actual expected
  }

  test "Remove node - nested path" {
    let testOutputPath, tmpDir =
      [ "database"
        .= [ "host" .= "localhost"; "port" .= "5432"; "password" .= "secret" ] ]
      |> prepareFile "test.yaml"

    dir "." [ [ "database.password" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "database:
  host: localhost
  port: 5432
"

    "Should remove password" |> Expect.equal actual expected
  }

  test "Remove node - predicate mid-path" {
    let testOutputPath, tmpDir =
      [ "servers"
        .= [ [ "name" .= "prod"; "config" .= [ "timeout" .= "30"; "retries" .= "3" ] ]
             [ "name" .= "dev"; "config" .= [ "timeout" .= "10"; "retries" .= "1" ] ] ] ]
      |> prepareFile "test.yaml"

    dir "." [ [ "servers.[name=prod].config.retries" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "servers:
- name: prod
  config:
    timeout: 30
- name: dev
  config:
    timeout: 10
    retries: 1
"

    "Should remove retries from prod only" |> Expect.equal actual expected
  }

  test "Remove node - top level key" {
    let testOutputPath, tmpDir =
      [ "version" .= "1.0"; "name" .= "myapp"; "debug" .= "true" ]
      |> prepareFile "test.yaml"

    dir "." [ [ ".debug" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "version: 1.0
name: myapp
"

    "Should remove debug key" |> Expect.equal actual expected
  }

  test "Remove node - entire array" {
    let testOutputPath, tmpDir =
      [ "items" .= [ [ "id" .= "1" ]; [ "id" .= "2" ] ]; "other" .= "data" ]
      |> prepareFile "test.yaml"

    dir "." [ [ "items" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "other: data
"

    "Should remove entire items array" |> Expect.equal actual expected
  }


  test "Remove node - scalar sequence by value" {
    let testOutputPath, tmpDir =
      [ "tags" .= [ "production"; "debug"; "experimental" ] ]
      |> prepareFile "test.yaml"

    dir "." [ [ "tags.[debug]" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "tags:
- production
- experimental
"

    "Should remove debug tag" |> Expect.equal actual expected
  }

  test "Remove node - predicate with quoted value" {
    let testOutputPath, tmpDir =
      items
        [ [ "url" .= "https://api.example.com"; "name" .= "api" ]
          [ "url" .= "https://web.example.com"; "name" .= "web" ] ]
      |> prepareFile "test.yaml"

    dir "." [ [ "items.[url=\"https://api.example.com\"]" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "items:
- url: https://web.example.com
  name: web
"

    "Should remove api item" |> Expect.equal actual expected
  }

  test "Remove node - predicate with special chars in key" {
    let testOutputPath, tmpDir =
      items
        [ [ "app.kubernetes.io/name" .= "myapp"; "version" .= "1.0" ]
          [ "app.kubernetes.io/name" .= "other"; "version" .= "2.0" ] ]
      |> prepareFile "test.yaml"

    dir "." [ [ "items.[app.kubernetes.io/name=myapp]" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "items:
- app.kubernetes.io/name: other
  version: 2.0
"

    "Should remove myapp item" |> Expect.equal actual expected
  }

  test "Remove node - deep nested with multiple predicates" {
    let testOutputPath, tmpDir =
      [ "environments"
        .= [ [ "name" .= "prod"
               "servers"
               .= [ [ "hostname" .= "prod-1"; "port" .= "8080" ]
                    [ "hostname" .= "prod-2"; "port" .= "8081" ] ] ]
             [ "name" .= "dev"
               "servers" .= [ [ "hostname" .= "dev-1"; "port" .= "3000" ] ] ] ] ]
      |> prepareFile "test.yaml"

    dir
      "."
      [ [ "environments.[name=prod].servers.[hostname=prod-2]" ]
        |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "environments:
- name: prod
  servers:
  - hostname: prod-1
    port: 8080
- name: dev
  servers:
  - hostname: dev-1
    port: 3000
"

    "Should remove prod-2 server only" |> Expect.equal actual expected
  }

  test "Remove node - scalar with spaces" {
    let testOutputPath, tmpDir =
      [ "commands" .= [ "npm start"; "npm test"; "npm run build" ] ]
      |> prepareFile "test.yaml"

    dir "." [ [ "commands.[\"npm test\"]" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "commands:
- npm start
- npm run build
"

    "Should remove npm test command" |> Expect.equal actual expected
  }

  test "Remove node - array index at root level" {
    let testOutputPath, tmpDir =
      [ [ "name" .= "first" ]; [ "name" .= "second" ]; [ "name" .= "third" ] ]
      |> prepareFile "test.yaml"

    dir "." [ [ "0" ] |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "- name: second
- name: third
"

    "Should remove first item from root array" |> Expect.equal actual expected
  }

  test "Remove node - nested predicate then key" {
    let testOutputPath, tmpDir =
      [ "clusters"
        .= [ [ "name" .= "us-east"
               "config" .= [ "replicas" .= "3"; "autoscale" .= "true" ] ]
             [ "name" .= "eu-west"
               "config" .= [ "replicas" .= "5"; "autoscale" .= "false" ] ] ] ]
      |> prepareFile "test.yaml"

    dir
      "."
      [ [ "clusters.[name=eu-west].config.autoscale" ]
        |> File.noYamlPaths testOutputPath ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "clusters:
- name: us-east
  config:
    replicas: 3
    autoscale: true
- name: eu-west
  config:
    replicas: 5
"

    "Should remove autoscale from eu-west only" |> Expect.equal actual expected
  }

  ]
|> testList "Tests"
|> runTestsWithCLIArgs [] [||]
