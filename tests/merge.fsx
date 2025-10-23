#load "common.fsx"
#load "../fsx/Render.fsx"

open Common
open Expecto
open Ksl
open Ksl.Builder.Mold
open System.IO
open Yzl

let items = Yzl.seq
let item = Yzl.str<string>

[ test "Merge simple scalar into map" {
    let testOutputPath, tmpDir =
      [ "name" .= "original"
        "value" .= "123" ]
      |> prepareFile "test.yaml"

    let mergeYzl =
      [ "value" .= "456"
        "extra" .= "new" ]

    dir "." [ File.mergeYzl testOutputPath [ !mergeYzl ] ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "name: original
value: 456
extra: new
"

    "Should merge scalars into existing map" |> Expect.equal actual expected
  }

  test "Merge nested map" {
    let testOutputPath, tmpDir =
      [ "config"
        .= [ "timeout" .= "30"
             "retries" .= "3" ] ]
      |> prepareFile "test.yaml"

    let mergeYzl =
      [ "config"
        .= [ "retries" .= "5"
             "maxConnections" .= "100" ] ]

    dir "." [ File.mergeYzl testOutputPath [ !mergeYzl ] ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "config:
  timeout: 30
  retries: 5
  maxConnections: 100
"

    "Should merge nested maps preserving existing keys"
    |> Expect.equal actual expected
  }

  test "Merge into sequence" {
    let testOutputPath, tmpDir =
      items [
        [ "name" .= "first" ]
        [ "name" .= "second" ]
      ]
      |> prepareFile "test.yaml"

    let mergeYzl = items [ [ "name" .= "third" ] ]

    dir "." [ File.mergeYzl testOutputPath [ !mergeYzl ] ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "items:
- name: first
- name: second
- name: third
"

    "Should merge sequence items" |> Expect.equal actual expected
  }

  test "MergeYzlAt - simple property path" {
    let testOutputPath, tmpDir =
      [ "database"
        .= [ "host" .= "localhost"
             "port" .= "5432" ]
        "cache" .= [ "ttl" .= "3600" ] ]
      |> prepareFile "test.yaml"

    let mergeYzl =
      ![ "host" .= "prod.db.com"
         "maxConnections" .= "50" ]

    dir "." [ File.mergeYzlAt testOutputPath "database" mergeYzl ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "database:
  host: prod.db.com
  port: 5432
  maxConnections: 50
cache:
  ttl: 3600
"

    "Should merge at nested path preserving siblings"
    |> Expect.equal actual expected
  }

  test "MergeYzlAt - predicate-based path" {
    let testOutputPath, tmpDir =
      items [
        [ "name" .= "prod"
          "port" .= "8080" ]
        [ "name" .= "dev"
          "port" .= "3000" ]
      ]
      |> prepareFile "test.yaml"

    let mergeYzl =
      ![ "port" .= "8443"
         "ssl" .= "true" ]

    dir "." [ File.mergeYzlAt testOutputPath "items.[name=prod]" mergeYzl ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "items:
- name: prod
  port: 8443
  ssl: true
- name: dev
  port: 3000
"

    "Should merge into predicate-matched item" |> Expect.equal actual expected
  }

  test "MergeYzlAt - simple nested property chain" {
    let testOutputPath, tmpDir =
      [ "servers"
        .= [ [ "name" .= "primary"
               "config"
               .= [ "timeout" .= "30"
                    "retries" .= "3" ] ] ] ]
      |> prepareFile "test.yaml"

    let mergeYzl =
      ![ "timeout" .= "60"
         "maxQueue" .= "1000" ]

    dir "." [ File.mergeYzlAt testOutputPath "servers.0.config" mergeYzl ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "servers:
- name: primary
  config:
    timeout: 60
    retries: 3
    maxQueue: 1000
"

    "Should merge at deeply nested path with index" |> Expect.equal actual expected
  }

  test "MergeYzlAt - array index path" {
    let testOutputPath, tmpDir =
      items [
        [ "id" .= "1"
          "status" .= "active" ]
        [ "id" .= "2"
          "status" .= "inactive" ]
      ]
      |> prepareFile "test.yaml"

    let mergeYzl =
      ![ "status" .= "archived"
         "archived_at" .= "2024-01-01" ]

    dir "." [ File.mergeYzlAt testOutputPath "items.1" mergeYzl ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "items:
- id: 1
  status: active
- id: 2
  status: archived
  archived_at: 2024-01-01
"

    "Should merge at array index path" |> Expect.equal actual expected
  }

  test "MergeYzlAt with empty path targets root" {
    let testOutputPath, tmpDir = [ "a" .= "1" ] |> prepareFile "test.yaml"

    let mergeYzl = ![ "b" .= "2" ]

    dir "." [ File.mergeYzlAt testOutputPath "" mergeYzl ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "a: 1
b: 2
"

    "Should merge at root when path is empty" |> Expect.equal actual expected
  }

  test "MergeYzlAt - map in sequence by predicate" {
    let testOutputPath, tmpDir =
      [ "servers"
        .= [ [ "hostname" .= "prod-1"
               "port" .= "8080"
               "status" .= "active" ]
             [ "hostname" .= "prod-2"
               "port" .= "8081"
               "status" .= "inactive" ] ] ]
      |> prepareFile "test.yaml"

    let mergeYzl =
      ![ "status" .= "archived"
         "lastSeen" .= "2024-01-01" ]

    dir "." [ File.mergeYzlAt testOutputPath "servers.[hostname=prod-2]" mergeYzl ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "servers:
- hostname: prod-1
  port: 8080
  status: active
- hostname: prod-2
  port: 8081
  status: archived
  lastSeen: 2024-01-01
"

    "Should merge into sequence item by predicate" |> Expect.equal actual expected
  }

  test "Merge into empty sequence uses block style" {
    let testOutputPath, tmpDir =
      items []  // empty sequence
      |> prepareFile "test.yaml"

    let mergeYzl = items [ [ "name" .= "first" ] ]

    dir "." [ File.mergeYzl testOutputPath [ !mergeYzl ] ]
    |> RenderTo.fileSystem tmpDir

    let actual = File.ReadAllText testOutputPath

    let expected =
      "items:
- name: first
"

    "Should merge into empty sequence with block style not flow style"
    |> Expect.equal actual expected
  }
  ]
|> testList "Merge Tests"
|> runTestsWithCLIArgs [] [||]
