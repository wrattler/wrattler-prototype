module Wrattler.Datastore

open Fable.Core
open Wrattler.Common

let dataStoreUrl = "http://localhost:7102"

let cachedFrames = System.Collections.Generic.Dictionary<string, Future<obj[]>>()

let storeFrame hash file json = async {
  let url = sprintf "%s/%s/%s" dataStoreUrl hash file
  cachedFrames.[url] <- Async.StartAsFuture(async.Return(json))
  let! _ = Http.Request("PUT", url, jsonStringify json) 
  return url }

let fetchFrame url = async {
  if not (cachedFrames.ContainsKey(url)) then
    cachedFrames.[url] <- Async.StartAsFuture <| async { 
      Log.trace("interpreter", "Fetching data frame: %s", url)
      let! json = Http.Request("GET", url)
      return jsonParse json }
  return! Async.AwaitFuture cachedFrames.[url] }

let tryFetchPreview url refresh = 
  if cachedFrames.ContainsKey(url) && cachedFrames.[url].Value.IsSome then Some(cachedFrames.[url].Value.Value)
  else 
    Async.StartImmediate <| async {
      let! _ = fetchFrame url 
      refresh () }
    None