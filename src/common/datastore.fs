module Wrattler.Datastore

open Fable.Core
open Wrattler.Common

let dataStoreUrl = "http://localhost:7102"

let cachedFrames = System.Collections.Generic.Dictionary<string, obj[]>()

let storeFrame hash file json = async {
  let url = sprintf "%s/%s/%s" dataStoreUrl hash file
  let! _ = Http.Request("PUT", url, jsonStringify json) 
  return url }

let fetchFrame url = async {
  if not (cachedFrames.ContainsKey(url)) then
    Log.trace("interpreter", "Fetching data frame: %s", url)
    let! json = Http.Request("GET", url)
    cachedFrames.[url] <- jsonParse json
  return cachedFrames.[url] }

let tryFetchPreview url refresh = 
  if cachedFrames.ContainsKey(url) then Some(cachedFrames.[url])
  else 
    Async.StartImmediate <| async {
      let! _ = fetchFrame url 
      refresh () }
    None