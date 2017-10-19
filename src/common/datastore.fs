module Wrattler.Datastore

open Fable.Core
open Wrattler.Common

let cachedFrames = System.Collections.Generic.Dictionary<string, obj[]>()

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