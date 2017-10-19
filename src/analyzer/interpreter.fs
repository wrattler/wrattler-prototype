module Wrattler.Interpreter
open Wrattler.Ast
open Wrattler.Common

// ------------------------------------------------------------------------------------------------

open Wrattler.Binder

let rec evaluate ent = async {
  if ent.Value.IsSome then 
    Log.trace("interpreter", "Skipping %s (%s): %O", Astops.formatEntityKind ent.Kind, ent.Symbol.ToString(), box ent)
  else
  Log.trace("interpreter", "Evaluating %s (%s): %O", Astops.formatEntityKind ent.Kind, ent.Symbol.ToString(), box ent)
  match ent.Kind with
  | Root -> 
      ent.Value <- Some Nothing

  | Code("r", code, vars) ->
      let vars = vars |> List.choose (function { Kind = DataFrame(n, _); Value = Some(Frame v) } -> Some(n, v) | _ -> None)
      let! res = evalRCode (ent.Symbol.ToString()) vars code
      ent.Value <- Some res

  | Code("js", code, vars) ->
      let vars = vars |> List.choose (function { Kind = DataFrame(n, _); Value = Some(Frame v) } -> Some(n, v) | _ -> None)
      let code = 
        "(function(addOutput) { return (function(frames) {" +
        (vars |> Seq.mapi (fun i (v, _) -> sprintf "  var %s = frames[%d];" v i) |> String.concat "\n") +
        "  " + code + "}) })"
      let frames = ResizeArray<_>()
      for _, url in vars do 
        let! frame = Datastore.fetchFrame url
        frames.Add(frame)
      let outputs = ResizeArray<_>()
      eval<((string -> unit) -> unit) -> obj[][] -> unit> code outputs.Add (frames.ToArray())
      ent.Value <- Some(Outputs(outputs.ToArray()))

  | Code _ -> 
      failwith "Code in unsupported langauge"

  | DataFrame(v, rblock) ->
      do! evaluate rblock
      match rblock.Value with
      | Some(Frames frames) -> ent.Value <- Some(Frame(Map.find v frames))
      | v -> failwithf "R block did not evaluate to Frames but to %A" v

  | EntityKind.CodeBlock(lang, rcode, vars) ->
      do! evaluate rcode
      for v in vars do do! evaluate v
      ent.Value <- Some Nothing

  | Notebook ents -> 
      for ent in ents do 
        do! evaluate ent 
      ent.Value <- Some Nothing }
