module Wrattler.Interpreter
open Wrattler.Ast
open Wrattler.Common

// ------------------------------------------------------------------------------------------------

open Wrattler.Binder

let evalR (ctx:Languages.AnalyzerContext<_>) ent = async {
  match ent.Kind with
  | EntityKind.CodeBlock(lang, rcode, exports) ->
      do! ctx.Analyze(rcode, ctx.Context)
      for v in exports do do! ctx.Analyze(v, ctx.Context)
      return "", Nothing

  | EntityKind.Code("r", code, imports) ->
      let vars = imports |> List.choose (function { Kind = DataFrame(n, _); Value = Some(Frame v) } -> Some(n, v) | _ -> None)
      let! console, frames = evalRCode (ent.Symbol.ToString()) vars code
      return console, frames

  | EntityKind.DataFrame(v, rblock) ->
      do! ctx.Analyze(rblock, ctx.Context)
      match rblock.Value with
      | Some(Frames frames) when Map.containsKey v frames -> return "", Frame(Map.find v frames)
      | Some(Frames _) -> return "", Nothing // The variable was not really a frame
      | v -> return failwithf "R block did not evaluate to Frames but to %A" v

  | _ -> 
      return failwithf "Not an R entity: %A" ent }

let evalJs (ctx:Languages.AnalyzerContext<_>) ent = async {
  match ent.Kind with
  | EntityKind.CodeBlock(lang, jscode, exports) ->
      do! ctx.Analyze(jscode, ctx.Context)
      for v in exports do do! ctx.Analyze(v, ctx.Context)
      return "", Nothing

  | Code("javascript", code, imports) ->
      let vars = imports |> List.choose (function 
        | { Kind = DataFrame(n, _); Value = Some(Frame v) } -> Some(n, v) 
        | ent ->  
            Log.error("interpreter", "Entity '%s' did not evaluate to a frame value: %O", AstOps.formatEntityKind ent.Kind, ent.Value)
            None)
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
      return "", (Outputs(outputs.ToArray())) 

  | _ -> 
      return failwithf "Not a JS entity: %A" ent }
