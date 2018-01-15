module Wrattler.Gamma.Monaco

open Fable.Core
open Fable.Import.Monaco
open Fable.Import.Browser

open Wrattler
open Wrattler.Languages
open Wrattler.Common
open Wrattler.Gamma.Ast
open Wrattler.Gamma.AstOps
open Wrattler.Gamma.TypeChecker
open Fable.Core.JsInterop

let noState = 
  { new languages.IState with
      member this.clone() = this
      member this.equals(other) = true }

let getColorClass = function
  | TokenKind.String _ -> "string" 
  | TokenKind.QIdent _ | TokenKind.Ident _ -> "ident" 
  | TokenKind.Dot _ -> "operator" 
  | TokenKind.Let | TokenKind.Boolean _ | TokenKind.Fun | TokenKind.Arrow -> "keyword" 
  | TokenKind.Number _ -> "number" 
  | _ -> ""

let tokensProvider = 
  { new languages.TokensProvider with
      member this.tokenize(line, state) =
        let tokens = JsInterop.createEmpty<languages.ILineTokens>
        tokens.endState <- noState
        tokens.tokens <- ResizeArray()

        let tokenized, _ = Tokenizer.tokenize "na" line
        for t in tokenized do
          let tok = JsInterop.createEmpty<languages.IToken>
          tok.startIndex <- float t.Range.Start
          tok.scopes <- Fable.Core.U2.Case1 (getColorClass t.Token)
          tokens.tokens.Add(tok)

        tokens
      member this.getInitialState() = noState }

let createCompletionProvider (getService:string -> string * (string -> Async<BindingResult>)) = 
  { new languages.CompletionItemProvider with 
      member this.triggerCharacters = Some(ResizeArray [| for i in 0 .. 255 -> string (char i) |])
      member this.provideCompletionItems(model, position, token) =           
        async {      
          try    
            let block, typeCheck = getService (model.uri.toString())
            let input = model.getValue(editor.EndOfLinePreference.LF, false)
            
            let conv = LocationMapper(input)
            let loc = conv.LineColToAbsolute(int position.lineNumber, int position.column)
            
            let! ents = typeCheck  input
            let optMembers = 
              ents.Entities |> Seq.tryPick (fun (rng, ent) ->
                match ent with 
                | { Kind = GammaEntity(GammaEntityKind.Member({ Type = Some t }, { Kind = GammaEntity(GammaEntityKind.MemberName(n)) })) } 
                      when loc >= rng.Start && loc <= rng.End + 1 && rng.Block = block -> 
                    Log.trace("completions", "Antecedant at current location (member '%s'): %O", n, t)
                    match t with
                    | GammaType(Type.Object obj) -> Some(n, rng, obj.Members)
                    | _ -> None
                | { Kind = GammaEntity(GammaEntityKind.Member({ Type = Some t }, { Kind = GammaEntity(GammaEntityKind.MemberName(n)) })) } ->
                    Log.trace("completions", "Ignoring '%s' at location %s-%s (current=%s)", n, rng.Start, rng.End, loc)
                    None
                | _ -> None)

            let convertRange (rng:Ast.Range) = 
              let sl, sc = conv.AbsoluteToLineCol(rng.Start)
              let el, ec = conv.AbsoluteToLineCol(rng.End)
              let res = JsInterop.createEmpty<IRange>
              res.startColumn <- float sc
              res.startLineNumber <- float sl
              res.endColumn <- float ec + 1.0
              res.endLineNumber <- float el
              res

            match optMembers with 
            | None -> 
                Log.trace("completions", "no members at %s", loc)
                return ResizeArray []
            | Some (currentName, nameRange, members) -> 
                let nameRange = convertRange nameRange
                Log.trace("completions", "providing %s members at %O", members.Length, nameRange)

                let members = members |> Array.filter (fun m ->
                  match AstOps.pickMetaByType "http://schema.thegamma.net" "CompletionItem" m.Metadata with
                  | Some item -> not (getProperty item "hidden")
                  | _ -> true)

                let completion =
                  [ for m in members ->
                      let ci = JsInterop.createEmpty<languages.CompletionItem>
                      let n = m.Name
                      let k = 
                        match m.Type with 
                        | Type.Method _ -> languages.CompletionItemKind.Method 
                        | _ -> languages.CompletionItemKind.Property
                      ci.kind <- k
                      ci.label <- n
                      ci.insertText <- Some(AstOps.escapeIdent n)
                      // We set the current text in the range as 'filterText' so Monaco 
                      // does not filter things out when Ctrl+Space is typed (trick!)
                      //ci.filterText <- Some(n + AstOps.escapeIdent currentName) 
                      match m.Type with
                      | Type.Method(arguments=args) -> 
                          let acc, l = 
                            [ for ma in args -> (if ma.Optional then "?" else "") + ma.Name ] 
                            |> Seq.fold (fun (acc, l:string) s ->
                                if l.Length > 100 then (l::acc, s)
                                else (acc, if l = "" then s else l+","+s)) ([], "")
                          let args = l::acc |> List.rev |> String.concat ",\n"
                          ci.documentation <- Some("(" + args + ")")
                      | _ -> ()

                      let eo = JsInterop.createEmpty<editor.ISingleEditOperation>
                      eo.text <- AstOps.escapeIdent n
                      eo.range <- nameRange
                      ci.textEdit <- Some eo
                      ci ] 
                Log.trace("completions", "returning %O", Array.ofSeq completion)
                return ResizeArray(completion)
            with e ->
              Log.exn("completions", "completions failed %O", e)
              return ResizeArray() } |> Async.StartAsPromise |> Fable.Core.U4.Case2

      member this.resolveCompletionItem(item, token) = Fable.Core.U2.Case1 item }

let createdEditors = System.Collections.Generic.Dictionary<string, string * (string -> Async<BindingResult>)>()
let getService uri = createdEditors.[uri]

let setupMonacoServices () = 
  let lang = JsInterop.createEmpty<languages.ILanguageExtensionPoint>
  lang.id <- "gamma"
  languages.Globals.register(lang)
  languages.Globals.setTokensProvider("gamma", tokensProvider) |> ignore
  languages.Globals.registerCompletionItemProvider("gamma", createCompletionProvider getService) |> ignore

let configureMonacoEditor (ed:editor.ICodeEditor) block checker = 
  createdEditors.Add(ed.getModel().uri.toString(), (block, checker))