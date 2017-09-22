module Fable.Helpers.Markdown
open Fable.Core.JsInterop

type Markdown =
  abstract toHTML : string -> string
  abstract renderJsonML : obj -> string
  abstract toHTMLTree : obj -> obj
  abstract parse : string -> obj

let markdown : Markdown = importMember "markdown"

