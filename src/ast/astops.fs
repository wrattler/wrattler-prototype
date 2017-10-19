module Wrattler.Ast.Astops

// ------------------------------------------------------------------------------------------------

/// Format entity kind into something readable
let formatEntityKind = function
  | EntityKind.Root -> "root"
  | EntityKind.Code(lang, code, _) -> lang + " code: " + code.Substring(0, min 10 code.Length) + "..."
  | EntityKind.CodeBlock(lang, _, _) -> lang + " code block"
  | EntityKind.DataFrame(var, _) -> "frame " + var
  | EntityKind.Notebook(_) -> "notebook"
  
/// Return entity name (or anonymous) and all its antecedants
let rec entityCodeAndAntecedents = function
  | EntityKind.Root -> [], ""
  | EntityKind.Code(lang, code, frames) -> frames, sprintf "<code lang='%s'>%s</code>" lang code
  | EntityKind.CodeBlock(lang, code, vars) -> code::vars, sprintf "<codeblock lang='%s'/>" lang
  | EntityKind.DataFrame(var, block) -> [block], sprintf "<var name='%s'/>" var
  | EntityKind.Notebook(blocks) -> blocks, "<notebook/>"
  




