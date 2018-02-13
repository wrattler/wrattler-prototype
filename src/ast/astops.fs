module Wrattler.Ast.AstOps

// ------------------------------------------------------------------------------------------------

/// Format entity kind into something readable
let formatEntityKind = function
  | EntityKind.Root -> "root"
  | EntityKind.Code(lang, code, _) -> lang + " code: " + code.Substring(0, min 10 code.Length) + "..."
  | EntityKind.CodeBlock(lang, _, _) -> lang + " code block"
  | EntityKind.DataFrame(var, _) -> "frame " + var
  | EntityKind.Notebook(_) -> "notebook"
  | EntityKind.CustomEntity ent -> ent.FormatEntity()
   
/// Return entity name (or anonymous) and all its antecedants
let rec entityCodeAndAntecedents = function
  | EntityKind.Root -> [], ""
  | EntityKind.Code(lang, code, imports) -> imports, sprintf "<code lang='%s'>%s</code>" lang code
  | EntityKind.CodeBlock(lang, code, exports) -> code::exports, sprintf "<codeblock lang='%s'/>" lang
  | EntityKind.DataFrame(var, block) -> [block], sprintf "<var name='%s'/>" var
  | EntityKind.Notebook(blocks) -> blocks, "<notebook/>"
  | EntityKind.CustomEntity ent -> ent.GetCodeAndAntecedents()
  
// Provide easy access to entity's antecedents
type Entity with
  member x.Antecedents = let ans, _ = entityCodeAndAntecedents x.Kind in ans




