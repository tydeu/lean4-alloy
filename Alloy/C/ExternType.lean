/-
Copyright (c) 2023 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
import Alloy.C.Syntax
import Alloy.C.Translator
import Alloy.Util.OpaqueType
import Lean.Compiler.NameMangling

namespace Alloy.C
open Lean Elab Parser Command

/-- The configuration of an Alloy C `(opaque_)extern_type` declaration. -/
structure ExternType extends Translator where
  /--
  An optional name for the automatically generated C function which
  constructs a Lean object for the type from the C value.
  -/
  toLean := .anonymous
  /--
  An optional name for the automatically generated C function which
  which extracts the C value from the Lean object for the external type.
  -/
  ofLean := .anonymous
  /--
  An optional name to give the generated
  `lean_external_class` declaration for the external type.
  -/
  externalClass : Name := .anonymous
  /--
  The C `foreach(ptr, fn)` function of the external type.
  This function should apply the Lean function `f` to each Lean object
  stored in the  C value of the type `ptr`.
  By default, an appropriately named no-op foreach function will be generated.
  -/
  foreach : Name := .anonymous
  /--
  The C `finalize(ptr)` function of the external type.
  This function should decrement the reference counters of any Lean object
  stored in the C value of the type `ptr` and free any memory it owns.
  By default, an appropriately named no-op finalize function will be generated.
  -/
  finalize : Name := .anonymous

unsafe def evalExternTypeUnsafe (stx : Syntax) : TermElabM ExternType :=
  Term.evalTerm ExternType (mkConst ``ExternType) stx

@[implemented_by evalExternTypeUnsafe]
opaque evalExternType (stx : Syntax) : TermElabM ExternType


namespace externTypeVal

syntax fieldSym := " => " str
syntax fieldCFn := "(" ident,+ ")"  (" => " str)? " := " stmtSeq

/--
A field of an `(opaque_)extern_type` declaration.
Fields either point to an existing C symbol or directly define a new function.

**New function**
```
alloy c opaque_extern_type Foo => Foo where
  finalize(ptr) := free(ptr)
```

**Existing symbol**
```
alloy c section
static inline void Foo_finalize(Foo* ptr) {
  free(ptr);
}
end

alloy c opaque_extern_type Foo => Foo where
  finalize => Foo_finalize
```
-/
syntax field :=  ident (fieldSym <|> (noWs fieldCFn))

syntax valWhere := " where " ppIndent(many1Indent(ppLine field))
syntax valTerm := " := " term

end externTypeVal

open externTypeVal in
syntax externTypeVal := valWhere <|> valTerm

inductive ExternTypeField
| sym (ref : Syntax) (sym : StrLit)
| cfn (ref : Syntax) (sym? : Option StrLit) (args : Array Ident) (stmts : Array StmtLike)

abbrev ExternTypeField.ref : ExternTypeField → Syntax
| sym ref .. | cfn ref .. => ref

open externTypeVal in
def elabExternTypeFields (stxs : Array (TSyntax ``field)) : CommandElabM (NameMap ExternTypeField) := do
 stxs.foldlM (init := {}) fun d stx => do
    match stx with
    | `(field|$id$val) =>
      let fieldName := id.getId
      if d.contains fieldName then
        throwErrorAt stx s!"field '{fieldName}' has already been specified"
      let projName := ``ExternType ++ fieldName
      pushInfoTree <| InfoTree.node (children := {}) <| Info.ofFieldInfo
        {projName, fieldName, lctx := {}, val := toExpr Name.anonymous, stx := id}
      match val with
      | `(fieldSym| => $cid) =>
        return d.insert fieldName <| .sym id cid
      | `(fieldCFn|($args,*) $[=> $sym?]? := $stmts*) =>
        return d.insert fieldName <| .cfn id sym? args stmts
      | _ => throwErrorAt val "ill-formed extern_type field value:{indentD val}"
    | _ => throwErrorAt stx "ill-formed extern_type field:{indentD stx}"

/--
Declare a type to be represented by a `lean_external_class` object.
The class definition is determination by a configuration passed via a `where`
clause. The fields of which are defined in `Alloy.C.ExternType`.

**Example**
```
alloy c extern_type LeanFoo => CFoo where
  toLean => "CFoo_to_lean"
  ofLean => "of_LeanFoo"
  externalClass => "g_Y_Lean"
  foreach(ptr, fn) := {}
  finalize(ptr) := free(ptr)
```
-/
scoped syntax (name := externType)
"alloy " &"c " &"extern_type " ident " => " cSpec+ externTypeVal : command

elab_rules : command
| `(externType| alloy c extern_type $tid => $cTy* $val:externTypeVal) => do
  let name ← resolveGlobalConstNoOverloadWithInfo tid
  let cfg : ExternType ← id do
    match val with
    | `(externTypeVal|:=%$tk $term) =>
      logWarningAt tk "the `:=` syntax for `extern_type` is deprecated; please migrate to the `where` syntax instead"
      let cfg ← liftTermElabM <| evalExternType term
      return cfg
    | `(externTypeVal|where%$whereTk $fields*) => withRef whereTk do
      let fields ← elabExternTypeFields fields
      Prod.snd <$> StateT.run (s := {}) do
      let extractIdField name f := do
        if let some field := fields.find? name then
          match field with
          | .sym _ sym => modify (f · (Name.mkSimple sym.getString))
          | .cfn ref .. => throwErrorAt ref "'{name}' does not support a function value"
      extractIdField `toLean ({· with toLean := ·})
      extractIdField `ofLean ({· with ofLean := ·})
      extractIdField `externalClass ({· with externalClass := ·})
      if let some field := fields.find? `foreach then
        match field with
        | .sym _ sym =>
          modify ({· with foreach := .mkSimple sym.getString})
        | .cfn ref sym? args stmts =>
          if args.size > 2 then
            throwErrorAt ref "wrong number of arguments for 'foreach' declaration: expected at most 2, got {args.size}"
          let ptr := args[0]?.getD <| mkIdent `ptr
          let f := args[1]?.getD <| mkIdent `f
          let body := packBody stmts
          let cid :=
            match sym? with
            | some sym => mkIdentFrom sym <| .mkSimple sym.getString
            | none => mkIdentFrom ref <| .mkSimple <| "_alloy_foreach_" ++ name.mangle
          modify ({· with foreach := cid.getId})
          let cmd := Unhygienic.run `(
            alloy c section
            static inline void $cid:ident($cTy:cDeclSpec* * $ptr:ident, b_lean_obj_arg $f) $body:compStmt
            end
          )
          withMacroExpansion ref cmd <| elabCommand cmd
      if let some field := fields.find? `finalize then
        match field with
        | .sym _ sym =>
          modify ({·  with finalize := .mkSimple sym.getString})
        | .cfn ref sym? args stmts =>
          if args.size > 1 then
            throwErrorAt ref "wrong number of arguments for 'finalize' declaration: expected at most 1, got {args.size}"
          let ptr := args[0]?.getD <| mkIdent `ptr
          let body := packBody stmts
          let cid :=
            match sym? with
            | some sym => mkIdentFrom sym <| .mkSimple sym.getString
            | none => mkIdentFrom ref <| .mkSimple <| "_alloy_finalize_" ++ name.mangle
          modify ({·  with finalize := cid.getId})
          let cmd := Unhygienic.run `(
            alloy c section
            static inline void $cid:ident($cTy:cDeclSpec* * $ptr:ident) $body:compStmt
            end
          )
          withMacroExpansion ref cmd <| elabCommand cmd
    | _ => throwErrorAt val "ill-formed extern_type value"
  let cls := mkIdent <| if cfg.externalClass.isAnonymous then
    .mkSimple <| "_alloy_g_class_" ++ name.mangle else cfg.externalClass
  let toLean := if cfg.toLean.isAnonymous then
    .mkSimple <| "_alloy_to_" ++ name.mangle else cfg.toLean
  let ofLean := if cfg.ofLean.isAnonymous then
    .mkSimple <| "_alloy_of_" ++ name.mangle else cfg.ofLean
  let foreach := mkIdent <| if cfg.foreach.isAnonymous then
    .mkSimple <| "_alloy_foreach_" ++ name.mangle else cfg.foreach
  if cfg.foreach.isAnonymous then
    let cmd := Unhygienic.run `(
      alloy c section
      static inline void $foreach:ident(void * ptr, b_lean_obj_arg f) {}
      end
    )
    withMacroExpansion (← getRef) cmd <| elabCommand cmd
  let finalize := mkIdent <| if cfg.finalize.isAnonymous then
    .mkSimple <| "_alloy_finalize_" ++ name.mangle else cfg.finalize
  if cfg.finalize.isAnonymous then
    let cmd := Unhygienic.run `(
      alloy c section
      static inline void $finalize:ident(void * ptr) {}
      end
    )
    withMacroExpansion (← getRef) cmd <| elabCommand cmd
  let cmd := Unhygienic.run `(
    alloy c section
    static lean_external_class * $cls:ident = NULL;
    static inline lean_obj_res $(mkIdent toLean):ident($cTy:cDeclSpec* * o) {
      if ($cls == NULL) {
        $cls:ident = lean_register_external_class(
          (void(*)(void*))$finalize, (void(*)(void*, b_lean_obj_arg))$foreach);
      }
      return lean_alloc_external($cls, o);
    }
    static inline $cTy* * $(mkIdent ofLean):ident(b_lean_obj_arg o) {
      return ($cTy* *)(lean_get_external_data(o));
    }
    end
  )
  withMacroExpansion (← getRef) cmd <| elabCommand cmd
  modifyEnv fun env => translatorExt.insert env name {toLean, ofLean}


/--
Declare an `opaque_type` represented by a `lean_external_class` object.
The class definition is determination by a configuration passed via a `where`
clause. The fields of which are defined in `Alloy.C.ExternType`.

**Example**
```
alloy c opaque_extern_type LeanFoo => CFoo where
  toLean => "CFoo_to_lean"
  ofLean => "of_LeanFoo"
  externalClass => "g_Y_Lean"
  foreach(ptr, fn) := {}
  finalize(ptr) := free(ptr)
```
-/
scoped syntax (name := opaqueExternType)
(docComment)? (Term.attributes)? (visibility)? «unsafe»?
"alloy " &"c " &"opaque_extern_type " declId binders (typeLvSpec)?
  " => " cSpec+ externTypeVal : command

macro_rules
| `(opaqueExternType| $(doc?)? $(attrs?)? $(vis?)? $[unsafe%$uTk?]?
  alloy c opaque_extern_type $declId $bs* $[$ty]? => $cTy* $val:externTypeVal) => do
  let id : Ident := ⟨declId.raw[0]⟩
  `(
    $[$doc?:docComment]? $(attrs?)? $(vis?)? $[unsafe%$uTk?]? opaque_type $declId $bs* $[$ty]?
    alloy c extern_type $id => $cTy* $val:externTypeVal
  )
