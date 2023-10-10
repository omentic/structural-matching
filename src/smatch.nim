import std/[macros, options, sequtils, strutils, sugar]

## The Pattern object. Contains information for injecting bindings.
type PatternKind = enum
  kDiscard, kValue, kBinding,
  kObject, kTuple, kUnion,
  kRange, kSeq, kSet

## The Pattern object. Contains information for injecting bindings.
type Pattern = ref object
  case kind: PatternKind
  of kDiscard: # the `_` operator. matches anything and does not bind.
    discard
  of kRange: # the `..` operator. for seqs and sets.
    discard  # matches some number of values and does not bind.
  of kValue: # a value match. can be a variable.
    value: NimNode
  of kBinding: # a new binding.
    binding: NimNode
    rebinding: Option[NimNode] # the "y" in `x as y`
  of kObject: # match named objects
    objType: string
    objFields: seq[Pattern]
  of kUnion: # match the special union type
    unionVariant: NimNode
    unionType: Pattern
  of kTuple: # match unnamed tuples
    tupFields: seq[Pattern]
  of kSeq: # match a sequence or array
    seqFields: seq[Pattern]
  of kSet: # match a set or table
    setFields: seq[Pattern]

# todo: how to inject variables and also keep the if? do we use this in `case`?
macro `of`*(expr: NimNode, patt: Pattern) =
  discard

## Parse a NimNode into a proper Pattern.
func parse(pattern: NimNode): Pattern =
  case pattern.kind
  of nnkCall: # objects and unions
    Pattern(kind: kObject, objType: pattern[0].strVal, objFields: pattern[1 .. ^1].map(x => x.parse()))
  of nnkTupleConstr: # tuples
    Pattern(kind: kTuple, tupFields: pattern.children.toSeq.map(x => x.parse()))
  of nnkBracket: # seqs and arrays
    Pattern(kind: kSeq, seqFields: pattern.children.toSeq.map(x => x.parse()))
  of nnkCurly: # sets
    Pattern(kind: kSet, setFields: pattern.children.toSeq.map(x => x.parse()))
  of nnkInfix: # bindings (usually?)
    let infix = pattern[0]
    if infix == ident("as"):
      Pattern(kind: kBinding, binding: pattern[1], rebinding: some(pattern[2]))
    else: # not the `as` infix, probably a value
      Pattern(kind: kValue, value: pattern)
  # todo: kRange
  of nnkIdent: # discards, bindings, and values
    if pattern == ident("_"):
      Pattern(kind: kDiscard)
    else: # todo: syntax sugar infix support (requires semantics)
      Pattern(kind: kValue, value: pattern)
  else: # values
    Pattern(kind: kValue, value: pattern)

func compile(pattern: Pattern): NimNode = discard

# todo: what should we generate?
macro `case`*(body: untyped): untyped =
  case body.kind
  of nnkCaseStmt:
    let subject = body[0]
    let cases = body[1 .. ^1]
    for branch in cases:
      case branch.kind
      of nnkOfBranch:
        let pattern = branch[0]
        let body = branch[1]
        discard
        case pattern.kind
        of nnkCommand: # match with `where` clause (probably)
          let pattern_clause = pattern[0]
          let where_clause = pattern[1]
          if where_clause.kind != nnkCommand: # checks
            error("bluh", where_clause)
          if where_clause[0] != ident("where"):
            error("bluh", where_clause[0])
          let where_cond = where_clause[1]
          let parsed = pattern_clause.parse()
          # let compiled = parsed.compile()
          # todo: inject
        of nnkCall: # match with no `where` clause
          let parsed = pattern.parse()
          discard
          # let compiled = parsed.compile()
          # todo: inject
        else: # match is a literal (probably)
          discard
      of nnkElifBranch, nnkElse:
        discard # todo: inject
      else:
        error("invalid case branch", branch)
  else:
    error("not a case statement", body)

func `$`*(pattern: Pattern): string =
  case pattern.kind
  of kDiscard:
    "_"
  of kRange:
    ".."
  of kValue:
    $pattern.value
  of kBinding:
    $pattern.binding
  of kObject:
    $pattern.objType & "(" & pattern.objFields.map(x => $x).join(", ") & ")"
  of kUnion:
    case pattern.unionType.kind
    of kTuple, kSeq, kSet:
      $pattern.unionVariant & $pattern.unionType
    else:
      $pattern.unionVariant & "(" & $pattern.unionType & ")"
  of kTuple:
    "(" & pattern.tupFields.map(x => $x).join(", ") & ")"
  of kSeq:
    "[" & pattern.seqFields.map(x => $x).join(", ") & "]"
  of kSet:
    "{" & pattern.setFields.map(x => $x).join(", ") & "}"
