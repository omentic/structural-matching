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
  of kObject: # match named objects
    objType: NimNode
    objFields: seq[tuple[key: string, val: Pattern]]
  of kTuple: # match unnamed tuples
    tupFields: seq[tuple[key: Option[string], val: Pattern]]
  of kUnion: # match the special union type
    unionVariant: NimNode
    unionType: Pattern
  of kSeq: # match a sequence or array
    seqFields: seq[Pattern]
  of kSet: # match a set or table
    setFields: seq[Pattern]

# todo: how to inject variables and also keep the if? do we use this in `case`?
macro `of`*(expr: NimNode, patt: Pattern) =
  discard

# todo: look into how matchImpl works. what should we generate?
macro `case`*(n: NimNode): NimNode =
  discard

# todo: necessary? for unions only? idk
func `==`*(a, b: Pattern): bool =
  discard

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
