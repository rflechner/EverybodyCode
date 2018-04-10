module AST

open PositionTacking

type varname = string
type constname = string
type typename = string
type genericname = string
type infixOp = Add | Subtract | Multiply | Divide | Eq | Ne | Lt | Gt | Le | Ge | And | Or | Modulo
type prefixOp = Negative | IncBefore | DecBefore
type postfixOp = IncAfter | DecAfter
type HashTable<'k,'v> = System.Collections.Generic.Dictionary<'k,'v>

type value =
    | Bool              of bool
    | Decimal           of float
    | String            of string
    | Char              of char
    | Array             of expression list
    | HashMap           of (expression * expression) list
    | Interval          of expression * expression
and expression =
    | Litteral          of value
    | Variable          of varname
    | Constant          of constname
    | Operation         of expression * infixOp * expression
    | PrefixOp          of prefixOp * expression
    | PostfixOp         of postfixOp * expression
    | CallExp           of string * expression list
    | Apply             of expression * expression
    | InstanciateType   of string * (string option * expression) list
    | MethodCall        of string * string * expression list
    | ArrayOffset       of source:expression * offset:expression
and functionImpl      = string positioned * parameter positioned list * block * expression positioned option
and abstractFunction  = string positioned * parameter positioned list * string positioned
and statement =
    | SetVar            of firstAlloc:bool * varname * varset
    | If                of expression * block
    | IfElse            of expression * block * block
    | While             of expression * block
    | For               of start:expression positioned * ``end``:expression positioned * step:expression positioned option * varname positioned * block
    | ForEach           of expression positioned * expression positioned * block
    | ForEachIndex      of expression * expression * expression * block
    | ExternFunction    of string * parameter list
    | Function          of functionImpl
    | Break
    | Continue
    | Comment           of string
    | Return            of expression
    | CallStatement     of instance:string option * name:string * args:expression list
    | ApplyStatement    of expression * expression
    | DeclareType       of name:string * basename:string option * typeMember list
    | ExternalType      of name:string * basename:string option * typeMember list
    | AbstractType      of string * typeMember positioned list
    | AddEventHandler   of varname * string * string
    | NotRecognized     of string
    | UnionType         of name:string * unionMember list * genericname list
    | PatternMatching   of target:expression * conditions:patternMatchingCondition list * defaultAction:expression
and varset =
    | AssignValue       of expression
    | IncValue          of expression
    | DecValue          of expression
and block = statement positioned list
and parameter = varname * typename
and typeMember =
    | TypeProperty       of name:varname * typename * inConstructor:bool
    | TypeMethod         of functionImpl
    | TypeAbstractMethod of abstractFunction
and unionMember = 
  | UnionMember         of varname * typename option
  | GenericUnionMember  of varname * genericname
and patternMatchingCondition = patternMatchingComparison * expression * expression
and patternMatchingComparison = MustEqual | MustBeTrue

