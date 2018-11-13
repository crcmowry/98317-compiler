structure Lexer =
struct
  datatype token =
    IDENT of string
  | NUMBER of int
  | KWMAIN
  | KWIF
  | KWELSE
  | KWWHILE
  | KWRETURN
  | KWTRUE
  | KWFALSE
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | LAND
  | LOR
  | LNOT
  | LESS
  | GREATER
  | EQUAL
  | NOTEQUAL
  | ASSIGN
  | SEMI
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE


  open Stream

  exception Error

  structure Table =
     HashTable (structure Key = StringHashable)

  val keywords : token Table.table = Table.table 7

  val () =
    List.app (fn (str, token) => Table.insert keywords str token) [
      ("main", KWMAIN),
      ("if", KWIF),
      ("else", KWELSE),
      ("while", KWWHILE),
      ("return", KWRETURN),
      ("true", KWTRUE),
      ("false", KWFALSE)
    ]

  type pos = int

  type t = int -> (token * pos) front

  type self = { lexmain : char stream -> t }

  type info = { match : char list,
                len : int,
                start : char stream,
                follow : char stream,
                self : self }

  fun action f ({ match, len, follow, self, ... }:info) pos =
     Cons (f (match, len, pos), lazy (fn () => #lexmain self follow (pos+len)))

  fun simple token ({ len, follow, self, ... }:info) pos =
     Cons ((token, pos), lazy (fn () => #lexmain self follow (pos+len)))

  structure Arg =
  struct
    type symbol = char
    val ord = Char.ord

    type t = t

    type self = self
    type info = info

    fun eof _ _ = Nil

    fun skip ({ len, follow, self, ... }:info) pos = #lexmain self follow (pos+len)

    val ident =
      action
        (fn (chars, _, pos) =>
          let
            val str = implode chars
          in
            case Table.find keywords str of
              NONE => (IDENT str, pos)
            | SOME token => (token, pos)
          end)

    val number =
      action
        (fn (chars, _, pos) =>
         (case Int.fromString (implode chars) of
            SOME n => (NUMBER n, pos)
          | NONE => raise Bind)
          handle Overflow =>
            (print "Illegal constant at "; print (Int.toString pos); print ".\n"; raise Error))

    fun error _ pos =
      (print "Lexical error at "; print (Int.toString pos); print ".\n"; raise Error)

    val kwmain = simple KWMAIN
    val kwif = simple KWIF
    val kwelse = simple KWELSE
    val kwwhile = simple KWWHILE
    val kwreturn = simple KWRETURN
    val kwtrue = simple KWTRUE
    val kwfalse = simple KWFALSE

    val lbrace = simple LBRACE
    val rbrace = simple RBRACE
    val lparen = simple LPAREN
    val rparen = simple RPAREN

    val land = simple LAND
    val lor = simple LOR
    val lnot = simple LNOT

    val less = simple LESS
    val greater = simple GREATER
    val equal = simple EQUAL
    val notequal = simple NOTEQUAL

    val assign = simple ASSIGN
    val semi = simple SEMI
    val comma = simple COMMA

    val plus = simple PLUS
    val minus = simple MINUS
    val times = simple TIMES
    val divide = simple DIVIDE
  end

  structure LexMain =
    VSIMPLLexFun
     (structure Streamable = StreamStreamable
      structure Arg = Arg)

  fun lex s = lazy (fn () => LexMain.lexmain s 0)
end
