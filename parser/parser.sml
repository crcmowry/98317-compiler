structure Parser =
struct
  open VSIMPL

  structure Arg =
  struct
    type string = string
    type int = int
    type aexp = aexp
    type bexp = bexp
    type cmd = cmd
    type cmdlist = cmd list
    type program = program
    type identlist = string list

    fun singleton s = [s]

    val args_cons = op::
    val args_one = singleton
    val args = (fn x => x)
    fun args_empty () = []
    val main = (fn x => x)
    val cmd_cons = op::
    val cmd_one = singleton
    val cmds = (fn x => x)
    fun cmds_empty () = []
    val cmd_return = Return
    val cmd_while = While
    val cmd_if_else = If
    fun cmd_if (b, c) = If (b, c, [])
    val cmd_assign = Assign
    val bexp_greater = GreaterThan
    val bexp_less = LessThan
    val bexp_notequal = NotEquals
    val bexp_equal = Equals
    val bexp_or = OrElse
    val bexp_and = AndAlso
    val bexp_not = Not
    fun bexp_false () = BConst false
    fun bexp_true () = BConst true
    val bexp_id = (fn x => x)
    val aexp_divide = DividedBy
    val aexp_times = Times
    val aexp_minus = Minus
    val aexp_plus = Plus
    val aexp_number = AConst
    val aexp_ident = Var
    val aexp_id = (fn x => x)

    datatype terminal = datatype Lexer.token

    fun error s =
      case Stream.front s of
        Stream.Nil => (print "Syntax error at end of file.\n"; Fail "parse")
    | Stream.Cons ((_, pos), _) =>
        (print "Syntax error at "; print (Int.toString pos); print ".\n"; Fail "parse")
  end

  type pos = int

  structure StreamWithPos =
     CoercedStreamable (structure Streamable = StreamStreamable
                        type 'a item = 'a * pos
                        fun coerce (x, _) = x)

  structure ParseMain = VSIMPLParseFun (
    structure Streamable = StreamWithPos
    structure Arg = Arg
  )

  fun parse s = #1 (ParseMain.parse (Lexer.lex s))
end
