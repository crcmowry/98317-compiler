structure Assembly = 
struct
  type label = int
  datatype operand =
    Const of int
  | Var of string
  datatype instruction = 
    Label of label
  | MOV of string * operand
  | ADD of string * (operand * operand)
  | SUB of string * (operand * operand)
  | MUL of string * (operand * operand)
  | DIV of string * (operand * operand)
  | EQ of string * (operand * operand)
  | LT of string * (operand * operand)
  | JUMP of label
  | IF of operand * label * label
  | RET of operand
  type program = string list * instruction list

  infix 6 ^^
  fun s1 ^^ s2 = s1 ^ " " ^ s2

  val pp_operand = (fn Const n => Int.toString n | Var x => x)

  val pp_instruction = (fn
    Label l => Int.toString l ^ ":"
  | MOV (d, s) => "  " ^ d ^^ "<-" ^^ pp_operand s
  | ADD (d, (s1, s2)) => "  " ^ d ^^ "<-" ^^ pp_operand s1 ^^ "+" ^^ pp_operand s2
  | SUB (d, (s1, s2)) => "  " ^ d ^^ "<-" ^^ pp_operand s1 ^^ "-" ^^ pp_operand s2
  | MUL (d, (s1, s2)) => "  " ^ d ^^ "<-" ^^ pp_operand s1 ^^ "*" ^^ pp_operand s2
  | DIV (d, (s1, s2)) => "  " ^ d ^^ "<-" ^^ pp_operand s1 ^^ "/" ^^ pp_operand s2
  | EQ (d, (s1, s2)) => "  " ^ d ^^ "<-" ^^ pp_operand s1 ^^ "==" ^^ pp_operand s2
  | LT (d, (s1, s2)) => "  " ^ d ^^ "<-" ^^ pp_operand s1 ^^ "<" ^^ pp_operand s2
  | JUMP l => "  JUMP " ^ Int.toString l
  | IF (s, l1, l2) => "  IF" ^^ pp_operand s ^^ "THEN" ^^ Int.toString l1 ^^ "ELSE" ^^ Int.toString l2
  | RET s => "  RET" ^^ pp_operand s
  )
  
  fun pp_program (params, body) =
    "main (" ^ String.concatWith ", " params ^ ") =\n  " ^
    String.concatWith "\n  " (map pp_instruction body) ^ "\n"

  structure State = StringRedBlackDict
  structure Locs = IntRedBlackDict
  datatype result =
    UnassignedVariable
  | UnresolvedJump
  | NoReturn
  | DivideByZero
  | Value of int
  (* returns (number of steps it took to run, result) *)
  local
    fun ++n = n + 1
    fun run_instructions locs state n =
      let val ? = fn Const n => n | Var x => State.lookup state x in fn
        [] => (n, NoReturn)
      | instr::instrs => (
          case instr of
            Label _ => raise Fail "impossible"
          | MOV (x, s) =>
              run_instructions locs (State.insert state x (?s)) (++n) instrs
          | ADD (x, (s1, s2)) =>
              run_instructions locs (State.insert state x (?s1 + ?s2)) (++n) instrs
          | SUB (x, (s1, s2)) =>
              run_instructions locs (State.insert state x (?s1 - ?s2)) (++n) instrs
          | MUL (x, (s1, s2)) =>
              run_instructions locs (State.insert state x (?s1 * ?s2)) (++n) instrs
          | DIV (x, (s1, s2)) =>
              run_instructions locs (State.insert state x (?s1 div ?s2)) (++n) instrs
          | EQ (x, (s1, s2)) =>
              if ?s1 = ?s2 then
                run_instructions locs (State.insert state x 1) (++n) instrs
              else
                run_instructions locs (State.insert state x 0) (++n) instrs
          | LT (x, (s1, s2)) =>
              if ?s1 < ?s2 then
                run_instructions locs (State.insert state x 1) (++n) instrs
              else
                run_instructions locs (State.insert state x 0) (++n) instrs
          | JUMP l =>
              run_instructions locs state (++n) (Locs.lookup locs l)
          | IF (s, l1, l2) =>
              if ?s <> 0 then
                run_instructions locs state (++n) (Locs.lookup locs l1)
              else
                run_instructions locs state (++n) (Locs.lookup locs l2)
          | RET s => (n, Value (?s))
        ) handle
            State.Absent => (n, UnassignedVariable)
          | Locs.Absent => (n, UnresolvedJump)
          | Div => (n, DivideByZero)
      end
  in
    fun run (params, body) args =
      let
        val state = (
          ListPair.foldlEq
          (fn (x, v, state) => State.insert state x v)
          State.empty
          (params, args)
        ) handle ListPair.UnequalLengths =>
          raise Fail "Incorrect number of arguments."
        val (start, locs) = foldr (fn (instr, (instrs, locs)) =>
          case instr of
            Label l => (instrs, Locs.insert locs l instrs)
          | _ => (instr::instrs, locs)
        ) ([], Locs.empty) body
      in
        run_instructions locs state 0 start
      end
  end
end