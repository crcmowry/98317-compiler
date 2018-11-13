structure Compile : sig
  val compile : VSIMPL.program -> Assembly.program
end = struct
  structure V = VSIMPL 
  structure A = Assembly

  (*  fresh () ==> a string which has not previously been returned by this
   *  function, and which could not have appeared in the source program.
   *)
  local val c = ref 0 in
    fun fresh_label () = (c := !c + 1; !c)
  end
  fun fresh_var () = "%t" ^ Int.toString (fresh_label ())

  datatype arith = ADD | SUB | MUL | DIV | EQ | LT

  (* If you want any data structures look in the cmlib directory *)

  fun compile (params, body) =
    let
      fun arith (x : string) (a1 : V.aexp, a2 : V.aexp) (oper : arith) : A.instruction list =
        let
          val s1 = fresh_var ()
          val s2 = fresh_var ()
          val i1 = compile_arithmetic s1 a1
          val i2 = compile_arithmetic s2 a2
          val (t1, t2) = (A.Var s1, A.Var s2)
        in
          i1 @ i2 @ ((case oper of
                           ADD => A.ADD (x, (t1, t2))
                         | SUB => A.SUB (x, (t1, t2))
                         | MUL => A.MUL (x, (t1, t2))
                         | DIV => A.DIV (x, (t1, t2))
                         | EQ => A.EQ (x, (t1, t2))
                         | LT => A.LT (x, (t1, t2)))::[])
        end

      and compile_arithmetic (x : string) (ae : V.aexp) : A.instruction list =
        let
        in
          case ae of
               V.AConst c => (A.MOV (x, A.Const c))::[]
             | V.Var y => (A.MOV (x, A.Var y))::[]
             | V.Plus (a1, a2) => arith x (a1, a2) ADD
             | V.Minus (a1, a2) => arith x (a1, a2) SUB
             | V.Times (a1, a2) => arith x (a1, a2) MUL
             | V.DividedBy (a1, a2) => arith x (a1, a2) DIV
        end

      fun compile_boolean (x : string) (be : V.bexp) : A.instruction list =
        case be of
             V.BConst true => A.MOV(x, (A.Const 1))::[]
           | V.BConst false => A.MOV(x, (A.Const 0))::[]
           | V.Equals (a1, a2) => arith x (a1, a2) EQ
           | V.NotEquals (a1, a2) => compile_boolean (x) (V.Not (V.Equals (a1, a2)))
           | V.LessThan (a1, a2) => arith x (a1, a2) LT
           | V.GreaterThan (a1, a2) => arith x (a2, a1) LT
           | V.AndAlso (b1, b2) => raise Fail "Unimplemented"
           | V.OrElse (b1, b2) => raise Fail "Unimplemented"
           | V.Not b1 =>
               let
                 val t = fresh_var ()
                 val i = compile_boolean t b1
               in
                 raise Fail "Unimplemented"
               end

      fun compile' (cmds : V.cmd list) : A.instruction list =
        case (cmds) of
             [] => []
           | c::cs =>
               let
                 val i = (case c of
                               V.Assign (x, ae) => (compile_arithmetic x ae)
                             | V.If (b, c1, c2) =>
                                 let
                                   val l1 = fresh_label ()
                                   val l2 = fresh_label ()
                                   val x = fresh_var ()
                                   val ib = compile_boolean x b
                                   val cj = A.IF (A.Var x, l1, l2)
                                   val i1 = compile' c1
                                   val i2 = compile' c2
                                 in
                                   ib @ (cj::(A.Label l1)::i1)
                                      @ ((A.Label l2)::i2)
                                 end
                             | V.While (b, c') => raise Fail "unimplemented"
                             | V.Return (ae) =>
                                 let
                                   val x = fresh_var ()
                                 in
                                   (compile_arithmetic x ae) @
                                   ((A.RET (A.Var x))::[])
                                 end)
                 val is = compile' cs
               in
                 i @ is
               end
    in
      (params, compile' body)
    end
end
