structure Top =
struct
  fun parse file =
  let
    val ins = TextIO.openIn file
  in
    Parser.parse (Stream.fromTextInstream ins)
    before
    TextIO.closeIn ins
  end

  fun compile file =
  let
    val program = parse file
    val () = print "Finished parsing source code.\n"
    val () = Statics.check program
    val () = print "Finished checking program statics (if you've implemented the statics).\n"
    val program' = Compile.compile program
    val () = print "Finished compiling program.\n"
  in
    program'
  end handle Statics.Error msg => (
    print ("Statics error: " ^ msg ^ "\n");
    raise Fail "statics"
  )

  fun compile_and_print file =
    let val program' = compile file in
      print (Assembly.pp_program program')
    end

  fun compile_and_run file args =
    let
      val program' = compile file
      val () = print "Running compiled program...\n"
      val (steps, result) = Assembly.run program' args
    in
      print ("Took " ^ Int.toString steps ^ " steps to run.\n");
      print (case result of
        Assembly.UnassignedVariable =>
          "Program execution ended early due to use of unassigned variable.\n"
      | Assembly.UnresolvedJump =>
          "Program execution ended early due to jump to nonexistent label.\n"
      | Assembly.NoReturn =>
          "Program execution ended early due to lack of return instruction.\n"
      | Assembly.DivideByZero =>
          "Program resulted in division by zero error.\n"
      | Assembly.Value n =>
          "Program returned " ^ Int.toString n ^ ".\n"
      )
    end

end
