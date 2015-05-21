object Parser4WS {

  import Parser4._

  println(parse(pExpr, "1").map(_.run(Map.empty)))//> [1.2] parsed: \/-((Map(),1))
  println(parse(pExpr, "2+2").map(_.run(Map.empty)))
                                                  //> [1.4] parsed: \/-((Map(),4))
  println(parse(pExpr, "3*3+4*4*(1+2)").map(_.run(Map.empty)))
                                                  //> [1.14] parsed: \/-((Map(),57))
  println(parse(pExpr, "3*3*3+4*4*(1+2)+5").map(_.run(Map.empty)))
                                                  //> [1.18] parsed: \/-((Map(),80))

  println(parse(pL4, """1;
                       |2+2;
                       |3*3+4*4*(1+2);
                       |3*3*3+4*4*(1+2)+5;""".stripMargin).map(_.run(Map.empty)))
                                                  //> [4.19] parsed: \/-((Map(),List(1, 4, 57, 80)))

  println(parse(pL4, "1; 2+2; 3*3+4*4*(1+2); 3*3*3+4*4*(1+2)+5;").map(_.run(Map.empty)))
                                                  //> [1.42] parsed: \/-((Map(),List(1, 4, 57, 80)))

  println(parse(pVar, "var x = 1").map(_.run(Map.empty)))
                                                  //> [1.10] parsed: \/-((Map(x -> 1),()))
  println(parse(pExpr, "y * 3").map(_.run(Map("y" -> 2))))
                                                  //> [1.6] parsed: \/-((Map(y -> 2),6))

  println(parse(pL4, """var x = 1;
                       |var y = x * 3;
                       |x + y;
                       |(x + 1) * 3 +1;
                       |var z = 8;""".stripMargin).map(_.run(Map.empty)))
                                                  //> [5.11] parsed: \/-((Map(x -> 1, y -> 3, z -> 8),List(4, 7)))
                                                  
  println(parse(pL4, "var x = y;").map(_.run(Map.empty)))
                                                  //> [1.11] parsed: -\/(`y' not declared.)
  println(parse(pL4, "var x = 1; x * x; var x = 2; ").map(_.run(Map.empty)))
                                                  //> [1.29] parsed: -\/(`x' already declared with value `1'.)

}