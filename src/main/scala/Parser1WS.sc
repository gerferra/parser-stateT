object Parser1WS {

  import Parser1._

  println(parse(pExpr, "1"))                      //> [1.2] parsed: 1
  println(parse(pExpr, "2+2"))                    //> [1.4] parsed: 4
  println(parse(pExpr, "3*3+4*4*(1+2)"))          //> [1.14] parsed: 57

  println(parse(pL1, """1;
                       |2+2;
                       |3*3+4*4*(1+2);""".stripMargin))
                                                  //> [3.15] parsed: List(1, 4, 57)

  println(parse(pL1, "1; 2+2; 3*3+4*4*(1+2);"))   //> [1.23] parsed: List(1, 4, 57)

}