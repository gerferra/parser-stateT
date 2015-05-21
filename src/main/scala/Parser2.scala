
import scala.util.parsing.combinator._
import scalaz._, Scalaz._

object Parser2 extends RegexParsers {

  type SymTab = Map[String, Int]
  type S[A] = State[SymTab, A]
  type P[A] = Parser[S[A]]

  def pL2: P[List[Int]] = rep(pOptExpr <~ ";") ^^ {
    listS =>
      for {
        list <- listS.sequence
      } yield {
        list.flatten
      }
  }

  def pOptExpr: P[Option[Int]] = pVar ^^ (_.map(_ => none[Int])) | pExpr ^^ (_.map(_.some))

  def pVar: P[Unit] = "var" ~ pName ~ "=" ~ pExpr ^^ {
    case _ ~ name ~ _ ~ exprS =>
      for {
        expr <- exprS
        symtab <- State.get
        unit <- State.put(symtab + (name -> expr))
      } yield {
        unit
      }
  }

  def pExpr = pAdd

  def pAdd: P[Int] =
    pProd ~ "+" ~ pAdd ^^ {
      case leftS ~ _ ~ rightS =>
        for {
          left <- leftS
          right <- rightS
        } yield {
          left + right
        }
    } |
      pProd

  def pProd: P[Int] =
    pAtom ~ "*" ~ pProd ^^ {
      case leftS ~ _ ~ rightS =>
        for {
          left <- leftS
          right <- rightS
        } yield {
          left * right
        }
    } |
      pAtom

  def pAtom: P[Int] =
    pRef |
      """\d+""".r ^^ (x => State.state[SymTab, Int](x.toInt)) |
      "(" ~> pExpr <~ ")"

  def pRef: P[Int] = pName ^^ {
    name =>
      for {
        symtab <- State.get
      } yield {
        symtab(name)
      }
  }

  def pName: Parser[String] = """[A-Za-z_][A-Za-z0-9_]*""".r

}

object Parser2Test extends App {

  import Parser2._

  println(parse(pExpr, "1").map(_.run(Map.empty)))
  println(parse(pExpr, "2+2").map(_.run(Map.empty)))
  println(parse(pExpr, "3*3+4*4*(1+2)").map(_.run(Map.empty)))
  println(parse(pExpr, "3*3*3+4*4*(1+2)+5").map(_.run(Map.empty)))

  println(parse(pL2, """1; 
                       |2+2;
                       |3*3+4*4*(1+2);
                       |3*3*3+4*4*(1+2)+5;""".stripMargin).map(_.run(Map.empty)))

  println(parse(pL2, "1; 2+2; 3*3+4*4*(1+2); 3*3*3+4*4*(1+2)+5;").map(_.run(Map.empty)))

  println(parse(pVar, "var x = 1".stripMargin).map(_.run(Map.empty)))
  println(parse(pExpr, "y * 3".stripMargin).map(_.run(Map("y" -> 2))))

  println(parse(pL2, """var x = 1; 
                       |var y = x * 3;
                       |x + y;
                       |(x + 1) * 3 +1;
                       |var z = 8;""".stripMargin).map(_.run(Map.empty)))

  parse(pL2, "var x = y;").map(_.run(Map.empty))                       
}
