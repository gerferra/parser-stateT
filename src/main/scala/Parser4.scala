
import scala.util.parsing.combinator._
import scalaz._, Scalaz._

object Parser4 extends RegexParsers {

  type SymTab = Map[String, Int]

  sealed abstract class Err(override val toString: String)
  case class NotDeclared(name: String) extends Err(s"`$name' not declared.")
  case class AlreadyDeclared(name: String, value: Int) extends Err(
    s"`$name' already declared with value `$value'.")

  type V[A] = Err \/ A

  type S[A] = StateT[V, SymTab, A]
  type P[A] = Parser[S[A]]

  val State = StateT.stateTMonadState[SymTab, V]

  def pL4: P[List[Int]] = rep(pOptExpr <~ ";") ^^ {
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
        unit <- symtab.get(name) match {
          case Some(v) => StateT[V, SymTab, Unit] { s =>
            AlreadyDeclared(name, v).left
          }
          case None => State.put(symtab + (name -> expr))
        } 
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
      """\d+""".r ^^ (x => State.state(x.toInt)) |
      "(" ~> pExpr <~ ")"

  def pRef: P[Int] = pName ^^ {
    name =>
      for {
        symtab <- State.get
        vErrInt = symtab.get(name).toRightDisjunction(NotDeclared(name))
        res <- StateT[V, SymTab, Int] { s =>
          vErrInt.map(s -> _)
        }
      } yield {
        res
      }
  }

  def pName: Parser[String] = """[A-Za-z_][A-Za-z0-9_]*""".r

}

object Parser4Test extends App {

  import Parser4._

  
  println(parse(pExpr, "1").map(_.run(Map.empty)))
  println(parse(pExpr, "2+2").map(_.run(Map.empty)))
  println(parse(pExpr, "3*3+4*4*(1+2)").map(_.run(Map.empty)))
  println(parse(pExpr, "3*3*3+4*4*(1+2)+5").map(_.run(Map.empty)))

  println(parse(pL4, """1; 
                       |2+2;
                       |3*3+4*4*(1+2);
                       |3*3*3+4*4*(1+2)+5;""".stripMargin).map(_.run(Map.empty)))

  println(parse(pL4, "1; 2+2; 3*3+4*4*(1+2); 3*3*3+4*4*(1+2)+5;").map(_.run(Map.empty)))

  println(parse(pVar, "var x = 1".stripMargin).map(_.run(Map.empty)))
  println(parse(pExpr, "y * 3".stripMargin).map(_.run(Map("y" -> 2))))

  println(parse(pL4, """var x = 1; 
                       |var y = x * 3;
                       |x + y;
                       |(x + 1) * 3 +1;
                       |var z = 8;""".stripMargin).map(_.run(Map.empty)))

  println(parse(pL4, "var x = y;").map(_.run(Map.empty))) 
  println(parse(pL4, "var x = 1; x * x; var x = 2; ").map(_.run(Map.empty))) 

}

