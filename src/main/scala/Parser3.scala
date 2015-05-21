
import scala.util.parsing.combinator._
import scala.util.parsing.input.Position
import scalaz._, Scalaz._

object Parser3 extends RegexParsers {

  type SymTab = Map[String, Int]

  sealed abstract class Err(override val toString: String)
  case class NotDeclared(name: String) extends Err(s"`$name' not declared.")
  case class AlreadyDeclared(name: String, value: Int) extends Err(
    s"`$name' already declared with value `$value'.")

  type V[A] = Err \/ A
  type S[A] = State[SymTab, V[A]]
  type P[A] = Parser[S[A]]

  /*
   * The commented out parsers weren't adapted.
   */

  /*
  def pL3: P[List[Int]] = rep(pOptExpr <~ ";") ^^ {
    listS =>
      for {
        list <- listS.sequence
      } yield {
        list.flatten
      }
  }

  def pOptExpr: P[Option[Int]] = pVar ^^ (_.map(_ => none[Int])) | pExpr ^^ (_.map(_.some))
*/
  def pVar: P[Unit] = "var" ~ pName ~ "=" ~ pExpr ^^ {
    case _ ~ name ~ _ ~ exprS =>
      for {
        exprV <- exprS
        symtab <- State.get[SymTab]
        newSymTab = {
          for {
            expr <- exprV
            s <- symtab.get(name) match {
              case Some(v) => AlreadyDeclared(name, v).left // if the name exists, creates a left with the error
              case None => symtab.right // if not exists, returns the symtab as a right
            }
          } yield {
            s + (name -> expr)
          }
        }
        res <- newSymTab match {
          case -\/(l) => State.state[SymTab, V[Unit]](l.left) // if the newSymTab is a left, I need to put the error in the State
          case \/-(r) => State.put(r).map(_.right) // if the newSymTab is a right, I need to use it a the new state, and the call map to put the () inside an \/ 
        }
      } yield {
        res
      }
  }

  def pExpr = pAdd

  def pAdd: P[Int] =
    pProd ~ "+" ~ pAdd ^^ {
      case leftS ~ _ ~ rightS =>
        for {
          leftV <- leftS
          rightV <- rightS
        } yield {
          for {
            left <- leftV
            right <- rightV
          } yield {
            left + right
          }
        }
    } |
      pProd

  def pProd: P[Int] =
    pAtom ~ "*" ~ pProd ^^ {
      case leftS ~ _ ~ rightS =>
        for {
          leftV <- leftS
          rightV <- rightS
        } yield {
          for {
            left <- leftV
            right <- rightV
          } yield {
            left * right
          }
        }
    } |
      pAtom

  def pAtom: P[Int] =
    pRef |
      """\d+""".r ^^ (x => State.state[SymTab, V[Int]](x.toInt.right)) |
      "(" ~> pExpr <~ ")"

  def pRef: P[Int] = pName ^^ {
    name =>
      for {
        symtab <- State.get
      } yield {
        symtab.get(name).toRightDisjunction(NotDeclared(name))
      }
  }

  def pName: Parser[String] = """[A-Za-z_][A-Za-z0-9_]*""".r

}

object Parser3Test extends App {

  import Parser3._

  println(parse(pExpr, "1").map(_.run(Map.empty)))
  println(parse(pExpr, "2+2").map(_.run(Map.empty)))
  println(parse(pExpr, "3*3+4*4*(1+2)").map(_.run(Map.empty)))
  println(parse(pExpr, "3*3*3+4*4*(1+2)+5").map(_.run(Map.empty)))
  /*
  println(parse(pL3, """1; 
                       |2+2;
                       |3*3+4*4*(1+2);
                       |3*3*3+4*4*(1+2)+5;""".stripMargin).map(_.run(Map.empty)))

  println(parse(pL3, "1; 2+2; 3*3+4*4*(1+2); 3*3*3+4*4*(1+2)+5;").map(_.run(Map.empty)))
*/
  println(parse(pVar, "var x = 1".stripMargin).map(_.run(Map.empty)))
  println(parse(pExpr, "y * 3".stripMargin).map(_.run(Map("y" -> 2))))
  /*
  println(parse(pL3, """var x = 1; 
                       |var y = x * 3;
                       |x + y;
                       |(x + 1) * 3 +1;
                       |var z = 8;""".stripMargin).map(_.run(Map.empty)))

  parse(pL3, "var x = y;").map(_.run(Map.empty))

*/
}