
import scala.util.parsing.combinator._

object Parser1 extends RegexParsers {
  
  def pL1: Parser[List[Int]] = rep(pExpr <~ ";") 

  def pExpr = pAdd

  def pAdd: Parser[Int] =
    pProd ~ "+" ~ pAdd ^^ {
      case left ~ _ ~ right => left + right
    } |
      pProd

  def pProd: Parser[Int] =
    pAtom ~ "*" ~ pProd ^^ {
      case left ~ _ ~ right => left * right
    } |
      pAtom

  def pAtom: Parser[Int] =
    """\d+""".r ^^ (_.toInt) |
      "(" ~> pExpr <~ ")"

}

object Parser1Test extends App {

  import Parser1._

  println(parse(pExpr, "1"))
  println(parse(pExpr, "2+2"))
  println(parse(pExpr, "3*3+4*4*(1+2)"))
  println(parse(pExpr, "3*3*3+4*4*(1+2)+5"))
  
  println(parse(pL1, """1; 
                       |2+2;
                       |3*3+4*4*(1+2);
                       |3*3*3+4*4*(1+2)+5;""".stripMargin))
  
  println(parse(pL1, "1; 2+2; 3*3+4*4*(1+2); 3*3*3+4*4*(1+2)+5;"))
}