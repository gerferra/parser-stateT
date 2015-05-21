import scala.util.parsing.combinator._

object BasicCombinators extends Parsers 
                                  with RegexParsers {

  def pA  : Parser[Char]         = 'A'
  def pB  : Parser[Char]         = 'B'
  def pAlt: Parser[Char]         = pA | pB
  def pSeq: Parser[Char ~ Char]  = pA ~ pB
  def pRep: Parser[List[Char]]   = rep(pA)
  def pOpt: Parser[Option[Char]] = opt(pA)
}
