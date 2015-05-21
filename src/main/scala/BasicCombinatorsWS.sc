object BasicCombinatorsWS {

  import BasicCombinators._

  parse(pA, "A")                                  //> res0: BasicCombinators.ParseResult[Char] = [1.2] parsed: A
  parse(pB, "A")                                  //> res1: BasicCombinators.ParseResult[Char] = [1.1] failure: `B' expected but A 
                                                  //| found
                                                  //| 
                                                  //| A
                                                  //| ^
  parse(pB, "B")                                  //> res2: BasicCombinators.ParseResult[Char] = [1.2] parsed: B
  parse(pAlt, "A")                                //> res3: BasicCombinators.ParseResult[Char] = [1.2] parsed: A
  parse(pAlt, "B")                                //> res4: BasicCombinators.ParseResult[Char] = [1.2] parsed: B
  parse(pSeq, "A")                                //> res5: BasicCombinators.ParseResult[BasicCombinators.~[Char,Char]] = [1.2] fa
                                                  //| ilure: end of input
                                                  //| 
                                                  //| A
                                                  //|  ^
  parse(pSeq, "AB")                               //> res6: BasicCombinators.ParseResult[BasicCombinators.~[Char,Char]] = [1.3] pa
                                                  //| rsed: (A~B)

  parse(pRep, "")                                 //> res7: BasicCombinators.ParseResult[List[Char]] = [1.1] parsed: List()
  parse(pRep, "AAAAA")                            //> res8: BasicCombinators.ParseResult[List[Char]] = [1.6] parsed: List(A, A, A,
                                                  //|  A, A)
  parse(pOpt, "")                                 //> res9: BasicCombinators.ParseResult[Option[Char]] = [1.1] parsed: None
  parse(pOpt, "A")                                //> res10: BasicCombinators.ParseResult[Option[Char]] = [1.2] parsed: Some(A)
  parse(pOpt, "B")                                //> res11: BasicCombinators.ParseResult[Option[Char]] = [1.1] parsed: None
}