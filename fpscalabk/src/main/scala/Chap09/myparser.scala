object Log {
    val isDebug = false
    def debug(s: String) = if(isDebug) println(s)
}
// "abc" => ('a',""bc)
case class State[S,A](run: S => (A,S))

// needs some input => input output
trait ParserValue[+A] {
    val value: Option[A]
}
case class Match[A](value: Option[A], msg: String = "default message") extends ParserValue[A] 
/**
 * used for fold as the initial value, if the ZeroState is used in the zero parser as the unit state, i.e. zero element of addition == 0
 * 
 */ 
case object ZeroState extends ParserValue[Nothing] {
    val value = None
}
case class ParserError[A](errorMsg: String) extends ParserValue[A] {
    val value = None
}
case class EOF[A](value: Option[A]) extends ParserValue[A]

// case class ParserGeneral[Input, A](parse: State[ Input, ParserValue[A] ])
// type Parser[A] = ParserGeneral[String, A]
object alias {
    type ParserState[A] =  State[ (String, List[A]), ParserValue[A] ]
}

/**
 * Parser is a State Monad 
 */
case class Parser[A](parse: State[ (String, List[String]), ParserValue[A] ]) {

    import alias._   
    
    //def map2[B,C](pb:Parser[B])(f: (A,B) => C ): Parser[C]
    
    // def map2(parser1: Parser[A], parser2: Parser[A])(f: (A,A) => A): Parser[A]
    def flatMap[B](f: ParserValue[A] => Parser[B]): Parser[B] = Parser[B] {
        State ( {
                    case (input, prevMatches) => {
                        //val (parserVal, (nextInput, _) ) = this.parse.run((input, Nil))
                        val (parserVal, nextState ) = this.parse.run((input, prevMatches))
                        f(parserVal).parse.run( nextState ) //parserVal.previousMatches
                    }
                }
              )
    }
    
    def pred(p: Char => Boolean): Parser[A] = Parser[A] {
        State({
            
            case (input, prevMatches) if (!input.isEmpty) => 
                val c = input(0)
                if ( p(c) ) this.parse.run((input, prevMatches))
                else (ParserError("predicate did not pass! =("), (input, prevMatches))
                    
            case (input, prevMatches) => (ParserError("input what emtpy when testing predicate! =("), (input, prevMatches))
            
        })
    }
    
    // this succeeds breark else try p2
    def |(p2: Parser[A]): Parser[A] = or(p2)
     
    def or(p2: Parser[A]): Parser[A] = Parser[A]{
        State({
                case (inputStr, prevMatches) => 
                    this.parse.run((inputStr, prevMatches)) match {
                        // error try the next guy
                        case (error@ParserError(msg), _) => p2.parse.run((inputStr,prevMatches))
                        // anything else, just return the value
                        case p1Res@_ => p1Res
                    }
              }
          )
    }
    
    // alias for chain
    def >>(p2: Parser[A]): Parser[A] = chain(p2)
    
    /**
     * parserA.chain(zeroParser) and zeroParser.chain(parserA) should both
     * return parserA unchanged.
     * @param p2 the parser you apply after this parser has been applied.
     * @return a Parser when run that will execute this parser and the passed in parser.
     */
    def chain(p2: Parser[A]): Parser[A] =
        Parser[A] ({
            State (
                input => { // (A,S)
                  this.parse.run(input) match {
                      case (p1ParserVal@Match(v, msg), p1NextInput) => {
                          Log.debug(s"parse value is Match ${p1ParserVal} msg is ${msg}")
                          // if p == ZeroParser parserVal
                          p2.parse.run(p1NextInput) match {
                              // ignore if ZeroState
                              case (_@ZeroState, _) => {
                                  Log.debug(s"p2 is the ZeroState return p1 (${p1ParserVal}, ${p1NextInput})")
                                  (p1ParserVal, p1NextInput)
                              }
                              case (p2ParserVal, p2NextInput) => {
                                  Log.debug(s"p2 is NOT ZERO return p2 (${p2ParserVal}, ${p2NextInput})")
                                  (p2ParserVal, p2NextInput)
                              }
                          }
                      }
                      case (parserVal@ZeroState, nextInput) => {
                          Log.debug(s"parse value is not startState ${parserVal}")
                          p2.parse.run(nextInput)

                      }
                      case (parserVal, nextInput) => {
                          Log.debug(s"parse value is not continue ${parserVal}")
                          (parserVal, nextInput)
                      }
                  }
                }
            )
        })
        // compose get value from A and pass it up to the function which will return B
        // char('c') { nextState => char('d')(nextState) }
        // State (
        //         input => {
        //             val (parserVal, nextInput) = this.parse.run(input)
        //             (f(parserVal), nextInput)
        //         }
        //       )

}

object Parser {
    
    // convience function so that run and a parse can be called from passing a String in stead of a Tuple[String,List[String]]
    // when running a parser for the first time, the List of matched char is the Empty List.
    implicit def string2ParserState[A](input:  String): (String, List[A]) = (input -> List[A]())
    
    // def unit[A](a: A): Parser[A] = Parser[A] {
    //         State( { case (input, prev) => (Match(Some(a), List(a)), (input, Nil)) } )
    //     }
    
    //
    def z[A]: Parser[A] = Parser[A](State {
        input => (ZeroState, input)
    })
    
    /**
     * "aababa" parse a single char
     * char is the "unit" function for Parser[Char]
     */
     def char(c: Char): Parser[Char] = Parser[Char] { 
         State({
             case (input, prev) =>
                val allMatches = (prev :+ c.toString)
                if (input.isEmpty) {
                    val endState = EOF(Some(c))
                    (endState, (input, allMatches) )   
                }
                else if (input(0) == c) {
                    val matchState = Match(Some(c), s"char fun ${input.substring(1)}")
                    (matchState -> (input.substring(1), allMatches) )
                }
                else {
                    val parseError = ParserError(s"could not parse char ${c} in string ${input}")
                    (parseError -> (input, allMatches))
                }
         })
     }
     
     // parse: State[ String, ParserValue[A] ]
     def string(s: String): Parser[Char] = chars(s.toList:_*)
     
     def char2(c1: Char, c2: Char): Parser[Char] = char(c1).chain(char(c2))
        
     def chars(cs: Char*): Parser[Char] = cs.foldRight(Parser.z[Char])( (c1, p2) => char(c1).chain(p2) )
          
}

object Test {
    import Parser._
    def test() = {
        val p = Parser.chars('a','b','c')
        val pEnd = p.parse.run( ("abc" -> Nil) )
        println(s"on parser 'a','b','c' parsing abc should yield abc ${p} ${pEnd}")
        val pError = p.parse.run( ("abdc" -> Nil) )
        println(s"on parser 'a','b','c' parsing abdc should yield error ${pError}")
    }
    
    def testId() = {
        println(s"Testing the identity")
        val zeroThenChar = Parser.z[Char].chain(Parser.char('a'))
        val charThenZero = Parser.char('a').chain(Parser.z)
        val justChar = Parser.char('a')
        val s = "a"
        val justCharVal = justChar.parse.run((s -> Nil))
        val zeroThenCharVal = zeroThenChar.parse.run((s -> Nil))
        val charThenZeroVal = charThenZero.parse.run((s -> Nil))
        println(s"zeroThenCharVal == justCharVal : ${zeroThenCharVal == justCharVal}")
        println(s"charThenZeroVal == justCharVal : ${charThenZeroVal == justCharVal}")
        println(s"zeroThenCharVal was ${zeroThenCharVal}")
        println(s"charThenZeroVal was ${charThenZeroVal}")
    }
    
    def testString() = {
        val stringParserText = "this is a test"
        val inputMatch = "this is a test with some other stuff"
        val inputMiss = "this is not a test"
        val strParser = Parser.string(stringParserText).parse
        val (parserMatchVal, matchState) = strParser.run( (inputMatch -> Nil) )
        val (parserMissVal, missState)   = strParser.run( (inputMiss -> Nil) )
        // parserMatchVal.isInstanceOf[Match[String]] == true
        // parserMissVal.isInstanceOf[ParserError[String]] == true
    }
    
    def testOr() = {
        val pa = Parser.char('a') 
        val pb = Parser.char('b')
        val inputB = "basomestuff"
        val inputA = "absomestuff"
        val t1 = pa.or(pb).parse.run(inputB)._1.value.isDefined == true
        val t2 = pb.or(pa).parse.run(inputB)._1.value.isDefined == true
        val t3 = pb.or(pa).parse.run(inputA)._1.value.isDefined == true
        val t4 = pa.or(pb).parse.run(inputA)._1.value.isDefined == true
        println("All test passed " + List(t1,t2,t3,t4).forall(_ == true))
    }
    
}