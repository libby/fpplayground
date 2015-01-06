object Log {
    val isDebug = false
    def debug(s: String) = if(isDebug) println(s)
}
// "abc" => ('a',""bc)
case class State[S,A](run: S => (A,S))

// needs some input => input output
trait ParserValue[+A] {
    val value: Option[A]
    val previousMatches: List[A]
    def endOfInput: Boolean
}
case class Match[A](value: Option[A], previousMatches: List[A], msg: String = "default message") extends ParserValue[A] {
    def endOfInput = false
}
case object ZeroState extends ParserValue[Nothing] {
    def endOfInput = false
    val value = None
    val previousMatches = Nil
}
case class ParserError[A](previousMatches: List[A], errorMsg: String) extends ParserValue[A] {
    def endOfInput = false
    val value = None
}
case class End[A](value: Option[A], previousMatches: List[A]) extends ParserValue[A] {
    def endOfInput = true
}

// case class ParserGeneral[Input, A](parse: State[ Input, ParserValue[A] ])
// type Parser[A] = ParserGeneral[String, A]

/**
 * Parser is a State Monad 
 */
case class Parser[A](parse: State[ (String, List[A]), ParserValue[A] ]) {
    
    // def map2(parser1: Parser[A], parser2: Parser[A])(f: (A,A) => A): Parser[A]

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
                      case (p1ParserVal@Match(v, prev, msg), p1NextInput) => {
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
                val allMatches = (prev :+ c)
                if (input.isEmpty) {
                    val endState = End(Some(c), allMatches)
                    (endState, (input, allMatches) )   
                }
                else if (input(0) == c) {
                    val matchState = Match(Some(c), allMatches, s"char fun ${input.substring(1)}")
                    (matchState -> (input.substring(1), allMatches) )
                }
                else {
                    val parseError = ParserError(allMatches, s"could not parse char ${c} in string ${input}")
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
        println(s" zeroThenCharVal == justCharVal : ${zeroThenCharVal == justCharVal}")
        println(s" charThenZeroVal == justCharVal : ${charThenZeroVal == justCharVal}")
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
        parserMatchVal.isInstanceOf[Match[String]] == true
        parserMissVal.isInstanceOf[ParserError[String]] == true
    }
    
}