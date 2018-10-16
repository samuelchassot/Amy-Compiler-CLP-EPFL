package amyc.test

import amyc.parsing._
import org.junit.Test

class LexerTests extends TestSuite {
  val pipeline = Lexer andThen DisplayTokens

  val baseDir = "lexer"

  val outputExt = "txt"

  @Test def testKeywords = shouldOutput("Keywords")

  @Test def testSingleAmp = shouldFail("SingleAmp")

  @Test def testUnclosedComment = shouldFail("UnclosedComment")

  @Test def testUnclosedStringLit = shouldFail("UnclosedStringLit")

  @Test def testIntLits = shouldOutput("IntLits")

  @Test def testIntLitsTooBig = shouldFail("IntLitsTooBig")

  @Test def testStringLit = shouldOutput("StringLit")

  @Test def testuCom = shouldOutput("uCom")
  //@Test def testUnclosedString = shouldOutput("StringLitUnclosed")
}
