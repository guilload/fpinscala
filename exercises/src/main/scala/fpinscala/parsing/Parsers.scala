package fpinscala.parsing

import language.higherKinds


trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  case class ParserOps[A](p: Parser[A]) {
  }

  object Laws {
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line: Int = input.take(offset).count(_ == '\n') + 1
  lazy val col: Int = input.take(offset).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.lengthCompare(1) > 0) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(), otherFailures: List[ParseError] = List()) {
}