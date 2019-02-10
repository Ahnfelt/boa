package com.github.ahnfelt.boa

import scala.collection.mutable.ListBuffer


object Tokenizer {

    def tokenize(code : String) : Array[Token] = {
        var offset = 0
        var lineOffset = 0
        var line = 1
        var indentation = 0
        var bracketStack = List[(Bracket, Int)]()
        val tokens = ListBuffer[Token]()

        def ahead(sequenceOfAlternatives : String*) : Boolean =
            if(offset + sequenceOfAlternatives.size > code.length) false else {
                sequenceOfAlternatives.zipWithIndex.forall { case (a, i) =>
                    a.contains(code(offset + i))
                }
            }

        def fail(problem : String) =
            throw new RuntimeException(problem + " at line " + line + " column " + (offset - lineOffset + 1))

        def handleIndentation() = {
            val newIndentation = offset - lineOffset
            if(newIndentation > indentation + 2) {
                fail("Expected " + (indentation + 2) + " spaces, got " + newIndentation)
            }
            if(newIndentation % 2 != 0) {
                fail("Indentation must be a multiple of 2 spaces")
            }
            var expectedBrackets = bracketStack.takeWhile(_._2 >= newIndentation)
            val onlyColonOrPipeExpectedBrackets = expectedBrackets.forall(b => b._1 == BColon || b._1 == BPipe)
            bracketStack = bracketStack.drop(expectedBrackets.size)
            while(expectedBrackets.nonEmpty) {
                val from = offset
                expectedBrackets.head._1 match {
                    case BCurly =>
                        if(!ahead("}")) fail("Expected '}'")
                        offset += 1
                        tokens += Token(code, from, offset, KCurlyRight)
                    case BSquare =>
                        if(!ahead("]")) fail("Expected ']'")
                        offset += 1
                        tokens += Token(code, from, offset, KSquareRight)
                    case BRound =>
                        if(!ahead(")")) fail("Expected ')'")
                        offset += 1
                        tokens += Token(code, from, offset, KRoundRight)
                    case BPipe =>
                    case BColon =>
                        tokens += Token(code, from, offset, KCurlyRight)
                }
                expectedBrackets = expectedBrackets.tail
            }
            if(onlyColonOrPipeExpectedBrackets && bracketStack.headOption.exists { case (b, i) =>
                i == newIndentation - 2 && (b == BColon || b == BPipe)
            }) {
                tokens += Token(code, offset, offset, KSemicolon)
            }
            indentation = newIndentation
        }

        while(offset < code.length) {
            while(offset < code.length && ahead(" ")) offset += 1
            var c = code(offset)
            if(ahead("\r\n")) {
                if(ahead("\r", "\n")) offset += 1
                offset += 1
                lineOffset = offset
                line += 1
                while(offset < code.length && ahead(" ")) offset += 1
                if(!ahead("#", " \r\n") && !ahead("\r\n")) handleIndentation()
            } else if(c == '\t') {
                fail("Expected spaces, got a tab")
            } else if(ahead("#", " \r\n")) {
                while(offset < code.length && !ahead("\r\n")) offset += 1
            } else if(ahead("#")) {
                val from = offset
                offset += 1
                if(offset < code.length) c = code(offset)
                while(offset < code.length && (
                    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
                )) {
                    offset += 1
                    if(offset < code.length) c = code(offset)
                }
                tokens += Token(code, from, offset, KKeyword)
            } else if((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
                val tokenKind = if(c >= 'A' && c <= 'Z') KUpper else KLower
                val from = offset
                while(offset < code.length && (
                    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
                )) {
                    offset += 1
                    if(offset < code.length) c = code(offset)
                }
                tokens += Token(code, from, offset, tokenKind)
            } else if(c >= '0' && c <= '9') {
                val from = offset
                var decimal = false
                var exponent = false
                while(offset < code.length && (c >= '0' && c <= '9')) {
                    offset += 1
                    if(offset < code.length) c = code(offset)
                    if(!decimal && !exponent && offset + 1 < code.length && c == '.' &&
                        code(offset + 1) >= '0' && code(offset + 1) <= '9'
                    ) {
                        decimal = true
                        offset += 1
                        c = code(offset)
                    } else if(!exponent && (c == 'e' || c == 'E')) {
                        exponent = true
                        offset += 1
                        if(offset < code.length) c = code(offset)
                        if(c == '+' || c == '-') {
                            decimal = true
                            offset += 1
                            if(offset < code.length) c = code(offset)
                        }
                    }
                }
                tokens += Token(code, from, offset, if(decimal || exponent) KFloat else KInt)
            } else if(c == '"') {
                val from = offset
                offset += 1
                if(offset < code.length) c = code(offset)
                while(offset < code.length && c != '"') {
                    if(c == '\\') offset += 1
                    offset += 1
                    if(offset < code.length) c = code(offset)
                }
                if(c != '"') fail("Expected end of string: '\"'")
                offset += 1
                tokens += Token(code, from, offset, KString)
            } else if(ahead("!@$%&/=?^~*-+.:<>|")) {
                val from = offset
                while(offset < code.length && ahead("!@$%&/=?^~*-+.:<>|")) {
                    offset += 1
                    if(offset < code.length) c = code(offset)
                }
                val size = offset - from
                tokens += Token(code, from, offset,
                    if(size == 2 && code(from) == '<' && code(from + 1) == '-') KArrowLeft
                    else if(size == 2 && code(from) == ':' && code(from + 1) == ':') KColonColon
                    else if(size == 2 && code(from) == ':' && code(from + 1) == '=') KColonEqual
                    else if(size == 2 && code(from) == '+' && code(from + 1) == '=') KPlusEqual
                    else if(size == 2 && code(from) == '-' && code(from + 1) == '=') KMinusEqual
                    else if(size == 2 && code(from) == '.' && code(from + 1) == '.') KDotDot
                    else if(size == 1 && code(from) == '.') KDot
                    else if(size == 1 && code(from) == '=') KEqual
                    else if(size == 1 && code(from) == ':') {
                        bracketStack ::= (BColon -> indentation)
                        KCurlyLeft
                    } else if(size == 1 && code(from) == '|') {
                        bracketStack ::= (BPipe -> indentation)
                        KPipe
                    }
                    else KOperator
                )
            } else if(ahead("_")) {
                val from = offset
                offset += 1
                tokens += Token(code, from, offset, KUnderscore)
            } else if(ahead(",")) {
                val from = offset
                offset += 1
                tokens += Token(code, from, offset, KComma)
            } else if(ahead(";")) {
                val from = offset
                offset += 1
                tokens += Token(code, from, offset, KSemicolon)
            } else if(ahead("([{}])")) {
                val from = offset
                offset += 1
                if(c == '{') {
                    tokens += Token(code, from, offset, KCurlyLeft)
                    bracketStack ::= (BCurly -> indentation)
                } else if(c == '[') {
                    tokens += Token(code, from, offset, KSquareLeft)
                    bracketStack ::= (BSquare -> indentation)
                } else if (c == '(') {
                    tokens += Token(code, from, offset, KRoundLeft)
                    bracketStack ::= (BRound -> indentation)
                } else if (c == '}') {
                    while(bracketStack.headOption.exists { case (BColon, i) => i >= indentation; case _ => false }) {
                        tokens += Token(code, from, offset, KCurlyRight)
                        bracketStack = bracketStack.tail
                    }
                    tokens += Token(code, from, offset, KCurlyRight)
                    while(bracketStack.headOption.exists(_._1 == BPipe)) bracketStack = bracketStack.tail
                    bracketStack.headOption match {
                        case Some((BCurly, i)) if i >= indentation =>
                            bracketStack = bracketStack.tail
                        case Some((BCurly, _)) =>
                            fail("Wrong indentation for '}'")
                        case _ =>
                            fail("Unmatched '}'")
                    }
                } else if (c == ']') {
                    while(bracketStack.headOption.exists { case (BColon, i) => i >= indentation; case _ => false }) {
                        tokens += Token(code, from, offset, KCurlyRight)
                        bracketStack = bracketStack.tail
                    }
                    tokens += Token(code, from, offset, KSquareRight)
                    bracketStack.headOption match {
                        case Some((BSquare, i)) if i >= indentation =>
                            bracketStack = bracketStack.tail
                        case Some((BSquare, _)) =>
                            fail("Wrong indentation for ']'")
                        case _ =>
                            fail("Unmatched ']'")
                    }
                } else if (c == ')') {
                    while(bracketStack.headOption.exists { case (BColon, i) => i >= indentation; case _ => false }) {
                        tokens += Token(code, from, offset, KCurlyRight)
                        bracketStack = bracketStack.tail
                    }
                    tokens += Token(code, from, offset, KRoundRight)
                    bracketStack.headOption match {
                        case Some((BRound, i)) if i >= indentation =>
                            bracketStack = bracketStack.tail
                        case Some((BRound, _)) =>
                            fail("Wrong indentation for ')'")
                        case _ =>
                            fail("Unmatched ')'")
                    }
                }
            } else {
                fail("Unexpected '" + c + "'")
            }
        }
        lineOffset = offset
        handleIndentation()

        tokens.toArray[Token]
    }

    sealed abstract class Bracket
    case object BCurly extends Bracket
    case object BSquare extends Bracket
    case object BRound extends Bracket
    case object BPipe extends Bracket
    case object BColon extends Bracket

    case class Token(code : String, from : Int, to : Int, kind : TokenKind)

    sealed abstract class TokenKind
    case object KCurlyLeft extends TokenKind
    case object KCurlyRight extends TokenKind
    case object KSquareLeft extends TokenKind
    case object KSquareRight extends TokenKind
    case object KRoundLeft extends TokenKind
    case object KRoundRight extends TokenKind
    case object KOperator extends TokenKind
    case object KLower extends TokenKind
    case object KUpper extends TokenKind
    case object KUnderscore extends TokenKind
    case object KPipe extends TokenKind
    case object KDot extends TokenKind
    case object KDotDot extends TokenKind
    case object KComma extends TokenKind
    case object KSemicolon extends TokenKind
    case object KKeyword extends TokenKind
    case object KArrowLeft extends TokenKind
    case object KEqual extends TokenKind
    case object KPlusEqual extends TokenKind
    case object KMinusEqual extends TokenKind
    case object KColonEqual extends TokenKind
    case object KColonColon extends TokenKind
    case object KString extends TokenKind
    case object KInt extends TokenKind
    case object KFloat extends TokenKind

}
