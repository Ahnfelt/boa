package com.github.ahnfelt.boa

import com.github.ahnfelt.boa.Syntax._
import com.github.ahnfelt.boa.Tokenizer._

class Parser(tokens : Array[Token]) extends AbstractParser(tokens) {

    def parseFile() : BoaFile = {
        var boaFile = BoaFile(List(), List(), List())
        if(ahead(KSemicolon)) skip(KSemicolon)
        while(offset < tokens.length) {
            if(ahead("#import")) {
                boaFile = boaFile.copy(imports = parseImport() :: boaFile.imports)
            } else if(ahead("#public") || ahead("#protected") || ahead("#private")) {
                if(ahead(KKeyword, KLower) || ahead(KKeyword, KOperator) || ahead(KKeyword, KUpper, KDot)) {
                    boaFile = boaFile.copy(methods = parseMethod(false) :: boaFile.methods)
                } else {
                    boaFile = boaFile.copy(typeDefinitions = parseTypeDefinition(false) :: boaFile.typeDefinitions)
                }
            } else {
                unexpected()
            }
            if(ahead(KSemicolon)) skip(KSemicolon)
        }
        boaFile.copy(
            imports = boaFile.imports.reverse,
            typeDefinitions = boaFile.typeDefinitions.reverse,
            methods = boaFile.methods.reverse
        )
    }

    def parseImport() : Import = {
        val at = skip("#import").at
        val name = if(ahead(KUpper)) Some(skip(KUpper).value) else None
        val url = skip(KString).value
        Import(at, name, url)
    }

    def parseMethod(local : Boolean) : Method = {
        val public = ahead("#public")
        val at = if(local) skip("#local").at else if(public) skip("#public").at else skip("#private").at
        val static = if(!ahead(KUpper)) None else Some {
            val s = skip(KUpper).value
            skip(KDot)
            s
        }
        val name = if(ahead(KOperator)) skip(KOperator).value else skip(KLower).value
        val (typeParameters, None) = if(!ahead(KSquareLeft)) List() -> None else
            many(KSquareLeft, KSquareRight, KDotDot, Some(KComma), false) { skip(KUpper).value }
        val (parameters, rest) =
            many(KRoundLeft, KRoundRight, KDotDot, Some(KComma), false) { parseParameter() }
        val returnType = parseType()

        val body = if(ahead(KCurlyLeft, KPipe) || ahead(KCurlyLeft, KSemicolon, KPipe)) {
            val block = parseBlock()
            val types = parameters.map(_.parameterType) :+ returnType
            val arguments = parameters.map(p => EVariable(p.at, p.name))
            List(STerm(ECall(block.at, None, "call", Some(types), block :: arguments, None)))
        } else {
            skip(KCurlyLeft)
            if(ahead(KSemicolon)) skip(KSemicolon)
            val b = parseBody()
            skip(KCurlyRight)
            b
        }

        Method(at, static, name, public, typeParameters, parameters, rest, returnType, body)
    }

    def parseTypeDefinition(local : Boolean) : TypeDefinition = {
        val (at, publicType, publicConstructors) =
            if(local) { (skip("#local").at, false, false) }
            else if(ahead("#public")) { (skip("#public").at, true, true) }
            else if(ahead("#protected")) { (skip("#protected").at, true, false) }
            else { (skip("#private").at, false, false) }
        val name = skip(KUpper).value
        val (typeParameters, None) = if(!ahead(KSquareLeft)) List() -> None else
            many(KSquareLeft, KSquareRight, KDotDot, Some(KComma), false) { skip(KUpper).value }
        val (constructors, None) =
            if(ahead(KRoundLeft)) {
                val token = tokens(offset)
                val (parameters, rest) =
                    many(KRoundLeft, KRoundRight, KDotDot, Some(KComma), false) { parseParameter() }
                List(TypeConstructor(token.at, "of", parameters, rest)) -> None
            } else many(KCurlyLeft, KCurlyRight, KCurlyRight, Some(KSemicolon), true) { parseTypeConstructor() }
        TypeDefinition(at, publicType, publicConstructors, name, typeParameters, constructors)
    }

    def parseTypeConstructor() : TypeConstructor = {
        val token = skip(KLower)
        val (parameters, rest) =
            many(KRoundLeft, KRoundRight, KDotDot, Some(KComma), false) { parseParameter() }
        TypeConstructor(token.at, token.value, parameters, rest)
    }

    def parseParameter() : Parameter = {
        val token = skip(KLower)
        val t = parseType()
        Parameter(token.at, token.value, t)
    }

    def parseType() : Type = {
        val token = skip(KUpper)
        val (typeArguments, None) = if(!ahead(KSquareLeft)) List() -> None else
            many(KSquareLeft, KSquareRight, KDotDot, Some(KComma), false) { parseType() }
        Type(token.at, token.value, typeArguments)
    }

    def parseBody() : List[Statement] = {
        var result = List[Statement]()
        while(!ahead(KCurlyRight) && !ahead(KPipe)) {
            if(ahead("#mutable") || ahead(KLower, KEqual)) {
                val mutable = ahead("#mutable")
                if(mutable) skip("#mutable")
                val token = skip(KLower)
                skip(KEqual)
                val value = parseTerm()
                result ::= SLet(token.at, token.value, mutable, value)
                while(ahead(KComma)) {
                    skip(KComma)
                    val token = skip(KLower)
                    skip(KEqual)
                    val value = parseTerm()
                    result ::= SLet(token.at, token.value, mutable, value)
                }
            } else if(ahead(KLower, KArrowLeft)) {
                val token = skip(KLower)
                val at = skip(KArrowLeft).at
                val value = parseTerm()
                skip(KSemicolon)
                val body = parseBody()
                val block = EBlock(at, List(Case(List(PVariable(token.at, Some(token.value))), body)))
                result ::= STerm(ECall(at, None, "flatMap", None, List(value, block), None))
            } else if(ahead(KLower, KColonEqual) || ahead(KLower, KPlusEqual) || ahead(KLower, KMinusEqual)) {
                val token = skip(KLower)
                val increment =
                    if(ahead(KPlusEqual)) { skip(KPlusEqual); Some(true) }
                    else if(ahead(KMinusEqual)) { skip(KMinusEqual); Some(false) }
                    else { skip(KColonEqual); None }
                val value = parseTerm()
                val modified = increment.map { i =>
                    ECall(token.at, None, if(i) "+" else "-", None, List(EVariable(token.at, token.value)), None )
                }.getOrElse(value)
                result ::= SAssign(token.at, token.value, modified)
            } else if(ahead("#import")) {
                result ::= SImport(parseImport())
            } else if(ahead("#local") && ahead(KKeyword, KLower)) {
                result ::= SMethod(parseMethod(true))
            } else if(ahead("#local") && ahead(KKeyword, KUpper)) {
                result ::= STypeDefinition(parseTypeDefinition(true))
            } else {
                result ::= STerm(parseTerm())
            }
            if(!ahead(KCurlyRight) && !ahead(KPipe)) skip(KSemicolon)
        }
        result.reverse
    }

    def parseTerm() : Term = parseInfix()

    def parseInfix() : Term = {
        var result = parseBinary()
        while(ahead(KLower, ":") || ahead(KSemicolon, KLower, ":")) {
            if(ahead(KSemicolon)) skip(KSemicolon)
            val token = skip(KLower)
            val block = parseBlock()
            result = ECall(token.at, None, token.value, None, List(result, block), None)
        }
        result
    }

    def parseBinary() : Term = {
        var result = parseUnary()
        while(ahead(KOperator)) {
            val token = skip(KOperator)
            val right = parseUnary()
            result = ECall(token.at, None, token.value, None, List(result, right), None)
        }
        result
    }

    def parseUnary() : Term = {
        if(ahead(KOperator)) {
            val token = skip(KOperator)
            val term = parseUnary()
            ECall(token.at, None, token.value, None, List(term), None)
        } else {
            parseMethod()
        }
    }

    def parseMethod() : Term = {
        var result = parseAtom()
        while(ahead(KDot)) {
            if(ahead(KSemicolon)) skip(KSemicolon)
            if(ahead(KDot)) skip(KDot)
            val token = skip(KLower)
            val (typeArguments, arguments, rest) = parseArguments()
            result = ECall(token.at, None, token.value, typeArguments, result :: arguments, rest)
        }
        result
    }

    def parseAtom() : Term = {
        val upperCall = ahead(KUpper, KCurlyLeft) || ahead(KUpper, KSquareLeft) || ahead(KUpper, KRoundLeft)
        val lowerCall = ahead(KLower, "{") || ahead(KLower, KSquareLeft) || ahead(KLower, KRoundLeft)
        if(upperCall) {
            val token = skip(KUpper)
            val (typeArguments, arguments, rest) = parseArguments()
            ECall(token.at, Some(token.value), "of", typeArguments, arguments, rest)
        } else if(lowerCall) {
            val token = skip(KLower)
            val (typeArguments, arguments, rest) = parseArguments()
            ECall(token.at, None, token.value, typeArguments, arguments, rest)
        } else if(ahead(KUpper, KDot)) {
            val static = Some(skip(KUpper).value)
            val at = skip(KDot).at
            val name = skip(KLower).value
            val (typeArguments, arguments, rest) = parseArguments()
            ECall(at, static, name, typeArguments, arguments, rest)
        } else if(ahead(KLower)) {
            val token = skip(KLower)
            EVariable(token.at, token.value)
        } else if(ahead(KString)) {
            val token = skip(KString)
            EString(token.at, token.value)
        } else if(ahead(KInt)) {
            val token = skip(KInt)
            EString(token.at, token.value)
        } else if(ahead(KFloat)) {
            val token = skip(KFloat)
            EString(token.at, token.value)
        } else if(ahead(KRoundLeft)) {
            skip(KRoundLeft)
            val term = parseTerm()
            skip(KRoundRight)
            term
        } else {
            unexpected()
        }
    }

    def parseArguments() : (Option[List[Type]], List[Term], Option[Term]) = {
        val typeArguments = if(!ahead(KSquareLeft)) None else
            Some(many(KSquareLeft, KSquareRight, KSquareRight, Some(KComma), false) { parseType() }._1)
        val (arguments, rest) = if(!ahead(KRoundLeft)) (List(), None) else
            many(KRoundLeft, KRoundRight, KDotDot, Some(KComma), false) { parseTerm() }
        var blocks = List[Term]()
        while(rest.isEmpty && ahead(KCurlyLeft)) {
            blocks ::= parseBlock()
        }
        (typeArguments, arguments ++ blocks.reverse, rest)
    }

    def parseBlock() : Term = {
        val at = skip(KCurlyLeft).at
        if(ahead(KSemicolon)) skip(KSemicolon)
        val (firstPatterns, None) = if(!ahead(KPipe)) List() -> None else
            many(KPipe, KPipe, KPipe, Some(KComma), false) { parsePatten() }
        if(firstPatterns.nonEmpty && ahead(KSemicolon)) skip(KSemicolon)
        val firstBody = parseBody()
        var cases = List(Case(firstPatterns, firstBody))
        if(ahead(KSemicolon)) skip(KSemicolon)
        while(ahead(KPipe)) {
            val (patterns, None) = many(KPipe, KPipe, KPipe, Some(KComma), false) { parsePatten() }
            val body = parseBody()
            cases ::= Case(patterns, body)
        }
        skip(KCurlyRight)
        EBlock(at, cases.reverse)
    }

    def parsePatten() : Pattern = {
        if(ahead(KUpper)) {
            val token = skip(KUpper)
            val (patterns, rest) = many(KRoundLeft, KRoundRight, KDotDot, Some(KComma), false) { parsePatten() }
            PConstructor(token.at, "of", patterns, rest)
        } else if(ahead(KLower, KRoundLeft)) {
            val token = skip(KLower)
            val (patterns, rest) = many(KRoundLeft, KRoundRight, KDotDot, Some(KComma), false) { parsePatten() }
            PConstructor(token.at, token.value, patterns, rest)
        } else if(ahead(KLower)) {
            val token = skip(KLower)
            PVariable(token.at, Some(token.value))
        } else if(ahead(KUnderscore)) {
            val token = skip(KUnderscore)
            PVariable(token.at, None)
        } else if(ahead(KString)) {
            val token = skip(KString)
            PString(token.at, token.value)
        } else if(ahead(KInt)) {
            val token = skip(KInt)
            PInt(token.at, token.value)
        } else if(ahead(KFloat)) {
            val token = skip(KFloat)
            PFloat(token.at, token.value)
        } else {
            unexpected()
        }
    }

}


abstract class AbstractParser(tokens : Array[Token]) {

    protected implicit class RichToken(token : Token) {
        def at : At = At(token.code, token.from, token.to)
        def value : String = token.code.substring(token.from, token.to)
    }

    protected var offset = 0

    protected def fail(problem : String) : Nothing =
        throw new RuntimeException(problem + " at offset " + offset)
    protected def unexpected() : Nothing = {
        val token = tokens(offset)
        fail("Unexpected " + token.kind.toString.drop(1) + ": '" + token.code.substring(token.from, token.to) + "'")
    }

    protected def ahead(kind : TokenKind) = offset < tokens.length &&
        tokens(offset).kind == kind
    protected def ahead(value : String) = offset < tokens.length &&
        tokens(offset).value == value
    protected def ahead(kind1 : TokenKind, kind2 : TokenKind) = offset + 1 < tokens.length &&
        tokens(offset).kind == kind1 && tokens(offset + 1).kind == kind2
    protected def ahead(kind1 : TokenKind, value : String) = offset + 1 < tokens.length &&
        tokens(offset).kind == kind1 && tokens(offset + 1).value == value
    protected def ahead(kind1 : TokenKind, kind2 : TokenKind, kind3 : TokenKind) = offset + 2 < tokens.length &&
        tokens(offset).kind == kind1 && tokens(offset + 1).kind == kind2 && tokens(offset + 2).kind == kind3
    protected def ahead(kind1 : TokenKind, kind2 : TokenKind, value : String) = offset + 2 < tokens.length &&
        tokens(offset).kind == kind1 && tokens(offset + 1).kind == kind2 && tokens(offset + 2).value == value

    protected def skip(tokenKind : TokenKind) =
        if(offset >= tokens.length) fail("File ended, expecting " + tokenKind) else {
            val token = tokens(offset)
            if(token.kind != tokenKind) {
                fail(
                    "Expecting " + tokenKind + ", got " + token.kind.toString.drop(1) + ": '" +
                        token.code.substring(token.from, token.to) + "'"
                )
            } else {
                offset += 1
                token
            }
        }

    protected def skip(value : String) =
        if(offset >= tokens.length) fail("File ended, expecting: " + value) else {
            val token = tokens(offset)
            if(!token.code.regionMatches(token.from, value, 0, value.length)) {
                fail(
                    "Expecting '" + value + "', got " + token.kind.toString.drop(1) + ": '" +
                        token.code.substring(token.from, token.to) + "'"
                )
            } else {
                offset += 1
                token
            }
        }

    protected def many[T](
        begin : TokenKind,
        end : TokenKind,
        restMarker : TokenKind,
        separator : Option[TokenKind],
        skipLeadingSeparator : Boolean
    )(element : => T) : (List[T], Option[T]) = {
        var result = List[T]()
        skip(begin)
        if(skipLeadingSeparator) for(s <- separator if ahead(s)) skip(s)
        while(!ahead(end) && !ahead(restMarker)) {
            result ::= element
            if(!ahead(end) && !ahead(restMarker)) for(s <- separator) skip(s)
        }
        val rest = !ahead(end)
        if(rest) skip(restMarker)
        skip(end)
        if(rest) result.drop(1).reverse -> result.headOption
        else result.reverse -> None
    }

}
