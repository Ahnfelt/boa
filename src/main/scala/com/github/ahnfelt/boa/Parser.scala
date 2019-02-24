package com.github.ahnfelt.boa

import com.github.ahnfelt.boa.Syntax._
import com.github.ahnfelt.boa.Tokenizer._

class Parser(tokens : Array[Token], flags : Set[String]) extends AbstractParser(tokens) {

    def parseFile() : BoaFile = {
        var boaFile = BoaFile(List(), List(), List())
        def parseTop(include : Boolean) : Unit = {
            if(ahead(KSemicolon)) skip(KSemicolon)
            while(offset < tokens.length) {
                if(ahead("#import")) {
                    val parsedImport = parseImport()
                    if(include) boaFile = boaFile.copy(imports = parsedImport :: boaFile.imports)
                } else if(ahead("#public") || ahead("#protected") || ahead("#private")) {
                    if(ahead(KKeyword, KLower) || ahead(KKeyword, KOperator) || ahead(KKeyword, KUpper, KDot)) {
                        val method = parseMethod(false)
                        if(include) boaFile = boaFile.copy(methods = method :: boaFile.methods)
                    } else {
                        val typeDefinition = parseTypeDefinition(false)
                        val methods = methodsForTypeDefinition(typeDefinition)
                        if(include) boaFile = boaFile.copy(
                            methods = methods.reverse ++ boaFile.methods,
                            typeDefinitions = typeDefinition :: boaFile.typeDefinitions
                        )
                    }
                } else if(ahead("#if")) {
                    parseStaticIf(parseTop)
                } else {
                    return
                }
                if(ahead(KSemicolon)) skip(KSemicolon)
            }
        }
        parseTop(true)
        if(offset < tokens.length) unexpected()
        boaFile.copy(
            imports = boaFile.imports.reverse,
            typeDefinitions = boaFile.typeDefinitions.reverse,
            methods = boaFile.methods.reverse
        )
    }

    private def parseStaticIf(parse : Boolean => Unit) : Unit = {
        skip("#if")
        var wasTrue = staticCondition(parseTerm())
        if(ahead(KSemicolon)) skip(KSemicolon)
        parse(wasTrue)
        while(ahead("#elseif")) {
            skip("#elseif")
            val isTrue = staticCondition(parseTerm()) && !wasTrue
            if(ahead(KSemicolon)) skip(KSemicolon)
            parse(isTrue)
            if(isTrue) wasTrue = true
        }
        if(ahead("#else")) {
            skip("#else")
            if(ahead(KSemicolon)) skip(KSemicolon)
            parse(!wasTrue)
        }
        skip("#end")
    }

    private def staticCondition(term : Term) : Boolean = term match {
        case EVariable(_, name) =>
            flags(name)
        case ECall(_, _, _, None, "not", None, List(e), None) =>
            !staticCondition(e)
        case ECall(_, _, _, None, "and", None, List(e1, EBlock(_, List(Case(List(), None, List(STerm(e2)))))), None) =>
            staticCondition(e1) && staticCondition(e2)
        case ECall(_, _, _, None, "or", None, List(e1, EBlock(_, List(Case(List(), None, List(STerm(e2)))))), None) =>
            staticCondition(e1) || staticCondition(e2)
        case _ =>
            fail("Expected a static condition: e1 and: e2, e1 or: e2, not(e), or a variable")
    }

    private def methodsForTypeDefinition(typeDefinition : TypeDefinition) : List[Method] = {
        val typeArguments = typeDefinition.typeParameters.map(Type(typeDefinition.at, "", _, List()))
        val theType = Type(typeDefinition.at, "", typeDefinition.name, typeArguments)
        val constructors = typeDefinition.constructors.map(c => Method(
            header = MethodHeader(
                at = c.at,
                static = Some(typeDefinition.name),
                url = "",
                public = typeDefinition.publicConstructors,
                name = c.name,
                typeParameters = typeDefinition.typeParameters,
                parameters = c.parameters,
                rest = c.rest,
                returnType = theType
            ),
            body = List(SProvided(c.at))
        ))
        val fields = typeDefinition.constructors match {
            case List(c) if typeDefinition.isRecord =>
                val rest = c.rest.map(r => r.copy(parameterType = Type(r.at, ":core", "Array", List(r.parameterType))))
                (c.parameters ++ rest.toList).map(p => Method(
                    header = MethodHeader(
                        at = p.at,
                        static = None,
                        url = "",
                        public = typeDefinition.publicConstructors,
                        name = p.name,
                        typeParameters = typeDefinition.typeParameters,
                        parameters = List(Parameter(p.at, "this", theType)),
                        rest = None,
                        returnType = p.parameterType
                    ),
                    body = List(SProvided(p.at))
                ))
            case _ => List()
        }
        constructors ++ fields
    }

    def parseImport() : Import = {
        val at = skip("#import").at
        val name = if(ahead(KUpper)) Some(skip(KUpper).value) else None
        val url = decodeString(skip(KString).value)
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
            List(STerm(ECall(block.at, List(), "", None, "call", Some(types), block :: arguments, None)))
        } else {
            skip(KCurlyLeft)
            if(ahead(KSemicolon)) skip(KSemicolon)
            val b = parseBody()
            skip(KCurlyRight)
            b
        }

        Method(MethodHeader(at, static, "", name, public, typeParameters, parameters, rest, returnType), body)
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
        val isRecord = ahead(KRoundLeft)
        val (constructors, None) =
            if(isRecord) {
                val token = tokens(offset)
                val (parameters, rest) =
                    many(KRoundLeft, KRoundRight, KDotDot, Some(KComma), false) { parseParameter() }
                List(TypeConstructor(token.at, "of", parameters, rest)) -> None
            } else many(KCurlyLeft, KCurlyRight, KCurlyRight, Some(KSemicolon), true) { parseTypeConstructor() }
        TypeDefinition(at, publicType, publicConstructors, isRecord, "", name, typeParameters, constructors)
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
        Type(token.at, "", token.value, typeArguments)
    }

    def parseBody() : List[Statement] = {
        var result = List[Statement]()
        def parseStatements(include : Boolean) : Unit = {
            while(!ahead(KCurlyRight) && !ahead(KPipe)) {
                if(ahead("#mutable") || ahead(KLower, KEqual)) {
                    val mutable = ahead("#mutable")
                    if(mutable) skip("#mutable")
                    val token = skip(KLower)
                    skip(KEqual)
                    val value = parseTerm()
                    if(include) result ::= SLet(token.at, token.value, mutable, value)
                    while(ahead(KComma)) {
                        skip(KComma)
                        val token = skip(KLower)
                        skip(KEqual)
                        val value = parseTerm()
                        if(include) result ::= SLet(token.at, token.value, mutable, value)
                    }
                } else if(ahead(KLower, KArrowLeft)) {
                    val token = skip(KLower)
                    val at = skip(KArrowLeft).at
                    val value = parseTerm()
                    skip(KSemicolon)
                    val body = parseBody()
                    val block = EBlock(at, List(Case(List(PVariable(token.at, Some(token.value))), None, body)))
                    if(include) result ::= STerm(ECall(at, List(), "", None, "flatMap", None, List(value, block), None))
                } else if(
                    ahead(KLower, KColonEqual) ||
                        ahead(KLower, KDotEqual) ||
                        ahead(KLower, KPlusEqual) ||
                        ahead(KLower, KMinusEqual)
                ) {
                    val name = skip(KLower).value
                    val (token, increment) =
                        if(ahead(KDotEqual)) skip(KDotEqual) -> None
                        else if(ahead(KPlusEqual)) skip(KPlusEqual) -> Some(true)
                        else if(ahead(KMinusEqual)) skip(KMinusEqual) -> Some(false)
                        else skip(KColonEqual) -> None
                    val value = if(token.kind != KDotEqual) parseTerm() else {
                        val m = skip(KLower)
                        val (typeArguments, arguments, rest) = parseArguments()
                        ECall(
                            m.at, List(), "", None, m.value, typeArguments, EVariable(token.at, name) :: arguments, rest
                        )
                    }
                    val modified = increment.map { i => ECall(
                        token.at, List(), "", None, if(i) "+" else "-", None, List(EVariable(token.at, name)), None
                    )}.getOrElse(value)
                    if(include) result ::= SAssign(token.at, name, modified)
                } else if(ahead("#import")) {
                    val importStatement = parseImport()
                    if(include) result ::= SImport(importStatement)
                } else if(ahead("#local") && ahead(KKeyword, KLower)) {
                    val method = parseMethod(true)
                    if(include) result ::= SMethod(method)
                } else if(ahead("#local") && ahead(KKeyword, KUpper)) {
                    val typeDefinition = parseTypeDefinition(true)
                    val methods = methodsForTypeDefinition(typeDefinition)
                    if(include) {
                        result ::= STypeDefinition(typeDefinition)
                        for(m <- methods) result ::= SMethod(m)
                    }
                } else if(ahead("#if")) {
                    parseStaticIf(parseStatements)
                } else if(ahead("#elseif") || ahead("#else") || ahead("#end")) {
                    return
                } else {
                    val term = STerm(parseTerm())
                    if(include) result ::= term
                }
                if(!ahead(KCurlyRight) && !ahead(KPipe)) skip(KSemicolon)
            }
        }
        parseStatements(true)
        result.reverse
    }

    def parseTerm() : Term = parseInfix()

    def parseInfix() : Term = {
        var result = parsePair()
        while(ahead(KLower, ":") || ahead(KSemicolon, KLower, ":")) {
            if(ahead(KSemicolon)) skip(KSemicolon)
            val token = skip(KLower)
            val block = parseBlock()
            result = ECall(token.at, List(), "", None, token.value, None, List(result, block), None)
        }
        result
    }

    def parsePair() : Term = {
        val left = parseBinary()
        if(!ahead(KArrowRight)) left else {
            val token = skip(KArrowRight)
            val right = parsePair()
            ECall(token.at, List(), ":core", Some("Pair"), "of", None, List(left, right), None)
        }
    }

    def parseBinary() : Term = {
        var result = parseUnary()
        while(ahead(KOperator)) {
            val token = skip(KOperator)
            val right = parseUnary()
            result = ECall(token.at, List(), "", None, token.value, None, List(result, right), None)
        }
        result
    }

    def parseUnary() : Term = {
        if(ahead(KOperator)) {
            val token = skip(KOperator)
            val term = parseUnary()
            ECall(token.at, List(), "", None, token.value, None, List(term), None)
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
            result = ECall(token.at, List(), "", None, token.value, typeArguments, result :: arguments, rest)
        }
        result
    }

    def parseAtom() : Term = {
        val upperCall = ahead(KUpper, KCurlyLeft) || ahead(KUpper, KSquareLeft) || ahead(KUpper, KRoundLeft)
        val lowerCall = ahead(KLower, "{") || ahead(KLower, KSquareLeft) || ahead(KLower, KRoundLeft)
        if(upperCall) {
            val token = skip(KUpper)
            val (typeArguments, arguments, rest) = parseArguments()
            ECall(token.at, List(), "", Some(token.value), "of", typeArguments, arguments, rest)
        } else if(lowerCall) {
            val token = skip(KLower)
            val (typeArguments, arguments, rest) = parseArguments()
            ECall(token.at, List(), "", None, token.value, typeArguments, arguments, rest)
        } else if(ahead(KUpper, KDot)) {
            val static = Some(skip(KUpper).value)
            val at = skip(KDot).at
            val name = skip(KLower).value
            val (typeArguments, arguments, rest) = parseArguments()
            ECall(at, List(), "", static, name, typeArguments, arguments, rest)
        } else if(ahead(KLower)) {
            val token = skip(KLower)
            EVariable(token.at, token.value)
        } else if(ahead(KString)) {
            val token = skip(KString)
            EString(token.at, token.value)
        } else if(ahead(KInt)) {
            val token = skip(KInt)
            EInt(token.at, token.value)
        } else if(ahead(KFloat)) {
            val token = skip(KFloat)
            EFloat(token.at, token.value)
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
        var cases = if(!ahead(KPipe)) List() else List(parseCase())
        if(cases.isEmpty) {
            cases ::= Case(List(), None, parseBody())
        } else {
            while(ahead(KPipe)) cases ::= parseCase()
        }
        skip(KCurlyRight)
        EBlock(at, cases.reverse)
    }

    def parseCase() : Case = {
        skip(KPipe)
        var patterns = List[Pattern]()
        while(!ahead(KPipe) && !ahead(KCurlyLeft)) {
            patterns ::= parsePattern()
            if(ahead(KComma)) skip(KComma)
        }
        val condition = if(ahead(KCurlyLeft)) {
            skip(KCurlyLeft)
            val term = parseTerm()
            skip(KCurlyRight)
            Some(term)
        } else None
        skip(KPipe)
        if(ahead(KSemicolon)) skip(KSemicolon)
        val body = parseBody()
        Case(patterns.reverse, condition, body)
    }

    def parsePattern() : Pattern = {
        if(ahead(KLower, KRoundLeft, KCurlyLeft)) {
            val token = skip(KLower)
            skip(KRoundLeft)
            skip(KCurlyLeft)
            val variable = if(ahead(KLower)) Some(skip(KLower).value) else { skip(KUnderscore); None }
            skip(KCurlyRight)
            skip(KRoundRight)
            PConstructorFields(token.at, token.value, variable)
        } else if(ahead(KLower, KRoundLeft)) {
            val token = skip(KLower)
            val (patterns, rest) = many(KRoundLeft, KRoundRight, KDotDot, Some(KComma), false) { parsePattern() }
            PConstructor(token.at, token.value, patterns, rest)
        } else if(ahead(KRoundLeft)) {
            val at = tokens(offset).at
            val (patterns, rest) = many(KRoundLeft, KRoundRight, KDotDot, Some(KComma), false) { parsePattern() }
            PConstructor(at, "of", patterns, rest)
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

    protected def decodeString(text : String) = {
        text.drop(1).dropRight(1)
    }

}
