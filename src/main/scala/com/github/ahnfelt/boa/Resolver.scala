package com.github.ahnfelt.boa

import com.github.ahnfelt.boa.Syntax._

import scala.collection.mutable
import scala.io.Source

class Resolver(fullUrl : String, imports : Map[String, BoaFile]) {

    val typeUrl = fullUrl.replaceAll("[{][^}]+[}]", "{}")

    val importedMethods = imports.values.toList.flatMap(_.methods)
    val importedTypes = imports.values.toList.flatMap(_.typeDefinitions)

    trait TypeSymbols { 
        val typeParameters : List[String]
        val types : List[TypeDefinition]
        def withTypeParameters(typeParameters : List[String]) : TypeSymbols
    }
    
    case class Symbols(
        typeParameters : List[String],
        types : List[TypeDefinition],
        methods : List[MethodHeader]
    ) extends TypeSymbols {
        def withTypeParameters(typeParameters : List[String]) = copy(typeParameters = typeParameters)
    }

    def resolveFile(boaFile : BoaFile) : BoaFile = {
        val typeDefinitions = boaFile.typeDefinitions.map(_.copy(url = typeUrl))
        val methods = boaFile.methods.map(m => m.copy(header = m.header.copy(url = fullUrl)))
        val symbols = Symbols(
            List(), 
            importedTypes ++ typeDefinitions, 
            importedMethods.map(_.header) ++ methods.map(_.header)
        )
        val newSymbols = symbols.copy(types = symbols.types.map(resolveTypeDefinition(_, symbols)))
        val newerSymbols = newSymbols.copy(methods = newSymbols.methods.map(resolveMethodHeader(_, newSymbols)))
        boaFile.copy(
            typeDefinitions = newerSymbols.types,
            methods = methods.map(resolveMethodDefinition(_, newerSymbols))
        )
    }

    def resolveTypeDefinition(typeDefinition : TypeDefinition, symbols : TypeSymbols) = {
        val newSymbols = symbols.withTypeParameters(symbols.typeParameters ++ typeDefinition.typeParameters)
        typeDefinition.copy(
            url = typeUrl,
            constructors = typeDefinition.constructors.map(resolveTypeConstructor(_, newSymbols))
        )
    }

    def resolveTypeConstructor(constructor : TypeConstructor, symbols : TypeSymbols) = {
        constructor.copy(
            parameters = constructor.parameters.map(p =>
                p.copy(parameterType = resolveType(p.parameterType, symbols))
            ),
            rest = constructor.rest.map(p =>
                p.copy(parameterType = resolveType(p.parameterType, symbols))
            )
        )
    }

    def resolveMethodDefinition(method : Method, symbols : Symbols) = {
        val newSymbols = symbols.withTypeParameters(symbols.typeParameters ++ method.header.typeParameters)
        method.copy(
            header = resolveMethodHeader(method.header, newSymbols),
            body = resolveBody(method.body, newSymbols)
        )
    }

    def resolveMethodHeader(header : MethodHeader, symbols : TypeSymbols) = {
        val newSymbols = symbols.withTypeParameters(symbols.typeParameters ++ header.typeParameters)
        header.copy(
            url = fullUrl,
            parameters = header.parameters.map(p => p.copy(parameterType = resolveType(p.parameterType, newSymbols))),
            rest = header.rest.map(p => p.copy(parameterType = resolveType(p.parameterType, newSymbols))),
            returnType = resolveType(header.returnType, newSymbols)
        )
    }

    def resolveBody(statements : List[Statement], symbols : Symbols) : List[Statement] = {
        val symbols1 = symbols.copy(
            methods = symbols.methods ++ statements.collect {
                case m : SMethod => m.method.header.copy(url = ":local")
            },
            types = symbols.types ++ statements.collect {
                case t : STypeDefinition => t.definition.copy(url = ":local")
            }
        )
        val symbols2 = symbols1.copy(types = symbols1.types.map(resolveTypeDefinition(_, symbols1)))
        val symbols3 = symbols2.copy(methods = symbols2.methods.map(resolveMethodHeader(_, symbols2)))
        statements.map(resolveStatement(_, symbols3))
    }

    def resolveStatement(statement : Statement, symbols : Symbols) : Statement = {
        statement match {
            case _ : SMagic => statement
            case _ : SImport => statement
            case SMethod(method) => SMethod(resolveMethodDefinition(method, symbols))
            case STypeDefinition(definition) => STypeDefinition(resolveTypeDefinition(definition, symbols))
            case SLet(at, name, mutable, value) => SLet(at, name, mutable, resolveTerm(value, symbols))
            case SAssign(at, name, value) => SAssign(at, name, resolveTerm(value, symbols))
            case STerm(value) => STerm(resolveTerm(value, symbols))
        }
    }

    def resolveTerm(term : Term, symbols : Symbols) : Term = {
        term match {
            case ETypeAnnotation(at, e, annotation) =>
                ETypeAnnotation(at, resolveTerm(e, symbols), resolveType(annotation, symbols))
            case EString(at, v) => term
            case EInt(at, v) => term
            case EFloat(at, v) => term
            case EWildcard(at) => term
            case EVariable(at, name) => term
            case EBlock(at, cases) =>
                EBlock(at, cases.map(c => c.copy(
                    patterns = c.patterns.map(resolvePattern(_, symbols)),
                    condition = c.condition.map(resolveTerm(_, symbols)),
                    body = resolveBody(c.body, symbols)
                )))
            case call@ECall(at, overloads, url, static, name, typeArguments, arguments, rest) =>
                val allOverloads = symbols.methods.filter(_.name == name).filter(_.static == static)
                if(allOverloads.isEmpty) {
                    throw new RuntimeException("Unknown method: " + static.map(_ + ".").getOrElse("") + name)
                }
                val overloads = allOverloads.filter(o =>
                    o.parameters.size == arguments.size ||
                    (o.rest.nonEmpty && o.parameters.size <= arguments.size)
                ).filter(o => rest.isEmpty || o.rest.nonEmpty)
                if(overloads.isEmpty) {
                    throw new RuntimeException("Wrong number of arguments: " + static.map(_ + ".").getOrElse("") + name)
                }
                call.copy(
                    overloads = overloads,
                    typeArguments = typeArguments.map(_.map(resolveType(_, symbols))),
                    arguments = arguments.map(resolveTerm(_, symbols)),
                    rest = rest.map(resolveTerm(_, symbols))
                )
        }
    }

    def resolveType(t : Type, symbols : TypeSymbols) : Type = {
        if(symbols.typeParameters.contains(t.name)) t.copy(url = ":parameter") else {
            val urls = symbols.types.filter(_.name == t.name).map(_.url)
            urls match {
                case List(newUrl) =>
                    t.copy(
                        url = newUrl,
                        typeArguments = t.typeArguments.map(resolveType(_, symbols))
                    )
                case List() => throw new RuntimeException("Unknown type: " + t.name)
                case _ => throw new RuntimeException("Ambiguous type: " + t.name + " from " + urls.mkString(" or "))
            }
        }
    }

    def resolvePattern(pattern : Pattern, symbols : Symbols) = pattern

}
