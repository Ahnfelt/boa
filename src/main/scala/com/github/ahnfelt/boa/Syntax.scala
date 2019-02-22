package com.github.ahnfelt.boa

// Missing: &&, ||, named arguments

object Syntax {

    case class At(code : String, from : Int, to : Int) {
        override def toString = "" + from + "\n"
    }

    trait Located { def at : At }

    case class BoaFile(imports : List[Import], typeDefinitions : List[TypeDefinition], methods : List[Method])

    case class Import(at : At, name : Option[String], url : String)

    case class TypeDefinition(
        at : At,
        publicType : Boolean,
        publicConstructors : Boolean,
        isRecord : Boolean,
        url : String,
        name : String,
        typeParameters : List[String],
        constructors : List[TypeConstructor]
    )

    case class TypeConstructor(
        at : At,
        name : String,
        parameters : List[Parameter],
        rest : Option[Parameter]
    )

    case class Method(
        header : MethodHeader,
        body : List[Statement]
    )

    case class MethodHeader(
        at : At,
        static : Option[String],
        url : String,
        name : String,
        public : Boolean,
        typeParameters : List[String],
        parameters : List[Parameter],
        rest : Option[Parameter],
        returnType : Type,
    )

    case class Constraint(at : At, typeClass : String, typeArguments : List[Type])

    case class Parameter(at : At, name : String, parameterType : Type)

    case class Type(at : At, url : String, name : String, typeArguments : List[Type])

    sealed abstract class Statement extends Located
    case class SProvided(at : At) extends Statement
    case class SImport(importStatement : Import) extends Statement { def at = importStatement.at }
    case class SMethod(method : Method) extends Statement { def at = method.header.at }
    case class STypeDefinition(definition : TypeDefinition) extends Statement { def at = definition.at }
    case class SLet(at : At, name : String, mutable : Boolean, value : Term) extends Statement
    case class SAssign(at : At, name : String, value : Term) extends Statement
    case class STerm(value : Term) extends Statement { def at = value.at }

    sealed abstract class Term extends Located
    case class ETypeAnnotation(at : At, term : Term, annotation : Type) extends Term
    case class EString(at : At, value : String) extends Term
    case class EInt(at : At, value : String) extends Term
    case class EFloat(at : At, value : String) extends Term
    case class EWildcard(at : At) extends Term
    case class EVariable(at : At, name : String) extends Term
    case class EBlock(at : At, cases : List[Case]) extends Term
    case class ECall(
        at : At,
        overloads : List[MethodHeader],
        url : String,
        static : Option[String],
        name : String,
        typeArguments : Option[List[Type]],
        arguments : List[Term],
        rest : Option[Term]
    ) extends Term

    case class Case(patterns : List[Pattern], condition : Option[Term], body : List[Statement])

    sealed abstract class Pattern extends Located
    case class PVariable(at : At, name : Option[String]) extends Pattern
    case class PAlias(at : At, name : String, pattern : Pattern) extends Pattern
    case class PInt(at : At, value : String) extends Pattern
    case class PFloat(at : At, value : String) extends Pattern
    case class PString(at : At, value : String) extends Pattern
    case class PConstructor(at : At, name : String, patterns : List[Pattern], rest : Option[Pattern]) extends Pattern
    case class PConstructorFields(at : At, name : String, variable : Option[String]) extends Pattern

}
