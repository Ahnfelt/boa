package com.github.ahnfelt.boa

import com.github.ahnfelt.boa.Syntax._

import scala.collection.mutable
import scala.io.Source

class Importer() {

    val cache = mutable.HashMap[String, BoaFile]()

    def importAndProcess(url : String, flags : Set[String]) : BoaFile = {
        cache.getOrElseUpdate(url, {
            val boaFile = tokenizeAndParse(url, printTokensAndSyntax = true, flags)
            val imported = boaFile.imports.map(i => i.url -> importAndProcess(i.url, flags)).toMap
            val resolved = new Resolver(url, imported).resolveFile(boaFile)
            val checked = new Checker(imported).checkFile(resolved)
            checked
        })
    }

    def tokenizeAndParse(sourceFile : String, printTokensAndSyntax : Boolean, flags : Set[String]) : BoaFile = {
        val code = Source.fromFile(sourceFile, "UTF-8").mkString
        val tokens = Tokenizer.tokenize(code)
        if(printTokensAndSyntax) {
            println(tokens.zipWithIndex.map { case (t, i) =>
                i + "\t" + t.kind + "\t" + code.substring(t.from, t.to)
            }.mkString("\n"))
            println()
        }
        val boaFile = new Parser(tokens, flags).parseFile()
        if(printTokensAndSyntax) {
            println(boaFile)
            println()
        }
        boaFile
    }

}
