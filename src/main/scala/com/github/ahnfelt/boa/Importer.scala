package com.github.ahnfelt.boa

import com.github.ahnfelt.boa.Syntax._

import scala.collection.mutable
import scala.io.Source

class Importer() {

    val cache = mutable.HashMap[String, BoaFile]()

    def importAndProcess(url : String) : BoaFile = {
        cache.getOrElseUpdate(url, {
            val boaFile = tokenizeAndParse(url, printTokensAndSyntax = false)
            val imported = boaFile.imports.map(i => i.url -> importAndProcess(i.url)).toMap
            val resolved = new Resolver(url, imported).resolveFile(boaFile)
            val checked = new Checker(imported).checkFile(resolved)
            checked
        })
    }

    def tokenizeAndParse(sourceFile : String, printTokensAndSyntax : Boolean) : BoaFile = {
        val code = Source.fromFile(sourceFile, "UTF-8").mkString
        val tokens = Tokenizer.tokenize(code)
        if(printTokensAndSyntax) {
            println(tokens.map(t => t.kind + "\t" + code.substring(t.from, t.to)).mkString("\n"))
            println()
        }
        val boaFile = new Parser(tokens).parseFile()
        if(printTokensAndSyntax) {
            println(boaFile)
            println()
        }
        boaFile
    }

}
