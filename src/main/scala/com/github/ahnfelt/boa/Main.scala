package com.github.ahnfelt.boa

import scala.io.Source

object Main {
    def main(arguments : Array[String]) : Unit = {
        val code = Source.fromFile(arguments(0), "UTF-8").mkString
        val tokens = Tokenizer.tokenize(code)
        println(tokens.map(t => t.kind + "\t" + code.substring(t.from, t.to)).mkString("\n"))
        println()
        val syntax = new Parser(tokens).parseFile()
        print(syntax)
    }
}
