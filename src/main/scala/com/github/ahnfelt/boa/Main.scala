package com.github.ahnfelt.boa

object Main {
    def main(arguments : Array[String]) : Unit = {
        val file = arguments(0)
        val flags = arguments.drop(1).toSet
        val boaFile = new Importer().importAndProcess(file, flags)
        println(boaFile)
    }
}
