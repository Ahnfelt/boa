package com.github.ahnfelt.boa

object Main {
    def main(arguments : Array[String]) : Unit = {
        val boaFile = new Importer().importAndProcess(arguments(0))
        println(boaFile)
    }
}
