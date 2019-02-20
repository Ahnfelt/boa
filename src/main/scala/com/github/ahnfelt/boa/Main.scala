package com.github.ahnfelt.boa

import scala.io.Source

object Main {
    def main(arguments : Array[String]) : Unit = {
        val boaFile = new Importer().importAndProcess(arguments(0))
        println(boaFile)
    }
}
