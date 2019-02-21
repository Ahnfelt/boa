package com.github.ahnfelt.boa

import com.github.ahnfelt.boa.Syntax._

class Checker(imported : Map[String, BoaFile]) {

    def checkFile(boaFile : BoaFile) : BoaFile = {
        boaFile.copy(
            methods = boaFile.methods.map(checkMethod)
        )
    }

    def checkMethod(method : Method) : Method = {
        method
    }

}
