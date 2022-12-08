package util

import datalog.{Parser, Program, TypeChecker}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

object Misc {
  def fileToString(filename: String): String = {
    val src = Source.fromFile(filename)
    val s = src.mkString
    src.close()
    s
  }

  def writeToFile(content: String, outFileName: String): Unit = {
    val file = new File(outFileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(content)
    bw.close()
  }

  def getFileNameFromPath(filepath: String): String = filepath.split('/').last.split('.').head

  def createDirectory(path: String): Boolean = {
    val dir = new File(path)
    dir.mkdirs()
  }

  def parseProgram(filepath: String): Program = {
    val filename = Misc.getFileNameFromPath(filepath)
    val inputStr = Misc.fileToString(filepath)
    parseProgramFromRawString(inputStr).setName(filename.capitalize)
  }

  def parseProgramFromRawString(inputStr: String): Program = {
    val parser = new Parser()
    val raw = parser.parseAll(parser.program, inputStr).get
    val typeChecker = TypeChecker()
    typeChecker.updateTypes(raw)
  }


}
