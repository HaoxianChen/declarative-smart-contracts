package util

import datalog.{Parser, Program, TypeChecker}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import java.nio.file.{Files, Paths, StandardOpenOption}

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

  def readMaterializedRelationNames(filepath: String): Array[Array[String]] = {
    val str = fileToString(filepath)
    val lines = str.split("\n")
    lines.map(l=>l.split(",").map(_.trim()).filter(_.nonEmpty))
  }

  def isFileExists(filepath: String): Boolean = {
    val file = new File(filepath)
    file.exists()
  }

  def parseProgramFromRawString(inputStr: String): Program = {
    val parser = new Parser()
    val raw = parser.parseAll(parser.program, inputStr).get
    val typeChecker = TypeChecker()
    typeChecker.updateTypes(raw)
  }

  def appendToFile(text: String, filePath: String): Unit = {
    val path = Paths.get(filePath)
    Files.write(path, text.getBytes("UTF-8"), StandardOpenOption.CREATE, StandardOpenOption.APPEND)
  }


  // New helpers for split .dl directories (filesystem-based)
  /**
   * Combine split files (schema.dl, rules.dl, properties.dl) located inside `dirPath`
   * into a single datalog program string and return it. If a part is missing it will be skipped.
   * NOTE: this no longer writes a combined file to disk; it returns the combined content.
   */
  def combineSplitFilesToFile(dirPath: String): String = {
    val dir = Paths.get(dirPath)
    val schemaPath = dir.resolve("schema.dl").toString
    val rulesPath = dir.resolve("rules.dl").toString
    val propsPath = dir.resolve("properties.dl").toString

    val parts = Seq(schemaPath, rulesPath, propsPath).flatMap { p =>
      if (isFileExists(p)) {
        val content = fileToString(p)
        val withNewline = if (content.endsWith("\n")) content else content + "\n"
        Some(withNewline)
      } else None
    }

    val combined = parts.mkString("\n")
    combined
  }

  /** Parse a datalog Program from a split-directory produced by the splitter script. */
  def parseProgramFromSplitDir(dirPath: String): Program = {
    val combined = combineSplitFilesToFile(dirPath)
    // Derive a program name from the directory name
    val dirName = Paths.get(dirPath).getFileName.toString
    parseProgramFromRawString(combined).setName(dirName.capitalize)
  }

  /** Parse all split directories under `parentDir` and return a sequence of (dirName, Program). */
  def parseAllProgramsFromSplitParent(parentDir: String): Seq[(String, Program)] = {
    val parent = new File(parentDir)
    if (!parent.exists() || !parent.isDirectory) return Seq.empty
    val subdirs = parent.listFiles().filter(_.isDirectory).map(_.getName).sorted
    subdirs.map { name =>
      val dir = Paths.get(parentDir, name).toString
      val prog = parseProgramFromSplitDir(dir)
      (name, prog)
    }
  }

  // New helpers that operate on strings (no filesystem)
  /**
   * Combine the contents of schema, rules, and properties (each as a string) into a single
   * datalog program string. Any of the inputs can be empty or null; they will be skipped.
   */
  def combineSplitStrings(schema: String, rules: String, properties: String): String = {
    val parts = Seq(schema, rules, properties).filter(p => p != null && p.trim.nonEmpty).map { s =>
      if (s.endsWith("\n")) s else s + "\n"
    }
    parts.mkString("\n")
  }

  /**
   * Parse a Program from the concatenation of schema, rules, and properties strings.
   * Optionally provide a name to assign to the parsed Program. This avoids writing any
   * combined file to disk.
   */
  def parseProgramFromSplitStrings(schema: String, rules: String, properties: String, nameOpt: String = ""): Program = {
    val combined = combineSplitStrings(schema, rules, properties)
    val prog = parseProgramFromRawString(combined)
    if (nameOpt != null && nameOpt.nonEmpty) prog.setName(nameOpt) else prog
  }



}
