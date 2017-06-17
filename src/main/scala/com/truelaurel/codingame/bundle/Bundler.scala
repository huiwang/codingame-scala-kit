package com.truelaurel.codingame.bundle

import java.io.File

object BundlerMain {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Input file name must be provided")
      sys.exit(1)
    }
    args.foreach { fileName =>
      Bundler(fileName, StdBundlerIo()).bundle()
    }
  }
}

case class Bundler(fileName: String, io: BundlerIo) {
  val ignoredImports = Seq("scala", "java")

  def bundle(): Unit = {
    val outputFileContent = buildOutput
    io.save(fileName, outputFileContent)
  }

  def buildOutput: String = {
    val file = io.findFile(fileName)
    val content = transformFile(file).mkString("\n")
    stripComments(content)
  }

  def dependentFiles(file: File, fileLines: List[String]): List[File] = {
    val filesInSameFolder = io.filesInFolder(file.getParentFile)
    val filesFromImport = fileLines.flatMap(filesFromLine)
    filesFromImport ++ filesInSameFolder
  }

  def filesList(queue: List[File], contents: Map[File, List[String]]): List[File] = queue match {
    case Nil => Nil
    case file :: t =>
      val fileLines = io.readFile(file)
      val needed = dependentFiles(file, fileLines)
      val nextContents = contents + (file -> fileLines)
      val nextQueue = (needed ++ t).distinct.diff(nextContents.keySet.toSeq)
      filesList(nextQueue, nextContents) ++ needed :+ file
  }

  def transformFile(file: File): List[String] = {
    val allFiles = filesList(List(file), Map.empty).distinct
    allFiles.map(transformSingleFile)
  }

  private def transformSingleFile(f: File): String = {
    val lines = io.readFile(f)
    val (pkgLines, rest) = lines.span(_.startsWith("package"))
    val result = rest.map(_.trim).filterNot("".==).mkString("\n")
    pkgLines match {
      case Nil => result
      case List(pkgLine) =>
        val pkgName = pkgLine.drop("package ".size)
        s"""package object $pkgName {
           |$result
           |}
         """.stripMargin
      case _ => throw new Exception("Bundler does not support yet multiple packages declaration")
    }
  }

  private def filesFromLine(line: String): List[File] =
    if (!line.startsWith("import") || ignoredImports.exists(i => line.startsWith(s"import $i")))
      Nil
    else {
      val imported = line.split(" ").tail.mkString
      val packageElements = imported.replaceAll("_", "").replaceAll("\\{.*\\}", "").split("\\.")
      val subFolder = io.findFolder(packageElements)
      io.filesInFolder(subFolder)
    }

  def stripComments(x: String, s: String = "/*", e: String = "*/"): String = {
    val a = x indexOf s
    val b = x indexOf(e, a + s.length)
    if (a == -1 || b == -1) x
    else stripComments(x.take(a) + x.drop(b + e.length), s, e)
  }
}