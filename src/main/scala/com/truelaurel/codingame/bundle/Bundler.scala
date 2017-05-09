package com.truelaurel.codingame.bundle

import java.io.File

object BundlerMain extends App {
  if (args.isEmpty) {
    println("Input file name must be provided")
    sys.exit(1)
  }
  args.foreach { fileName =>
    Bundler(fileName, StdBundlerIo()).bundle()
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
    val content = transformFile(file)
    stripComments(content.mkString("\n"))
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
    println(allFiles)
    allFiles.flatMap(f => io.readFile(f).flatMap(transformedLine)).filterNot("".==)
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

  private def transformedLine(line: String): Option[String] = line match {
    case l if l.startsWith("package") => None
    case im if im.startsWith("import") && ignoredImports.exists(i => im.startsWith(s"import $i")) =>
      Some(im)
    case im if im.startsWith("import") =>
      val imported = im.split(" ").tail.mkString
      val elements = imported.split("\\.")
      val lastElt = elements(elements.size - 2)
      val isStarImport = im.endsWith("_") && lastElt.head.isUpper
      if (isStarImport) Some("import " + lastElt + "._")
      else None
    case b => Some(b)
  }
}