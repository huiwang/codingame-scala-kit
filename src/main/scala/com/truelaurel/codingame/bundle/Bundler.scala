package com.truelaurel.codingame.bundle

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util.Objects

import scala.collection.mutable
import scala.io.Source
import scala.util.control.NonFatal

object BundlerMain extends App {
  if (args.isEmpty) {
    println("Input file name must be provided");
    sys.exit(1)
  }
  args.foreach { fileName =>
    new Bundler(fileName, BundlerIo).bundle()
  }
}

class Bundler(val fileName: String,
              io: BundlerIo,
              val srcFolder: String = "./src/main/scala",
              val destFolder: String = "./target") {

  import Bundler._

  val seenFiles = mutable.Set.empty[File]

  def bundle(): Unit = {
    val outputFileContent = buildOutput
    val destFile = new File(destFolder, fileName)
    io.save(destFile, outputFileContent)
  }

  def buildOutput: String = {
    val file = io.findFile(fileName)
    val content = transformFile(file)
    stripComments(content.mkString("\n"))
  }

  def transformFile(file: File): List[String] =
    if (seenFiles.contains(file)) Nil
    else {
      seenFiles.add(file)
      val fileLines = io.readFile(file)
      val keptFileLines = fileLines.flatMap(transformedLine)

      val filesInSameFolder = io.filesInFolder(file.getParentFile)
      val filesFromImport = fileLines.flatMap(filesFromLine)
      val neededFiles = filesFromImport ++ filesInSameFolder
      val allLines = neededFiles.flatMap(transformFile) ++ keptFileLines

      allLines.filterNot("".==)
    }

  private def filesFromLine(line: String): List[File] =
    if (!line.startsWith("import") || ignoredImports.exists(i => line.startsWith(s"import $i")))
      Nil
    else {
      val imported = line.split(" ").tail.mkString
      val packageElements = imported.replaceAll("_", "").replaceAll("\\{.*\\}", "").split("\\.")
      val subFolder = io.findFolder(packageElements, new File(srcFolder))
      io.filesInFolder(subFolder)
    }
}

trait BundlerIo {
  def readFile(file: File): List[String] = {
    println(s"reading from $file")
    try {
      Source.fromFile(file).getLines().toList
    } catch {
      case NonFatal(e) =>
        println("Error while reading file " + file)
        e.printStackTrace()
        throw e
    }
  }

  def findFolder(packageElements: Array[String], rootFolder: File): File = {
    packageElements.foldLeft(rootFolder) {
      case (folder, pkg) =>
        val f = new File(folder, pkg)
        //TODO : could only import files listed in { cl1, cl2 }
        if (f.isDirectory) f else folder
    }
  }

  def save(destFile: File, content: String): Unit = {
    val pw = new PrintWriter(destFile)
    try {
      println(s"writing to $destFile")
      pw.write(content)
    } finally pw.close()

  }

  def findFile(fileName: String): File = {
    Files.find(Paths.get("."), Int.MaxValue, (path, _) => path.endsWith(fileName))
      .findAny()
      .orElseThrow(() => new IllegalArgumentException(s"$fileName not found"))
      .toFile
  }

  def filesInFolder(folder: File): List[File] = {
    Objects.requireNonNull(folder, "Folder should not be null")
    val files = folder.listFiles((pathname: File) => !pathname.getName.startsWith("."))
    Objects.requireNonNull(files, "visibleFiles should not be null in folder " + folder)
    files.filterNot(_.isDirectory).toList.sortBy(_.getAbsolutePath)
  }
}

object BundlerIo extends BundlerIo

object Bundler {
  val ignoredImports = Seq("scala", "java")

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