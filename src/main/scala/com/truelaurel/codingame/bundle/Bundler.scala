package com.truelaurel.codingame.bundle

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util.Objects

import scala.collection.mutable
import scala.io.Source


class Bundler(val fileName: String,
              val srcFolder: String = "./src/main/scala",
              val destFolder: String = "./target"
             ) {

  import Bundler._

  val ignoredImports = Seq("scala", "java")
  val seenFiles = mutable.Set.empty[File]

  def bundle(): Unit = {
    val file = findFile(fileName)
    val destFile = new File(destFolder, fileName)
    val pw = new PrintWriter(destFile)
    val outputFileContent = buildOutput
    try {
      println(s"writing bundle for $file to $destFile")
      pw.write(outputFileContent)
    } finally pw.close()
  }

  def buildOutput: String = {
    val file = findFile(fileName)
    val content = transformFile(file)
    strip2(content.filterNot("".==).mkString(System.lineSeparator))
  }

  def transformFile(file: File): List[String] =
    if (seenFiles.contains(file)) Nil
    else {
      seenFiles.add(file)
      val filesInSamePackage = transformFilesFromFolder(file.getParentFile)
      val lines = readFile(file)
      filesInSamePackage ++ transformContent(lines)
    }

  private def transformContent(lines: List[String]) =
    lines.flatMap {
      case l if l.startsWith("package") => Nil
      case i if i.startsWith("import") => transformImport(i)
      case b => List(b)
    }

  private def folderFromImport(im: String): Option[File] =
    if (ignoredImports.exists(i => im.startsWith(s"import $i"))) None
    else {
      val imported = im.split(" ").tail.mkString
      val sanitized = imported.replaceAll("_", "").replaceAll("\\{.*\\}", "")
      //TODO : could only import files listed in { cl1, cl2 }
      val subFolder = sanitized.split("\\.").foldLeft(new File(srcFolder)) {
        case (folder, pkg) =>
          val f = new File(folder, pkg)
          if (f.isDirectory) f else folder
      }
      println(s"$imported => $subFolder")
      Some(subFolder)
    }


  private def importToLine(im: String): Option[String] =
    if (ignoredImports.exists(i => im.startsWith(s"import $i")))
      Some(im)
    else {
      val imported = im.split(" ").tail.mkString
      val elements = imported.split("\\.")
      val lastElt = elements(elements.size - 2)
      val isStarImport = im.endsWith("_") && lastElt.head.isUpper
      if (isStarImport) Some("import " + lastElt + "._")
      else None
    }

  private def transformImport(im: String): List[String] = {
    println(s"resolving import $im")
    val transformed = folderFromImport(im).toList.flatMap(transformFilesFromFolder)
    val line = importToLine(im)
    transformed ++ line
  }

  def extractFolderFromImport(im: String): File =
    new File(srcFolder, im.substring(im.indexOf(" ") + 1, im.lastIndexOf(".")).replace(".", File.separator))

  def transformFilesFromFolder(folder: File): List[String] =
    filesInFolder(folder).flatMap(transformFile)

}

object Bundler extends App {
  val fileName = args.headOption.getOrElse(throw new IllegalArgumentException("Input file name must be provided"))
  new Bundler(fileName).bundle()

  def readFile(file: File): List[String] = {
    println(s"reading from $file")
    try {
      Source.fromFile(file).getLines().toList
    } catch {
      case e: Throwable =>
        println("Error while reading file " + file)
        e.printStackTrace()
        throw e
    }
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
    files.filterNot(_.isDirectory).toList
  }


  def strip2(x: String, s: String = "/*", e: String = "*/"): String = {
    val a = x indexOf s
    val b = x indexOf(e, a + s.length)
    if (a == -1 || b == -1) x
    else strip2(x.take(a) + x.drop(b + e.length), s, e)
  }
}