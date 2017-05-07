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

  val seenFiles: mutable.Set[File] = mutable.Set.empty

  def bundle(): Unit = {
    val file = findFile()
    val destFile = new File(destFolder, fileName)
    val pw = new PrintWriter(destFile)
    val outputFileContent: String = buildOutput(file)
    try {
      println(s"writing bundled file to $destFile")
      pw.write(outputFileContent)
    } finally pw.close()
  }

   def buildOutput(file: File): String = {
    val content = transformFile(file)
    strip2(content.filterNot("".==).mkString(System.lineSeparator))
  }

  def findFile(): File = {
    Files.find(Paths.get("."), Int.MaxValue, (path, _) => path.endsWith(fileName))
      .findAny()
      .orElseThrow(() => new IllegalArgumentException(s"$fileName not found"))
      .toFile
  }


  def transformFile(file: File): List[String] = {
    if (seenFiles.contains(file)) return Nil
    seenFiles.add(file)
    println(s"reading from $file")
    val filesInSamePackage = transformFiles(file.getParentFile)
    filesInSamePackage ++ readFile(file).flatMap {
      case l if l.startsWith("package") => Nil
      case i if i.startsWith("import") => transformImport(i)
      case b => List(b)
    }
  }

  val ignoredImports = Seq("scala", "java")

  private def transformImport(im: String): List[String] = {
    println(s"resolving import $im")
    if (ignoredImports.exists(i => im.startsWith(s"import $i"))) List(im)
    else {
      val imported = im.split(" ").tail.mkString

      val elements = imported.split("\\.")
      val lastElt = elements(elements.size - 2)
      val isStarImport = im.endsWith("_") && lastElt.head.isUpper
      //      val folder = elements.dropRight(if (isStarImport) 2 else 1).mkString(File.separator)
      val folder = folderFromImport(imported)
      val importObj = if (isStarImport) List("import " + lastElt + "._") else Nil
      transformFiles(folder) ++ importObj
    }
  }

  private def folderFromImport(im: String) = {
    val sanitized = im.replaceAll("_", "").replaceAll("\\{.*\\}", "")
    //TODO : could only import files listed in { cl1, cl2 }
    val subfolder = sanitized.split("\\.").foldLeft(new File(srcFolder)) {
      case (folder, pkg) =>
        val f = new File(folder, pkg)
        if (f.isDirectory) f else folder
    }
    println(s"$sanitized => $subfolder")
    subfolder
  }

  private def readFile(file: File) = {
    try {
      Source.fromFile(file).getLines().toList
    } catch {
      case e: Throwable =>
        println("Error while reading file " + file)
        e.printStackTrace()
        throw e
    }
  }

  def extractFolderFromImport(im: String): File = {
    new File(srcFolder, im.substring(im.indexOf(" ") + 1, im.lastIndexOf(".")).replace(".", File.separator))
  }

  def transformFiles(folder: File): List[String] = {
    Objects.requireNonNull(folder, "Folder should not be null")
    val visibleFiles: Array[File] = folder.listFiles((pathname: File) => !pathname.getName.startsWith("."))
    Objects.requireNonNull(visibleFiles, "visibleFiles should not be null in folder " + folder)
    visibleFiles.filterNot(_.isDirectory).toList.flatMap(transformFile)
  }

  def strip2(x: String, s: String = "/*", e: String = "*/"): String = {
    val a = x indexOf s
    val b = x indexOf(e, a + s.length)
    if (a == -1 || b == -1) x
    else strip2(x.take(a) + x.drop(b + e.length), s, e)
  }
}

object Bundler extends App {
  val fileName = args.headOption.getOrElse(throw new IllegalArgumentException("Input file name must be provided"))
  new Bundler(fileName).bundle()
}