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

  def bundle() {
    val file = findFile()
    val content = transformFile(file)
    val pw = new PrintWriter(new File(destFolder, fileName))
    try pw.write(strip2(content.filterNot(_.equals("")).mkString(System.lineSeparator()))) finally pw.close()
  }

  def findFile(): File = {
    Files.find(Paths.get("."), Int.MaxValue, (path, attrs) => path.endsWith(fileName))
      .findAny()
      .orElseThrow(() => new IllegalArgumentException(s"$fileName not found"))
      .toFile
  }


  def transformFile(file: File): List[String] = {
    if (seenFiles.contains(file)) return Nil
    seenFiles.add(file)
    val filesInSamePackage = transformFiles(file.getParentFile)
    filesInSamePackage ++ readFile(file).flatMap {
      case l if l.startsWith("package") => Nil
      case i if i.startsWith("import") => transformImport(i)
      case b => List(b)
    }
  }

  private def transformImport(im: String): List[String] =
    if (im.startsWith("import scala")) List(im)
    else {
      val Array(_, imported) = im.split(" ")
      val isStarImport = im.endsWith("_")
      val elements = imported.split("\\.")
      val folder = elements.dropRight(if (isStarImport) 2 else 1).mkString(File.separator)
      val importObj = if (isStarImport) List("import " + elements(elements.size - 2) + "._") else Nil
      transformFiles(new File(srcFolder, folder)) ++ importObj
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