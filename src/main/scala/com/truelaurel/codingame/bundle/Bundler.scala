package com.truelaurel.codingame.bundle

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util.Objects

import scala.collection.mutable
import scala.io.Source

class Bundler(val fileName: String,
              val srcFolder: String = "./src/main/scala",
              val destFolder: String = "./target",
              val organization: String = "com.truelaurel"
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
    val lines = readFile(file)
    val withoutPackages = lines.filterNot(_.startsWith("package"))
    val (imports, body) = withoutPackages.partition(_.startsWith(s"import $organization"))
    val filesInSamePackage = transformFiles(file.getParentFile)
    val (starImports, otherImports) = imports.partition(_.endsWith("_"))
    val filesFromImports = otherImports.flatMap(im => {
      val folderFromImport = extractFolderFromImport(im)
      transformFiles(folderFromImport)
    })
    starImports.distinct.map(_.replace(s"$organization.", "")) ++ filesInSamePackage ++ filesFromImports ++ body
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
  val organisation = if (args.length > 1) args(1) else "com.truelaurel"
  new Bundler(fileName, organization = organisation).bundle()
}