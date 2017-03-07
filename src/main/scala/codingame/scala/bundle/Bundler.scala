package codingame.scala.bundle

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util.Objects

import scala.collection.mutable
import scala.io.Source

class Bundler(private val fileName: String = "Player.scala",
              private val srcFolder: String = "./src/main/scala",
              private val destFolder: String = "./target") {

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
    val (imports, withoutPackagesImports) = withoutPackages.partition(_.startsWith("import com.truelaurel"))
    val filesInSamePackage = transformFiles(file.getParentFile)
    val filesFromImports = imports.flatMap(im => transformFiles(extractFolderFromImport(im)))
    filesInSamePackage ++ filesFromImports ++ withoutPackagesImports
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
    val files: Array[File] = folder.listFiles((pathname: File) => !pathname.getName.startsWith("."))
    Objects.requireNonNull(files, "files should not be null in folder " + folder)
    files.toList.flatMap(transformFile)
  }

  def strip2(x: String, s: String = "/*", e: String = "*/"): String = {
    val a = x indexOf s
    val b = x indexOf(e, a + s.length)
    if (a == -1 || b == -1) x
    else strip2(x.take(a) + x.drop(b + e.length), s, e)
  }
}

object Bundler {
  def main(args: Array[String]): Unit = {
    new Bundler().bundle()
  }
}