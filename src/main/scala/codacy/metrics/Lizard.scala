package codacy.metrics

import java.io

import better.files._
import codacy.docker.api.metrics.{FileMetrics, MetricsTool, LineComplexity}
import codacy.docker.api.{MetricsConfiguration, Source}
import com.codacy.api.dtos.Language
import com.codacy.docker.api.utils.CommandRunner

import scala.util.{Failure, Properties, Success, Try}
import scala.xml.XML

final case class LizardFileComplexity(fileName: String, methods: Seq[LizardMethodComplexity])

final case class LizardMethodComplexity(complexity: Int, fileName: String, row: Int)


object Lizard extends MetricsTool {

  override def apply(source: Source.Directory,
                     language: Option[Language], // Filter by language currently not supported
                     files: Option[Set[Source.File]],
                     options: Map[MetricsConfiguration.Key, MetricsConfiguration.Value]): Try[List[FileMetrics]] = {
    val javaFiles: Option[Seq[io.File]] = files.map(_.map(file => new io.File(source.path + file.path))(collection.breakOut))

    calculateComplexity(source.path, javaFiles).map(_.map {
      lizardFileComplexity =>

        val fileComplexity: Int = lizardFileComplexity.methods.map(_.complexity).:+(0).max

        val linesComplexity: Set[LineComplexity] =
          (for {
            methodComplexity <- lizardFileComplexity.methods
          } yield LineComplexity(methodComplexity.row, methodComplexity.complexity))(collection.breakOut)

        FileMetrics(lizardFileComplexity.fileName, Some(fileComplexity),
          lineComplexities = linesComplexity,
          nrMethods = Option(linesComplexity.size)) // since each line is a method/function)
    })

  }

  private def calculateComplexity(directory: String, files: Option[Seq[io.File]]): Try[List[LizardFileComplexity]] = {
    val raw = runTool(directory, complexityCommand(directory, files))
    parseOutput(raw)
  }

  private def runTool(directory: String, command: List[String]): Seq[String] = {
    val ioDirectoryOpt =
      JavaHelper.safeCall(new io.File(directory)).toOption

    CommandRunner.exec(command, ioDirectoryOpt) match {
      case Right(output) =>
        output.stdout
      case Left(_) =>
        Seq.empty
    }
  }

  private def complexityCommand(directory: String, files: Option[Seq[io.File]]): List[String] = {
    val dir = File(directory)
    val relativePaths = files.map(_.map { file =>
      dir.relativize(file.toScala).toString
    }).getOrElse(Seq.empty)

    List("lizard", "-X", "--languages", "csharp", "--CCN", "0") ++ relativePaths
  }

  private def parseOutput(out: Seq[String]): Try[List[LizardFileComplexity]] = {
    val FileNameLineMatch = ".*? at [.]?/?(.*):(.*)".r
    Try {
      val xml = XML.loadString(out.mkString(Properties.lineSeparator))
      val methodsComplexity = (xml \\ "measure").toList
        .filter(_.attribute("type").exists(_.headOption.exists(_.text == "Function")))
        .flatMap { measure =>
          (measure \\ "item").toList.flatMap { method =>
            for {
              complexity <- (method \\ "value").toList.lastOption.map(_.text)
              FileNameLineMatch(filename, lineNumber) <- method.attribute("name").flatMap(_.headOption.map(_.text))
            } yield LizardMethodComplexity(complexity.toInt, filename, lineNumber.toInt)
          }
        }

      methodsComplexity.groupBy(_.fileName).map(LizardFileComplexity.tupled(_))(collection.breakOut)

    }
  }
}

object JavaHelper {

  private val defaultException: Throwable = new Exception("Java call returned null")

  def safeCall[T](call: => T): Try[T] = {
    Try {
      Option(call).fold[Try[T]](Failure(defaultException))(Success(_))
    }.flatten
  }

}
