package codacy.metrics

import java.io

import codacy.docker.api.metrics.{FileMetrics, LineComplexity, MetricsTool}
import codacy.docker.api.{MetricsConfiguration, Source}
import com.codacy.api.dtos.Language
import com.codacy.docker.api.utils.CommandRunner

import scala.util.{Failure, Properties, Success, Try}
import scala.xml.{Node, XML}

object Lizard extends MetricsTool {

  override def apply(source: Source.Directory,
                     language: Option[Language], // Filter by language currently not supported
                     files: Option[Set[Source.File]],
                     options: Map[MetricsConfiguration.Key, MetricsConfiguration.Value]): Try[List[FileMetrics]] = {
    calculateComplexity(source.path, files)
  }

  private def calculateComplexity(directory: String, files: Option[Set[Source.File]]): Try[List[FileMetrics]] = {
    val raw = runTool(directory, complexityCommand(files))
    raw.flatMap(parseOutput)
  }

  private def runTool(directory: String, command: List[String]): Try[Seq[String]] = {
    val ioDirectoryOpt = new io.File(directory)

    CommandRunner.exec(command, Option(ioDirectoryOpt)) match {
      case Right(output) =>
        Success(output.stdout)
      case Left(error) =>
        Failure(new Exception(error))
    }
  }

  private def complexityCommand(files: Option[Set[Source.File]]): List[String] = {
    val relativePaths = files.getOrElse(Set.empty).map(_.path)

    List("lizard", "-X", "--languages", "csharp", "--CCN", "0") ++ relativePaths
  }

  private def parseOutput(out: Seq[String]): Try[List[FileMetrics]] = {
    Try {
      val xml = XML.loadString(out.mkString(Properties.lineSeparator))

      val complexityByFileMap = complexityByFile(xml)

      complexityByFileMap.map {
        case (fileName, linesComplexity) =>
          val fileComplexity: Int = (linesComplexity.map(_.value) ++ Set(0)).max

          FileMetrics(
            fileName,
            Some(fileComplexity),
            lineComplexities = linesComplexity,
            nrMethods = Option(linesComplexity.size)) // since each line is a method/function)
      }(collection.breakOut)
    }
  }

  private def complexityByFile(xml: Node): Map[String, Set[LineComplexity]] = {
    val FileNameLineMatch = ".*? at [.]?/?(.*):(.*)".r

    val complexitySeq = for {
      measure <- (xml \\ "measure").toList
      if isTypeFunction(measure)
      method <- (measure \\ "item").toList
      complexity <- (method \\ "value").toList.lastOption.map(_.text).toList
      FileNameLineMatch(filename, lineNumber) <- method.attribute("name").flatMap(_.headOption.map(_.text)).toList
    } yield {
      (filename, LineComplexity(lineNumber.toInt, complexity.toInt))
    }

    val complexityByFile: Map[String, Set[LineComplexity]] = complexitySeq.groupBy {
      case (filename, _) => filename
    }.mapValues(_.map {
      case (_, complexity) => complexity
    }(collection.breakOut))

    complexityByFile
  }

  private def isTypeFunction(measure: Node): Boolean = {
    measure.attribute("type").exists(_.headOption.exists(_.text == "Function"))
  }
}
