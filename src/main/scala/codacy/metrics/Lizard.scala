package codacy.metrics

import java.io

import com.codacy.docker.api.utils.CommandRunner
import com.codacy.plugins.api.languages.{Language, Languages}
import com.codacy.plugins.api.metrics.{FileMetrics, LineComplexity, MetricsTool}
import com.codacy.plugins.api.{Options, Source}

import scala.util.{Failure, Properties, Success, Try}
import scala.xml.{Elem, Node, XML}

object Lizard extends MetricsTool {

  override def apply(source: Source.Directory,
                     language: Option[Language],
                     files: Option[Set[Source.File]],
                     options: Map[Options.Key, Options.Value]): Try[List[FileMetrics]] = {
    language match {
      case Some(lang) if lang != Languages.CSharp =>
        Failure(new Exception(s"Lizard only supports C#. Provided language: $lang"))
      case _ =>
        val raw = runTool(source, complexityCommand(files))
        raw.flatMap(parseOutput)
    }
  }

  private def runTool(directory: Source.Directory, command: List[String]): Try[Seq[String]] = {
    CommandRunner.exec(command, Option(new io.File(directory.path))) match {
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
    val document = loadXml(out)

    document.map { xml =>
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

  private def loadXml(out: Seq[String]): Try[Elem] = {
    val xmlString = out.mkString(Properties.lineSeparator)
    Try(XML.loadString(xmlString)).recoverWith {
      case error =>
        Failure(new Exception(s"""|Could not parse the following XML with the following error: ${error.getMessage}
                                  |XML:
                                  |$xmlString""".stripMargin))
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
