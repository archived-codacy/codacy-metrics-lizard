package codacy.metrics

import better.files.File
import com.codacy.docker.api.utils.FileHelper
import com.codacy.plugins.api.Source
import com.codacy.plugins.api.languages.Languages.CSharp
import com.codacy.plugins.api.metrics.{FileMetrics, LineComplexity}
import org.specs2.mutable.Specification

import scala.util.Success

class LizardSpec extends Specification {

  val targetDir = File("src/test/resources")

  val fizzBuzz = FileMetrics(
    "codacy/metrics/Fizzbuzz.cs",
    Some(3),
    nrMethods = Some(1),
    lineComplexities = Set(LineComplexity(4, 3)))

  val helloWorld =
    FileMetrics("codacy/metrics/Hello.cs", Some(1), nrMethods = Some(1), lineComplexities = Set(LineComplexity(6, 1)))

  "Lizard" should {
    "get metrics" in {
      "all files within a directory" in {
        val expectedFileMetrics = List(fizzBuzz, helloWorld)
        val fileMetricsMap =
          Lizard(source = Source.Directory(targetDir.pathAsString), language = None, files = None, options = Map.empty)

        fileMetricsMap should beLike {
          case Success(elems) => elems should containTheSameElementsAs(expectedFileMetrics)
        }
      }

      "specific files" in {
        val expectedFileMetrics = List(fizzBuzz)

        val fileMetricsMap = Lizard(
          source = Source.Directory(targetDir.pathAsString),
          language = Option(CSharp),
          files = Some(Set(Source.File(s"${targetDir.pathAsString}/${fizzBuzz.filename}"))),
          options = Map.empty)

        fileMetricsMap.map(_.map(fm => fm.copy(filename = FileHelper.stripPath(fm.filename, targetDir.pathAsString)))) should beEqualTo(
          Success(expectedFileMetrics))
      }
    }
  }
}
