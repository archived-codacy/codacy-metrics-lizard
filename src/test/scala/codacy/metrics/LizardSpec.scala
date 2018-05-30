package codacy.metrics

import codacy.docker.api.Source
import codacy.docker.api.metrics.{FileMetrics, LineComplexity}
import org.specs2.mutable.Specification

import scala.util.Success

class LizardSpec extends Specification {

  val fizzBuzz = FileMetrics("codacy/metrics/Fizzbuzz.cs", Some(3), nrMethods = Some(1),lineComplexities = Set(LineComplexity(4,3)))
  val helloWorld = FileMetrics("codacy/metrics/Hello.cs", Some(1), nrMethods = Some(1), lineComplexities = Set(LineComplexity(6, 1)))

  val targetDir = "src/test/resources/"

  "Lizard" should {
    "get metrics" in {
      "all files within a directory" in {
        val expectedFileMetrics = List(fizzBuzz, helloWorld)
        val fileMetricsMap =
          Lizard(source = Source.Directory(targetDir), language = None, files = None, options = Map.empty)

        fileMetricsMap should beLike {
          case Success(elems) => elems should containTheSameElementsAs(expectedFileMetrics)
        }
      }

      "specific files" in {
        val expectedFileMetrics = List(fizzBuzz)

        val fileMetricsMap = Lizard(
          source = Source.Directory(targetDir),
          language = None,
          files = Some(Set(Source.File(fizzBuzz.filename))),
          options = Map.empty)

        fileMetricsMap should beEqualTo(Success(expectedFileMetrics))
      }
    }
  }
}
