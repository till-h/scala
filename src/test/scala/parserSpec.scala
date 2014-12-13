package arithmeticparser

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._

abstract class UnitSpec extends FlatSpec with ShouldMatchers

class parserSpec extends UnitSpec {
	val test_set = Table(
	("formula", "result"),
	("1+2","3"),
	("1-2","-1"),
	("1*2","2"),
	("1/2","0.5")
	)

	"The parser" should "get the maths right" in {
		val parse = new parser
		forAll[String,String](test_set) { (formula: String, result: String) =>
			parse(formula) should equal (result)
		}
	}
}