package arithmeticparser

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._

abstract class UnitSpec extends FlatSpec with ShouldMatchers

class parserSpec extends UnitSpec {
	val test_set = Table(
	("formula", "result"),
	// reaction to garbage should be to print it out and exit
	("",""),
	("Hello World!","Hello World!"),
	("R + J = Heart","R + J = Heart"),
	("3.0*PI","3.0*PI"),
	("(6*x)-7","6*x-7"),
	// atomic operations
	("1+2","3.0"),
	("1-2","-1.0"),
	("1*2","2.0"),
	("1/2","0.5"),
	// neighbouring identical operations
	("1+2+3+4+5+6","21.0"),
	("1-2-3-4-5-6","-19.0"),
	("1*2*3*4*5*6","720.0"),
	("1/2/3/4/5/6","0.0013888889"),
	// same-prededence operations
	("1+2-3+4-5+6","5.0"),
	("1*2/3*4/5*6","3.2"),
	// operator precedence
	("1+2*3","7.0"),
	("1+2/3","1.6666666666666"),
	("1-2*3","-5.0"),
	("1-2/3","-0.6666666666666"),
	("1-2+3*4/5+6-7*8/9","1.17777777778"),
	("1*2/3-4+5*6/7-8+9-10","-8.04761904762"),
	// brackets!
	("5*(6+7)","65.0"),
	("5*(6-7)","-5.0"),
	("5/(6+7)","0.38461538461"),
	("5/(6-7)","-5.0"),
	("3*(6-(20/4))","3.0"),
	("",""),
	("",""),
	("",""),
	("","")
	)

	"The parser" should "get the maths right" in {
		val parse = new parser
		forAll[String,String](test_set) { (formula: String, result: String) =>
			println(formula + " = " + result)
			parse(formula) should equal (result)
		}
	}
	// val parse = new parser
	// for (test <- test_set) { (formula: String, result: String) =>
	// 	val behave = "calculate " + formula + " = " + result
	// 	"The parser" should behave in {
	// 		parse(formula) should equal (result)
	// 	}
	// }
}