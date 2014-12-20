package arithmeticparser

import math._
import org.scalatest._
import org.scalatest.Checkpoints._
import Matchers._
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
	("1-2/3","0.3333333333333"),
	("1-2+3*4/5+6-7*8/9","1.17777777778"),
	("1*2/3-4+5*6/7-8+9-10","-8.04761904762"),
	// brackets!
	("5*(6+7)","65.0"),
	("5*(6-7)","-5.0"),
	("5/(6+7)","0.38461538461"),
	("5/(6-7)","-5.0"),
	("3*(6-(20/4))","3.0"),
	)

	"The parser" should "correctly evaluate manually entered formulae" in {
		val parse = new parser
		val cp = new Checkpoint
		forAll[String,String](test_set) { (formula: String, result: String) =>
			println(formula + " = " + result)
			try {
				cp { parse(formula).toDouble / result.toDouble should be (1.0 +- 0.001) }
			} catch {
				case e: Exception => println(e.getMessage)
			}
		}
		cp.reportAll
	}

	"The parser" should "agree with the results of the google search calculator" in {
		val parse = new parser

	}
}