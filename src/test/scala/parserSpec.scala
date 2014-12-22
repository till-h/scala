package arithmeticparser

import math._
import org.scalatest._
import org.scalatest.Checkpoints._
import matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._
import dispatch._
import dispatch.Defaults._

abstract class UnitSpec extends FlatSpec with ShouldMatchers

class parserSpec extends UnitSpec {


	val fractional_tolerance = 0.0001


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
		// Going wild
		("5*8/83-266+(82-5*6/(3*7-9)/77)*1/8","-255.272130731")
	)

	val formulae = Array(
		"6/(7+14)-9+(8*0)",
		"4+6",
		"4-6",
		"4*6",
		"4/6",
		"1-2-3-4-5-6"
		"1+2*3+4*5",
		"1+2+3+4+5",
		"1000/10/20/5/4",
		"1*2*3*4*5*6",
		"10*2+(10-8)*5",
		"1+10*2/2",
		"1-1",
		"1*1",
		"1/1",
		"2*(3*4)*(5*(6*7))",
		"2*(3*4)*(5*(6*7))*(2*6*7*1*(6*7*(8*(9)*4*2)*1)*1)*7"
	)


	val parse = new parser


	// Define matcher to check whether a list of possible results of a calculation contains
	// one satisfactory result.
	val containAtLeastOneElementNearUnity = new Matcher[List[Float]] {
    	def apply(list: List[Float]) = MatchResult(
        	{
        		var res = false
        		for (e <- list) { if (abs(e - 1.0) < fractional_tolerance) { res = true } }
        		res
        	},
        	list + " did not contain an element close to unity",
        	list + " did contain an element close to unity"
        )
  	}


	"The parser" should "correctly evaluate manually entered formulae" in {
		val cp = new Checkpoint
		forAll[String,String](test_set) { (formula: String, result: String) =>
			println("Testing: " + formula + " = " + result)
			try {
				val result_p = parse(formula).toDouble
				val result_m = result.toDouble
				if (!(result_p == 0.0 && result_m == 0.0)) {
					cp { result_p / result_m should be (1.0 +- fractional_tolerance) }
				}
			} catch {
				case e: Exception => println("Non-evaluation related exception:" + e.getMessage)
			}
		}
		cp.reportAll
	}


	"The parser" should "agree with the results of the Google online calculator" in {
		// To fish out the calculation result from the response body
		val re_formula_simple = ("""[0-9eE\./\-\*\+\(\) ]+ = ([0-9\-\u00A0]+)""").r
		val cp = new Checkpoint
		for (f <- formulae) {
			println("Testing " + f)
			val parsed = parse(f).toFloat
			val request_url = url("https://www.google.co.uk/search?q=" + f.replace("+", "%2B"))
			val async_response = Http(request_url OK as.String)
			val response = async_response() // force execution
			val ratio_list = re_formula_simple.findAllMatchIn(response).map( m => (m.group(1).replace("\u00A0", "").toFloat / parsed) ).toList
			println(re_formula_simple.findAllMatchIn(response).toList)
			println("\t" + ratio_list.toString)
			cp { ratio_list should containAtLeastOneElementNearUnity }
		}
		cp.reportAll
	}


}