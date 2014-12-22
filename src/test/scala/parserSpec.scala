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
		("3*(6-(20/4))","3.0")
	)

	val formulae = Array(
		// "6/(7+14)-9+(8*0)",
		// "4+6",
		// "4-6",
		// "4*6",
		// "4/6",
		// "1-2-3-4-5-6",
		// "1+2*3+4*5",
		// "1+2+3+4+5",
		// "1000/10/20/5/4",
		// "1*2*3*4*5*6",
		// "10*2+(10-8)*5",
		// "1+10*2/2",
		// "1-1",
		// "1*1",
		// "1/1",
		// "2*(3*4)*(5*(6*7))",
		"2*(3*4)*(5*(6*7))*(2*6*7*1*(6*7*(8*(9)*4*2)*1)*1)*7"
	)

	val parse = new parser

	"The parser" should "correctly evaluate manually entered formulae" in {
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

	// Define matcher to check whether a list of possible results of a calculation contains
	// one satisfactory result.
	val containAnElementNearUnity = new Matcher[List[Float]] {
    	def apply(list: List[Float]) = MatchResult(
        	{
        		var res = false
        		for (e <- list) { if (abs(e - 1.0) < 0.001) { res = true } }
        		res
        	},
        	list + " did not contain an element close to unity",
        	list + " did contain an element close to unity"
        )
  	}

  	// To recognise the part of Google's huge query response containing the result of the calculation
  	val re_formula_simple = ("""[0-9eE\./\-\*\+\(\) ]+ = """ + ParserRegex.float).r

	"The parser" should "agree with the results of the google online calculator" in {
		for (f <- formulae) {
			val parsed = parse(f).toFloat
			val request_url = url("https://www.google.co.uk/search?q=" + f.replace("+", "%2B"))
			val response = Http(request_url OK as.String)
			for (r <- response) { // asynchronously executed (e.g. order of execution not preserved)
				println("\n" + raw_url)
				re_formula_simple.findAllMatchIn(r).map( m => (m.group(1).toFloat / parsed).toFloat).toList should containAnElementNearUnity
			}
		}
	}
}