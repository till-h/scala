package arithmeticparser

import org.tillh.utils.StringUtils._

class parser {
	// constructor
	private var iteration = 0
	private val operators            = List("*","/","+","-")
	// TODO these need lots of unit tests
	private val re_innermost_bracket = """(\([^\)\(]+\))""".r
	private val float                = """((?<![0-9])-|^-)?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?""" /** ?<! is a negative lookbehind */
	private val re_multiplication    = (float + """\*""" + float).r
	private val re_division          = (float + """/"""  + float).r
	private val re_addition          = (float + """\+""" + float).r
	private val re_subtraction       = (float + """-"""  + float).r

	private def eval(str: String): String = {
		
		iteration = iteration + 1
		println(iteration.toString ++ "\t" ++ str)

		str match {
			
			// match in order of operation priority
			case str if (str.contains("(") || str.contains(")")) => {
				// use regex to match all atomic (...) occurences and replace these by eval((...)) in each case.
				val res = re_innermost_bracket.replaceAllIn(str, bracket => eval(bracket.toString.replaceAll("[()]", "")))
				eval(res)
			}
			
			// should only get here if no brackets are left inside current substring
			// Following convention, "comb" through the expression from left to right,
			// evaluating first the leftmost * OR / operation, then combing through
			// the result until no * or / is left, THEN doing the same for + OR -.
			// The important thing is to prioritise * and / equally, above the equally
			// prioritised + and -.

			// TODO equal prioritisation of (*,/) and (+,-):
			// combine them into one regex to check for and extract the operator per match group
			// perform operation depending on operator detected.
			case str if str matchesRegex re_multiplication => {
				var res = str
				for ( multi <- re_multiplication.findAllMatchIn(str) ) {
					val left = multi.toString.split("""\*""", 2)(0).toFloat
					val right = multi.toString.split("""\*""", 2)(1).toFloat
					val multi_res = (left * right).toString
					println("\t=" + multi_res)
					res = res.replace(multi.toString, multi_res)
				}
				eval(res)
			}
			
			case str if str matchesRegex re_division => {
				var res = str
				val divi = re_division.findFirstIn(str).get
				val floats = (float.r findAllIn str).toList
				val left = floats(0).toFloat
				val right = floats(1).toFloat
				val divi_res = (left / right).toString
				println("\t=" + divi_res)
				res = res.replace(divi.toString, divi_res)
				eval(res)
			}		
			
			case str if str matchesRegex re_addition => {
				var res = str
				for ( add <- re_addition.findAllMatchIn(str) ) {
					val left = add.toString.split("""\+""", 2)(0).toFloat
					val right = add.toString.split("""\+""", 2)(1).toFloat
					val add_res = (left + right).toString
					println("\t=" + add_res)
					res = res.replace(add.toString, add_res)
				}
				eval(res)
			}

			case str if str matchesRegex re_subtraction => {
				var res = str
				val sub = re_subtraction.findFirstIn(str).get // guaranteed to work as case above checked
				val floats = (float.r findAllIn str).toList
				val left = floats(0).toFloat
				val right = floats(1).toFloat
				val sub_res = (left - right).toString
				println("\t=" + sub_res)
				res = res.replace(sub.toString, sub_res)
				eval(res)
			}
			case str_no_operators_left => { str_no_operators_left }
		
		}
	}

	def apply(str: String) = { iteration = 0; eval(str) }
}

object Main extends App {
	//println(parser("2*(3*4)*(5*(6*7))"))
	//println(parser("2*(3*4)*(5*(6*7))*(2*6*7*1*(6*7*(8*(9)*4*2)*1)*1)*7"))
	val parser = new parser
	println(parser("1-2-3-4-5-6"))
	println(parser("1+2*3+4*5"))
	println(parser("1+2+3+4+5"))
	println(parser("1000/10/20/5/4"))
	println(parser("1*2*3*4*5*6"))
	println()
	println()
	println(parser("10*2+(10-8)*5"))
	//println(parser("1+10*2/2"))
	//println(parser(""))
	//println(parser(""))
	//println(res = parser("1-1"))
	//println(res = parser("1*1"))
	//println(res = parser("1/1"))
}