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
			case str if str matchesRegex re_multiplication => {
				var res = str
				// Use findAllIn for multiplication, because multiplication is associative.
				// findAllIn finds non-overlapping matches.
				// This is important to avoid clashes between neighbouring operations of the same type.
				// For instance, "1*2*3" only contains _one_ non-overlapping multiplication, implemented
				// as the first match "1*2".
				for ( multi <- re_multiplication.findAllMatchIn(str) ) {
					val left = multi.toString.split("""\*""", 2)(0).toFloat
					val right = multi.toString.split("""\*""", 2)(1).toFloat
					val multi_res = (left * right).toString
					println("\t=" + multi_res)
					res = res.replace(multi.toString, multi_res)
				}
				eval(res)
			}
			// Use findFirstIn for division, because division is non-associative.
			// 1/2/3/4 = 1/24 != (1/2)/(3/4) = 1/2 * 4/3 = 2/3
			case str if str matchesRegex re_division => {
				var res = str
				re_division.findFirstMatchIn(str).map { divi =>
					val left = divi.toString.split("""/""", 2)(0).toFloat
					val right = divi.toString.split("""/""", 2)(1).toFloat
					val divi_res = (left / right).toString
					println("\t=" + divi_res)
					res = res.replace(divi.toString, divi_res)
					eval(res)
				}
				res
			}	
			
			// should only get here if no priority operations * or / are left inside current substring
			// Use findAllIn for addition, because addition is associative.
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
			// Use findFirstIn for subtraction, because subtraction is non-associative.
			// 1-2-3-4 = -8 != (1-2)-(3-4) = -1 - (-1) = 0
			case str if str matchesRegex re_subtraction => {
				var res = str
				val sub = re_subtraction.findFirstMatchIn(str).get // guaranteed to work as case above checked
				println("sub = " + sub)
				val left = sub.toString.split("""-""", 2)(0).toFloat //TODO splitting doesn't work. -ve sign of first number is erroneously taken to be the operator
				val right = sub.toString.split("""-""", 2)(1).toFloat
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