object parser {
	// constructor
	private var iteration = 0
	private val operators = List("*","/","+","-")
	private val re_innermost_bracket = """(\([^\)\(]+\))""".r

	private def split_and_eval(str: String, re: String) = {
		var left = str.split(re, 2)(0)
		var right = str.split(re, 2)(1)
		if (operators.exists(left.contains(_))) { left = eval(left) }
		if (operators.exists(right.contains(_))) { right = eval(right) }
		(left, right)
	}

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
			case str if str.contains("*") => {
				val (left, right) = split_and_eval(str, "\\*")
				val res = left.toFloat * right.toFloat
				res.toString
			}
			case str if str.contains("/") => {
				val (left, right) = split_and_eval(str, "/")
				val res = left.toFloat / right.toFloat
				res.toString
			}
			// should only get here if no priority operations * or / are left inside current substring
			case str if str.contains("+") => {
				val (left, right) = split_and_eval(str, "\\+")
				val res = left.toFloat + right.toFloat
				res.toString
			}

			case str if str.contains("-") => {
				val (left, right) = split_and_eval(str, "-")
				val res = left.toFloat - right.toFloat
				res.toString
			}
			case str_no_operators_left => { str_no_operators_left }
		}
	}

	def apply(str: String) = eval(str)
}

object Main extends App {
	//println(parser("5*(3*2)"))
	//println(parser("2*(3*4)*(5*(6*7))"))
	//println(parser("2*(3*4)*(5*(6*7))*(2*6*7*1*(6*7*(8*(9)*4*2)*1)*1)*7"))
	println(parser("1+2*3"))
	//println(parser("1+10*2/2"))
	//println(parser(""))
	//println(parser(""))
	//println(res = parser("1-1"))
	//println(res = parser("1*1"))
	//println(res = parser("1/1"))
}