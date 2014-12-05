object parser {
	// constructor
	var iteration = 0
	val operators = List("*","/","+","-")
	val re_innermost_bracket = """(\(([^\)^\(]+)\))""".r

	//def apply(str: String) { eval(str) }
	def apply(str: String) { eval(str) }

	def eval(str: String): String = {
		iteration = iteration + 1
		println(iteration.toString ++ "\t" ++ str)
		str match {
			// match in order of operation priority
			case str if (str.contains("(") || str.contains(")")) => {
				// use regex to match all atomic (...) occurences and replace these by eval((...)) in each case.
				re_innermost_bracket.replaceAllIn(str, bracket => eval(bracket.toString.replaceAll("[()]", "")))
			}
			// should only get here if no brackets are left inside current substring
			case str if str.contains("*") => {
				var left = str.split("\\*")(0)
				var right = str.split("\\*")(1)
				if (operators.exists(left.contains(_))) { left = eval(left)}
				if (operators.exists(right.contains(_))) { right = eval(right)}
				val tmp = left.toFloat * right.toFloat
				eval(tmp.toString)
			}
			case str if str.contains("/") => { ""}
			// should only get here if no priority operations * or / are left inside current substring
			case str if str.contains("+") => { ""}
			case str if str.contains("-") => { ""}
		}
	}
}

object Main extends App {
	println(parser("5*(3*2)+7*(5-9)-((7/6)*2)"))
	//println(parser("1+1"))
	//println(res = parser("1-1"))
	//println(res = parser("1*1"))
	//println(res = parser("1/1"))
}