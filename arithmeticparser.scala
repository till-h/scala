object parser {
	// constructor
	private var iteration = 0
	private val operators = List("*","/","+","-")
	private val re_innermost_bracket = """(\([^\)\(]+\))""".r
	private val re_multiplication = """([0-9\.]+)\*([0-9\.]+)""".r
	private val re_division = """([0-9\.]+)/([0-9\.]+)""".r
	private val re_addition = """([0-9\.]+)\+([0-9\.]+)""".r
	private val re_subtraction = """([0-9\.]+)-([0-9\.]+)""".r

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
				var res = str
				for ( multi <- re_multiplication.findAllMatchIn(str) ) {
					val left = multi.toString.split("""\*""", 2)(0).toFloat
					val right = multi.toString.split("""\*""", 2)(1).toFloat
					val multi_res = (left * right).toString
					println("Multiplication: ", multi.toString, left, right, multi_res)
					res = res.replace(multi.toString, multi_res)
				}
				eval(res)
			}
			case str if str.contains("/") => {
				var res = str
				for ( divi <- re_division.findAllMatchIn(str) ) {
					val left = divi.toString.split("""/""", 2)(0).toFloat
					val right = divi.toString.split("""/""", 2)(1).toFloat
					val divi_res = (left / right).toString
					println("Division: ", divi.toString, left, right, divi_res)
					res = res.replace(divi.toString, divi_res)
				}
				eval(res)
			}
			
			// should only get here if no priority operations * or / are left inside current substring
			case str if str.contains("+") => {
				var res = str
				for ( add <- re_addition.findAllMatchIn(str) ) {
					val left = add.toString.split("""\+""", 2)(0).toFloat
					val right = add.toString.split("""\+""", 2)(1).toFloat
					val add_res = (left + right).toString
					println("Addition: ", add.toString, left, right, add_res)
					res = res.replace(add.toString, add_res)
				}
				eval(res)
			}

			case str if str.contains("-") => {
				var res = str
				for ( sub <- re_subtraction.findAllMatchIn(str) ) {
					val left = sub.toString.split("""-""", 2)(0).toFloat
					val right = sub.toString.split("""-""", 2)(1).toFloat
					val sub_res = (left / right).toString
					println("Division: ", sub.toString, left, right, sub_res)
					res = res.replace(sub.toString, sub_res)
				}
				eval(res)
			}
			case str_no_operators_left => { str_no_operators_left }
		
		}
	}

	def apply(str: String) = { iteration = 0; eval(str) }
}

object Main extends App {
	println(parser("5*(3*2)"))
	println()
	println()
	//println(parser("2*(3*4)*(5*(6*7))"))
	//println(parser("2*(3*4)*(5*(6*7))*(2*6*7*1*(6*7*(8*(9)*4*2)*1)*1)*7"))
	println(parser("1+2*3+4*5"))
	println()
	println()
	println(parser("1*2*3*4*5*6"))
	//println(parser("1+10*2/2"))
	//println(parser(""))
	//println(parser(""))
	//println(res = parser("1-1"))
	//println(res = parser("1*1"))
	//println(res = parser("1/1"))
}