package arithmeticparser

import org.tillh.utils.StringUtils._

class parser (verbose: Boolean = false){
	// constructor
	private var iteration            = 0
	private val operators            = List("*","/","+","-")
	private val re_innermost_bracket = ParserRegex.re_innermost_bracket
	private val float                = ParserRegex.float
	private val re_mul_div			 = ParserRegex.re_mul_div
	private val re_add_sub			 = ParserRegex.re_add_sub

	private def eval(str: String): String = {
		
		iteration = iteration + 1
		if (verbose) println(iteration.toString ++ "\t" ++ str)

		str match {
			
			// match in order of operation priority
			case str if (str.contains("(") || str.contains(")")) => {
				// use regex to match all atomic (...) occurences and replace these by eval((...)) in each case.
				val res = re_innermost_bracket.replaceAllIn(str, bracket => eval(bracket.toString.replaceAll("[()]", "")))
				eval(res)
			}
			
			// Should only get here if no brackets are left inside current substring.
			// Following convention, "comb" through the expression from left to right,
			// evaluating first the leftmost * OR / operation, then combing through
			// the result until no * or / is left, THEN doing the same for + OR -.
			// The important thing is to prioritise * and / equally, above the equally
			// prioritised + and -.
			case str if str matchesRegex re_mul_div => {
				var res = str
				val mul_div = re_mul_div.findFirstMatchIn(str).get
				val left = mul_div.group(1).toFloat
				val right = mul_div.group(5).toFloat
				var mul_div_res = ""
				mul_div.group(4) match {
					case "*" => mul_div_res = (left * right).toString
					case "/" => mul_div_res = (left / right).toString
				}
				if (verbose) println("\t=" + mul_div_res)
				res = res.replaceFirstOccurrence(mul_div.toString, mul_div_res)
				eval(res)
			}

			case str if str matchesRegex re_add_sub => {
				var res = str
				val add_sub = re_add_sub.findFirstMatchIn(str).get
				val left = add_sub.group(1).toFloat
				val right = add_sub.group(5).toFloat
				var add_sub_res = ""
				add_sub.group(4) match {
					case "+" => add_sub_res = (left + right).toString
					case "-" => add_sub_res = (left - right).toString
				}
				if (verbose) println("\t=" + add_sub_res)
				res = res.replaceFirstOccurrence(add_sub.toString, add_sub_res)
				eval(res)
			}

			case str_no_operators_left => { str_no_operators_left }
		
		}
	}

	def apply(str: String) = { iteration = 0; eval(str) }
}

object Main extends App {
	val parser = new parser
	println(parser("1-2-3-4-5-6"))
	println(parser("1+2*3+4*5"))
	println(parser("1+2+3+4+5"))
	println(parser("1000/10/20/5/4"))
	println(parser("1*2*3*4*5*6"))
	println(parser("10*2+(10-8)*5"))
	println(parser("1+10*2/2"))
	println(parser("1-1"))
	println(parser("1*1"))
	println(parser("1/1"))
	println(parser("2*(3*4)*(5*(6*7))"))
	println(parser("2*(3*4)*(5*(6*7))*(2*6*7*1*(6*7*(8*(9)*4*2)*1)*1)*7"))
}