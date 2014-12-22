package arithmeticparser

import org.tillh.utils.StringUtils._

class parser (verbose: Boolean = false) {

	// constructor
	private var iteration            = 0
	private val re_innermost_bracket = ParserRegex.re_innermost_bracket
	private val re_mul_div			 = ParserRegex.re_mul_div
	private val re_add_sub			 = ParserRegex.re_add_sub
	private val float                = ParserRegex.float

	def apply(str: String) = { iteration = 0; eval(str) }

	private def eval(str: String): String = {
		
		iteration = iteration + 1
		if (verbose) println(iteration.toString ++ "\t" ++ str)

		str match {
			// match in order of operation priority

			case str if (str.contains("(") || str.contains(")")) => {
				// use regex to match all atomic (...) occurences and replace these by eval(...) in each case.
				val res = re_innermost_bracket.replaceAllIn(str, bracket => eval(bracket.toString.replaceAll("[()]", "")))
				eval(res)
			}
			
			// Should only get here if no brackets are left inside current substring.
			// Following the convention, "comb" through the expression from left to right,
			// evaluating first the leftmost * OR / operation, then combing through
			// the result until no * or / is left, THEN doing the same for + OR -.
			// (I.e., prioritise * and / equally, above + and -, which are in turn
			// equally prioritised amongst themselves.)
			case str if str matchesRegex re_mul_div => {
				val mul_div = re_mul_div.findFirstMatchIn(str).get
				val left = mul_div.group(1).toFloat
				val right = mul_div.group(5).toFloat
				var mul_div_res = ""
				mul_div.group(4) match {
					case "*" => mul_div_res = (left * right).toString
					case "/" => mul_div_res = (left / right).toString
				}
				if (verbose) println("\t=" + mul_div_res)
				val res = str.replaceFirstOccurrence(mul_div.toString, mul_div_res)
				eval(res)
			}

			case str if str matchesRegex re_add_sub => {
				val add_sub = re_add_sub.findFirstMatchIn(str).get
				val left = add_sub.group(1).toFloat
				val right = add_sub.group(5).toFloat
				var add_sub_res = ""
				add_sub.group(4) match {
					case "+" => add_sub_res = (left + right).toString
					case "-" => add_sub_res = (left - right).toString
				}
				if (verbose) println("\t=" + add_sub_res)
				val res = str.replaceFirstOccurrence(add_sub.toString, add_sub_res)
				eval(res)
			}

			// return the string if nothing remains to be evaluated
			case single_float => { single_float }
		}
	}

}