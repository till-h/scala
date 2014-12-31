package org.tillh.arithmeticparser

import org.tillh.utils.StringUtils._
import org.tillh.utils.ParserRegex._
import Math.pow

class parser (verbose: Boolean = false) {

	// constructor
	private var iteration = 0

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
			
			case str if str matchesRegex re_exp => {
				val exp = re_exp.findFirstMatchIn(str).get
				val base = exp.group(1).toFloat
				val power = exp.group(5).toFloat
				val exp_res = pow(base, power).toString
				val res = str.replaceFirstOccurrence(exp.toString, exp_res)
				eval(res)
			}

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