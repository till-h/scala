package org.tillh.arithmeticparser

import org.tillh.utils.StringUtils._
import org.tillh.utils.ParserRegex._
import Math.{pow, exp, log, log10, sin, cos, tan}

class parser (verbose: Boolean = false) {

	// constructor
	private var iteration = 0

	def apply(str: String) = { iteration = 0; eval(str) }

	private def eval(str: String): String = {
		
		iteration = iteration + 1
		if (verbose) println(iteration.toString ++ "\t" ++ str)

		str match {
			// match in order of operation priority
			
			// match exp et al before simple brackets because the simple brackets regex matches the exp et al ones as well!
			// (It is a less specific regex than the exp et al regexs.)

			case str if str matchesRegex re_innermost_exp => {
				// use regex to match all atomic exp(...) occurences and replace these by exp(eval(...)) in each case.				
				val res = re_innermost_exp.replaceAllIn(str, bracket => exp(eval(bracket.toString.replaceAll("[()]", "").replaceAll("exp", "")).toFloat).toString)
				eval(res)
			}

			case str if str matchesRegex re_innermost_ln => {
				// use regex to match all atomic exp(...) occurences and replace these by exp(eval(...)) in each case.				
				val res = re_innermost_ln.replaceAllIn(str, bracket => log(eval(bracket.toString.replaceAll("[()]", "").replaceAll("ln", "")).toFloat).toString)
				eval(res)
			}

			case str if str matchesRegex re_innermost_log => {
				// use regex to match all atomic exp(...) occurences and replace these by exp(eval(...)) in each case.				
				val res = re_innermost_log.replaceAllIn(str, bracket => log10(eval(bracket.toString.replaceAll("[()]", "").replaceAll("log", "")).toFloat).toString)
				eval(res)
			}

			case str if str matchesRegex re_innermost_sin => {
				// use regex to match all atomic exp(...) occurences and replace these by exp(eval(...)) in each case.				
				val res = re_innermost_sin.replaceAllIn(str, bracket => sin(eval(bracket.toString.replaceAll("[()]", "").replaceAll("sin", "")).toFloat).toString)
				eval(res)
			}

			case str if str matchesRegex re_innermost_cos => {
				// use regex to match all atomic exp(...) occurences and replace these by exp(eval(...)) in each case.				
				val res = re_innermost_cos.replaceAllIn(str, bracket => cos(eval(bracket.toString.replaceAll("[()]", "").replaceAll("cos", "")).toFloat).toString)
				eval(res)
			}

			case str if str matchesRegex re_innermost_tan => {
				// use regex to match all atomic exp(...) occurences and replace these by exp(eval(...)) in each case.				
				val res = re_innermost_tan.replaceAllIn(str, bracket => tan(eval(bracket.toString.replaceAll("[()]", "").replaceAll("tan", "")).toFloat).toString)
				eval(res)
			}

			case str if str matchesRegex re_innermost_bracket => {
				// use regex to match all atomic (...) occurences and replace these by eval(...) in each case.				
				val res = re_innermost_bracket.replaceAllIn(str, bracket => eval(bracket.group(2).toString)) // drop the brackets in groups 1,3 in "bracket"
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