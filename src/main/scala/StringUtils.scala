package org.tillh.utils

/**
Following the example in http://alvinalexander.com/scala/scala-2.10-implicit-class-example,
add a matchesRegex method to the builtin String class.
We need this below.
*/
object StringUtils {

  implicit class StringAdditions(val s: String) {
    def matchesRegex(re: scala.util.matching.Regex):Boolean = {
    	if (re.findFirstIn(s) != None) { true }
    	else { false }
    }

    def replaceFirstOccurrence(original: String, substitute: String):String = {
        s.substring(0, s.indexOf(original)) + substitute + s.substring(s.indexOf(original) + original.length, s.length)
    }
  }
 
}