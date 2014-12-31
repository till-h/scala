package org.tillh.utils

object ParserRegex {
    val re_innermost_bracket = """(\([^\)\(]+\))""".r
    val float                = """(((?<![0-9])-|^-)?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)""" /** ?<! is a negative lookbehind */
    val re_exp               = (float + """(\^)"""   + float).r
    val re_mul_div           = (float + """(\*|/)""" + float).r
    val re_add_sub           = (float + """(\+|-)""" + float).r
}