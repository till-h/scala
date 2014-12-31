package org.tillh.utils

object ParserRegex {
    /*  Uses positive lookbehind and lookahead (the outermost pairs of
        brackets with the question marks) to make sure brackets
        surround the expression
     */
    val re_innermost_bracket = """(\()([^\)\(]+)(\))""".r
    val re_innermost_exp     = """(exp\()([^\)\(]+)(\))""".r
    val re_innermost_ln      = """(ln\()([^\)\(]+)(\))""".r  // natural logarithm
    val re_innermost_log     = """(log\()([^\)\(]+)(\))""".r // base-10 logarithm
    val re_innermost_sin     = """(sin\()([^\)\(]+)(\))""".r
    val re_innermost_cos     = """(cos\()([^\)\(]+)(\))""".r
    val re_innermost_tan     = """(tan\()([^\)\(]+)(\))""".r
    /*  ?<! is a negative lookbehind
     */
    val float                = """(((?<![0-9])-|^-)?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)"""
    val re_exp               = (float + """(\^)"""   + float).r
    val re_mul_div           = (float + """(\*|/)""" + float).r
    val re_add_sub           = (float + """(\+|-)""" + float).r
}