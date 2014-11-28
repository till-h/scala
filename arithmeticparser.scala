package org.bbc.arithmeticparser

object parser extends App {
	// constructor
	var expr = ""

	def apply() { expr = _; eval() }

	def eval() { print(expr) }
}

p = new parser("Hello World!")