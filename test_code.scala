object TESTCLASS extends App {

val re_multiplication = """([0-9\.]+)\*([0-9\.]+)""".r
//var res = "1*2*3*4"
var res = "1*2*3*4*5*6"
for ( multi <- re_multiplication.findAllMatchIn(res) ) {
	val left = multi.toString.split("""\*""", 2)(0).toFloat
	val right = multi.toString.split("""\*""", 2)(1).toFloat
	val multi_res = (left * right).toString
	println("Multiplication", multi.toString, left, right, multi_res)
	res = res.replace(multi.toString, multi_res)
}
println(res)

}