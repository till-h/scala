import org.tillh.arithmeticparser._

object Main extends App {
    val parser = new parser(verbose = true)
    var input = ""
    do {
        input = readLine("formula> ")
        println(parser(input))
    } while (input != "")
}
