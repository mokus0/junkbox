class App {
    def getArgs() = args
    private var args : Array[String] = null
    
    def main(args : Array[String]) = {this.args = args; Console.println("main")}
}

object Main extends App {
    Console.println(getArgs)
    
    ()
}