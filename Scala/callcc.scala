object CallCC {
    def callCC[A,B](func : (((=> A) => B) => A)) : A = func { return _ }
    
    def barf = {
        var f : ((=>String)=>Unit) = null
        
        println (callCC { (cont : (=>String) => Unit) => f = cont; "aoeu" })
        
        f("asdf")   // throws NonLocalReturnException
    }
}