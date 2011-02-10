import scala.tools.nsc.Interpreter

abstract class Thing;
abstract class ThingListener;


class Thing1 extends Thing;
abstract class Thing1Listener extends ThingListener {
    def thingHappened(x: Thing1):Unit
}

class Thing2 extends Thing;
abstract class Thing2Listener extends ThingListener {
    def thingHappened(x: Thing2):Unit
}

// Imagine there's about 500 Thing's in a complicated ad-hoc heirarchy and 
// exactly one ThingListener for every Thing, named predictably but not 
// related in any useful way within the type system.
// 'thingHappened' is a callback you want to implement, but for some insane 
// reason beyond your control, it's not defined in "ThingListener" AT ALL,
// even though it is THE SOLE REASON FOR THE EXISTENCE OF THAT CLASS.
//
// I found myself in exactly that situation, and wound up writing something like this:

trait ThingListenerFactory[T <: Thing] {
    def mkListener(onThingHappened: T=>Unit): ThingListener
}

object MkThingListener {
    private val factories = scala.collection.mutable.Map[Class[_], ThingListenerFactory[_]]()
    
    private def getFactory[T <: Thing](implicit manif: Manifest[T]): ThingListenerFactory[T] = {
        val factory = factories.getOrElseUpdate(manif.erasure, mkFactory[T])
        
        factory.asInstanceOf[ThingListenerFactory[T]]
    }
    
    private lazy val i = new Interpreter
    private def mkFactory[T <: Thing](implicit manif: Manifest[T]): ThingListenerFactory[T] = {
        val src = """
            new ThingListenerFactory[%T%] {
                def mkListener(onThingHappened: %T%=>Unit): ThingListener =
                    new %T%Listener {
                        def thingHappened(x: %T%) = onThingHappened(x)
                    }
            }
        """.replace("%T%", manif.toString)
        
        println(src)
        i.evalExpr[ThingListenerFactory[T]](src)
    }
    
    def apply[T <: Thing](onThing: T=>Unit)(implicit manif: Manifest[T]): ThingListener = {
        getFactory[T].mkListener(onThing)
    }
}
