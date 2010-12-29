// Abstract incremental parser with simple backtracking.
// The scala.util.parsing._ parsers are more sophisticated in their support
// for lookahead, etc., but lack the ability to incrementally consume input,
// which I find very useful for implementing networking protocols.
// 
// If it is later deemed necessary to add "optimistic" backtracking, 
// Failed should be given a "remaining input" parameter and there should be
// a "try" function that maps Partial(k) to Partial(inp => k(inp) match
// {case Failed(e,_)=>Failed(e,inp)})), with all unmentioned cases of the
// form case x => x.
// 
// Also worthwhile would be a proper EOF token or flag.  The current "null is 
// EOF" strategy makes the "eof" parser a bit dodgy. In particular, 
// Done(_,_).feedEOF does not do the right thing: There's no way to describe
// "some stuff then EOF" in the Done constructor.
sealed abstract class Parser[I,O] {
    import Parser._
    import ParserMonad._
    import ParserAlternative._
    
    def filter(p: O=>Boolean): Parser[I,O] = withFilter(p)
    def withFilter(p: O=>Boolean): Parser[I,O] = 
        for (x <- this)
        if (p(x)) unit(x) else none
    
    def map    [O2](f: O=>O2):           Parser[I,O2]   = fmap(f, this)
    def foreach[O2](f: O=>Parser[I,O2]): Parser[I,O2]   = bind(this,f)
    def flatMap[O2](f: O=>Parser[I,O2]): Parser[I,O2]   = bind(this,f)

    def <+>(other: Parser[I,O]): Parser[I,O]        = orElse(this, other)
    def <@>(expecting: String): Parser[I,O]         = this match {
        case Failed(_,near) => Failed(scala.collection.Set(expecting), near)
        case Partial(k)     => Partial(k(_) <@> expecting)
        case _              => this
    }
    
    def followedBy(other: Parser[I,_]): Parser[I,O] = 
        for (x <- this;
             _ <- other)
        yield x

    def notFollowedBy(other: Parser[I,_]): Parser[I,O] = followedBy(not(other))
    
    def feed(inp: Seq[I]): Parser[I,O] = this match {
        case Done(x,rest)
            if (rest != null
            &&   inp != null)   => Done(x, rest ++ inp)
        case Partial(k)         => k(inp)
        case _                  => this
    }
    
    def feedEOF: Parser[I,O] = feed(null)
    
    def tryFeed(inp: Seq[I]): Option[Parser[I,O]] =
        feed(inp) match {
            case p : Failed[_,_]    => None
            case p                  => Some(p)
        }
    
    def tryFeedEOF: Option[Parser[I,O]] = 
        feedEOF match {
            case p : Failed[_,_]    => None
            case p                  => Some(p)
        }
    
    def isDone: Boolean = this match {
        case Done(_,_)          => true
        case _                  => false
    }
    
    def check: Option[(O,Seq[I])] = this match {
        case Done(x,rest)       => Some(x, rest)
        case Failed(e, near)    =>
            throw new Exception("Parse error: expecting {"
                                + e.mkString(", ") + "}, found " 
                                + near.getOrElse("something else"))
        case _                  => None
    }
}

case class Done[I,O](result: O, remainder: Seq[I]) extends Parser[I,O]
case class Failed[I,O](expecting: scala.collection.Set[String], near: Option[I]) extends Parser[I,O]
case class Partial[I,O](continuation: Seq[I] => Parser[I,O]) extends Parser[I,O]

object ParserMonad {
    def unit[I,T](x: T): Parser[I,T] = Done(x, Seq())
    def fmap[I,A,B](f: A=>B, x: Parser[I,A]):Parser[I,B] = 
        x match {
            case Done(y, rest)  => Done(f(y),rest)
            case Failed(e,near) => Failed(e,near)
            case Partial(k)     => Partial(inp => fmap(f, k(inp)))
        }
    def bind[I,A,B](x: Parser[I,A], f: A => Parser[I,B]): Parser[I,B] = 
        x match {
            case Done(y, rest)  => f(y).feed(rest)
            case Failed(e,near) => Failed(e,near)
            case Partial(k)     => Partial {inp => bind(k(inp), f)}
        }
}

object ParserAlternative {
    def none[I,O]: Parser[I,O] = Failed(scala.collection.Set(), None)
    
    def orElse[I,O](x: Parser[I,O], y: Parser[I,O]): Parser[I,O] = x match {
        case Done(result, rest) => Done(result, rest)
        case Failed(_,_)        => y
        case Partial(k)         => Partial {inp => orElse(k(inp), y.feed(inp))}
    }
}

// various primitive parsers and parser combinators
object Parser {
    import ParserMonad._
    
    def unexpected[I,O](tok: I): Parser[I,O] = 
        Failed(scala.collection.Set(), Some(tok))
    
    // This one is a bit "magical"... AKA "hackish".  EOF is signaled by
    // the parser being "fed" a null Seq.
    def eof[I]: Parser[I,Unit] = Partial {
        case null       => Done((), Seq())
        case Seq()      => eof
        case inp        => unexpected(inp.head) <@> "EOF"
    }
    
    def anyToken[I]: Parser[I,I] = Partial {
        case null       => Failed(scala.collection.Set("primitive token"), None)
        case Seq()      => anyToken
        case inp        => Done(inp.head, inp.tail)
    }
    
    def take[I](n: Int): Parser[I,Seq[I]] = {
        if (n <= 0) unit(Seq())
        else Partial {
            case null                       => Done(Seq(),Seq())
            case inp if (inp.length >= n)   => val (x,rest) = inp splitAt n; Done(x,rest)
            case inp                        => for (rest <- take(n - inp.length)) yield (inp ++ rest)
        }
    }
    
    def reset[I,O](p: Parser[I,O], inp: Seq[I]): Parser[I,O] = p match {
        case Done(x,_)  => Done(x,inp)
        case _          => p
    }
    
    def not[I,O](p: Parser[I,O]): Parser[I,Unit] = p match {
        case Done(_,rest) => Failed(scala.collection.Set(), rest.headOption)
        case Failed(_,_)  => Done((), Seq())
        case Partial(k)   => Partial {inp => reset(not(k(inp)), inp) }
    }
    
    def token[I](tok: I): Parser[I,I] = satisfy(_ == tok)

    def satisfy[I](p: I=>Boolean): Parser[I,I] = 
        for (x <- anyToken[I] if p(x))
        yield x
    
    def option[I,O](default: O, parser: Parser[I,O]): Parser[I,O] = 
        parser <+> unit(default)
    
    def optional[I,O](parser: Parser[I,O]): Parser[I,Option[O]] = 
        (parser.map (Some(_) : Option[O]) <+> unit(None : Option[O]))
    
    def many[I,O](p: Parser[I,O]): Parser[I,scala.List[O]] = 
        option(Nil, many1(p))
    
    def many1[I,O](p: Parser[I,O]): Parser[I,scala.List[O]] = 
        for (x <- p; xs <- many(p))
        yield (x::xs)
    
    def digit: Parser[Char, Char] = satisfy[Char](_.isDigit) <@> "digit"
    def int: Parser[Char, Int] = 
        many1(digit).map {cs => new String(cs.toArray).toInt} <@> "int"
    
    def char(c: Char): Parser[Char, Char] = token(c) <@> c.toString
    def comma: Parser[Char, Char] = token(',') <@> "comma"
    def lf: Parser[Char, Char] = token('\n') <@> "line feed"
}