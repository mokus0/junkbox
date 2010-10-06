module TypeExperiments.IterateeAnalysis where

{-
    Context:
        I don't like the way iteratees are normally presented.  They seem/feel ad-hoc, contrived, or just plain dirty (in the same sense that inline asm or ConfiguratorPropertyFactoryFactory things feel "dirty") - as Conal Elliott said in the below-linked thread, "Assuming my sense of their intended meanings
is on track, they allow lots of well-typed but bogus values.".  So I've been thinking about how to simplify their presentation, and in particular how to say what they _are_ without having to get deep into the weeds of what they _do_ and how they do it.
        
        Along the way, I've read a few things.
        
        This mailing-list thread asked some of the questions I've been thinking, but never really answered all of it:
        http://www.haskell.org/pipermail/haskell-cafe/2010-August/082533.html
        
        In particular, a good "start" to an answer was given by Heinrich Apfelmus:
        http://www.haskell.org/pipermail/haskell-cafe/2010-August/082540.html
        But it misses a critical aspect of the abstraction itself; interleaving of the enumerator's execution with the iteratee's (as Heinrich points out himself in a later message in the thread).
        
        There were also several very good observations about abstraction leaks, and challenges to plug them.  I believe I have done so.
        
        Luke Palmer's article on "Semantic Design" - I read this after I came up with this stuff, but it's pretty much exactly what I feel this work has done.
        http://lukepalmer.wordpress.com/2008/07/18/semantic-design/
        
    Where does this work fit in to the grand scheme of the iteratee "phenomenon"?
        
        Ultimately, I think iteratees were a cool idea, great invention, important discovery, whatever your philosophy of mathematical knowledge classifies it as.  It is a good thing, and a powerful addition to programmers' tool sets.  But they are still pretty rough around the edges, both in terms of implementations and formal understanding of the concept itself.  
        So we've got these cool toys, we know enough about how they work to make ourselves dangerous.  We can take them apart and put them back together again, or put together new toys that are pretty much the same but subtly different, etc.  And that's what a lot of people are doing.  But for the most part, it really doesn't seem to me like we understand what they _are_, in the same way that we have deep and comprehensive understanding of, say, the reader monad.  As a result, we have a lot of leaky abstractions, mostly because we still really don't know/agree about what the abstraction is supposed to be in the first place.
        But, we know what we want them to do, and we know that a thing exists that does that.  We also know that it forms a monad, and we know a lot about monads in general and monad transformers in particular.  We have a lot of monads and transformers that do _parts_ of what we want iteratees to do.
        So, why not try putting all these well-understood parts together to get the same features we get from this little black box we call an iteratee?

    
    Observations:
    
        Iteratees actually do a lot of different things, and every presentation I've seen so far fails to separate them (or even really clearly enumerate or characterize them).  These different things can actually _all_ already be done by existing monads and monad transformers.  Introducing them separately then combining them would make for an easier read, more robust implementations (due to clear semantics arising from the composition of clearly-specified parts), and potentially more flexible implementations - layers could be mixed and matched, with typeclasses to cleanly glue it all together.  After all, not every iteratee _needs_ exceptions or lookahead.  And some may need other things - resumable exceptions, multiple data sources, incremental output, backtracking nondeterminism, etc.
        
        A significant obscurant in many (if not all) presentations to date is continuation-passing style.  Even the most straightforward "algebraic" implementations make prominent use of a "feed me" continuation.  This continuation in particular is central to every presentation of iteratees, and I find it unsightly and distracting.  It literally cuts another prominent aspect of the abstraction in two (specifically, the state-passing of the remainder of the stream).  Encapsulating this one continuation should really be a very high priority.  The Prompt and Program monads do exactly that (from the "MonadPrompt" and "operational" packages, respectively).  The latter even lets you get at it easily when you want to, which turns out to be critical for implementing the pause-resume cycle by which enumerators feed iteratees.
        
        http://www.haskell.org/pipermail/haskell-cafe/2010-August/082596.html
        Conal Elliott had bad vibes about the formulation of the Enumerator concept.  It may be that he was only referring to the presence of the 'a' type variable on the LHS - I agree about that.  The concept of an Enumerator as an Iteratee transformer, though, seems spot on.  With something like 'ProgramT' (which is essentially an optimization of 'm ProgramViewT'), "[[Enumerator]] = forall a. [[Iteratee a]] -> [[Iteratee a]]" is actually a pretty natural (and watertight) semantics.  The most natural thing it would do with the iteratee is 'viewT' and respond;  exactly what we want.  The only abuse that comes to mind immediately is calling the continuation more than once, but in a practical implementation, that abuse can be prevented by careful choice of exports from the iteratee library.

        http://www.haskell.org/pipermail/haskell-cafe/2010-August/082592.html
        Wren ng thornton had some very good observations, drawing a parallel between iteratees/enumerators and unfoldr/destroy fusion, and contrasted this with the build/foldr scheme.  I think I have found a way of looking at this that blurs the distriction quite severely, making both sides feel like they are the one in control.  Essentially, the whole thing taken together is more of a coroutine, pinging and ponging back and forth as needed.
        C. (Cam?) McCann described enumerators as "[sitting] there [turning] the crank".  In a sense it does, but in another sense it doesn't: it just transforms an Iteratee into a new one that knows how to turn its own crank, at least for a while.
        
        Generalizations to applicative functors briefly crossed my mind.  In particular, weakening the dependency on an underlying monad to an underlying applicative functor would be "neat".  After a few moments consideration, I reject this is a futile and pointless objective.  Control flow of the iteratee really does make essential use of the values returned by actions - if for no other reason, because it must inspect values it receives from the enumerators and decide when to ask for more.
        
 -}

{-
    And now, my thoughts and development of the ideas I want to convey.
    
    Iteratees do several things, all sort of haphazardly.  Seems like a prime situation for monad transformers and/or coproducts, both to clarify and make precise the specification and to modularize the implementation.
    -- Getting input
    -- "Pausing" and resuming when input is available
    -- Maintaining state (available but unconsumed input)
    -- Reporting errors, ideally also handling them
    
    Various implementing types exist:
        Oleg's originals (http://okmij.org/ftp/Haskell/Iteratee/):
            Iteratee.hs:
                type ErrMsg = String
                data Stream = EOF (Maybe ErrMsg) | Chunk String
                data Iteratee a
                    = IE_done a Stream
                    | IE_cont (Stream -> Iteratee a) (Maybe ErrMsg)
            
            IterateeM.hs:
                type ErrMsg = SomeException
                data Stream el = EOF (Maybe ErrMsg) | Chunk [el]
                data Iteratee el m a
                    = IE_done a
                    | IE_cont (Maybe ErrMsg) (Stream el -> m (Iteratee el m a, Stream el))
            
            IterateeCPS.hs:
                type ErrMsg = String
                data Stream = EOF (Maybe ErrMsg) | Chunk String
                data IterV a
                    = IE_done a Stream
                    | IE_cont (Stream -> IterV a) (Maybe ErrMsg)
                newtype Iteratee a = Iteratee
                    { unIter :: forall r. (a -> Stream -> IterV r) -> Stream -> IterV r }

            IterateeMCPS.hs:
                type ErrMsg = SomeException
                data Stream el = EOF (Maybe ErrMsg) | Chunk [el]
                newtype Iteratee el m a = 
                  Iteratee{runIter :: forall r. 
                        (a -> Stream el -> m r) -> 
                        ((Stream el -> Iteratee el m a) -> Maybe ErrMsg -> m r) ->
                        m r}
            
            Data.Iteratee:
                data Stream c = EOF (Maybe SomeException) | Chunk c
                newtype Iteratee s m a
                  = Iteratee {runIter :: forall r.
                         (a -> Stream s -> m r)
                         -> ((Stream s -> Iteratee s m a)
                             -> Maybe SomeException
                             -> m r)
                         -> m r}

            Data.Enumerator:
                data Stream a = Chunks [a] | EOF
                data Step a m b
                    = Continue (Stream a -> Iteratee a m b)
                    | Yield b (Stream a)
                    | Error SomeException
                newtype Iteratee a m b = Iteratee {runIteratee :: m (Step a m b)}
            
            John Lato in The Monad Reader, Issue 16:
                data StreamG el = Empty | El el | EOF 
                data IterV el a 
                    = Done a (StreamG el) 
                    | Cont (StreamG el -> IterV el a)
                
                data IterVM el m a
                    = DoneM a (StreamG el) 
                    | ContM (StreamG el -> Iteratee el m a) 
                newtype Iteratee el m a = Iteratee 
                    { runIter :: m (IterVM el m a) }
            
            John Millikin in an email (http://www.haskell.org/pipermail/haskell-cafe/2010-August/082643.html):
                type Iteratee a m b = m (Step a m b)
                data Step a m b 
                    = Continue (a -> Iteratee a m b) (m (Result a b))
                    | Result a b
                data Result a b 
                    = Yield b [a]
                    | Error String

    All these implementations have a few things in common:
        * They are monolithic constructs.  This is not, in itself, a problem, but it's a complex construct, with no simple reference implementation or clear external semantics against which to check that these implementations are "right".
        * They all make use of at least one explicit continuation, the "feed me" continuation.  As mentioned before, this cuts the "State (Stream a)" in half.  This opens the door for iteratees to do some truly weird things, like returning "remaining" data that was never read in the first place.  Not only that, it puts a burden on every well-meaning iteratee to handle its state-passing sanely.  In cases where input is chunked differently than the iteratee would like, this can be complex, subtle and error-prone.  Which is fine if sane primitives are provided and their use is encouraged, but every document about iteratees I've seen so far seems to also advocate EXPLICITLY WRITING THESE CONTINUATIONS BY HAND!  Seriously, nobody should _EVER_ have to do that unless they are implementing an iteratee library.  And they certainly shouldn't even make an appearance in anything with the word "tutorial" appearing in the vicinity.  Though, to be fair, I don't think there _are_ any iteratee tutorials.  Perhaps it's time?
        Aside: quick survey of introductory material, "tutorials" or otherwise (in particular, I'm looking for _any_ that doesn't focus almost exclusively on the "feed me" continuation, but I'm listing all that I find here either way):
            - http://cdsmith.wordpress.com/2010/05/23/iteratees-step-by-step-part-1
            - http://therning.org/magnus/archives/735
                ^ This one at least makes a token effort to make up some primitives and code iteratees constructively from them.
            - http://projects.haskell.org/pipermail/iteratee/2010-August/000040.html

 -}