------------------------------------------------------------------------
-- All library modules, along with short descriptions
------------------------------------------------------------------------

-- Note that core modules are not included.

module Everything where

-- Definitions of algebraic structures like monoids and rings
-- (packed in records together with sets, operations, etc.)
import Algebra

-- Properties of functions, such as associativity and commutativity
import Algebra.FunctionProperties

-- Morphisms between algebraic structures
import Algebra.Morphism

-- Some defined operations (multiplication by natural number and
-- exponentiation)
import Algebra.Operations

-- Some derivable properties
import Algebra.Props.AbelianGroup

-- Some derivable properties
import Algebra.Props.BooleanAlgebra

-- Boolean algebra expressions
import Algebra.Props.BooleanAlgebra.Expression

-- Some derivable properties
import Algebra.Props.DistributiveLattice

-- Some derivable properties
import Algebra.Props.Group

-- Some derivable properties
import Algebra.Props.Lattice

-- Some derivable properties
import Algebra.Props.Ring

-- Solver for commutative ring or semiring equalities
import Algebra.RingSolver

-- Commutative semirings with some additional structure ("almost"
-- commutative rings), used by the ring solver
import Algebra.RingSolver.AlmostCommutativeRing

-- Some boring lemmas used by the ring solver
import Algebra.RingSolver.Lemmas

-- Instantiates the ring solver with two copies of the same ring
import Algebra.RingSolver.Simple

-- Some algebraic structures (not packed up with sets, operations,
-- etc.)
import Algebra.Structures

-- Applicative functors
import Category.Applicative

-- Indexed applicative functors
import Category.Applicative.Indexed

-- Functors
import Category.Functor

-- Monads
import Category.Monad

-- A delimited continuation monad
import Category.Monad.Continuation

-- The identity monad
import Category.Monad.Identity

-- Indexed monads
import Category.Monad.Indexed

-- The partiality monad
import Category.Monad.Partiality

-- The state monad
import Category.Monad.State

-- Basic types related to coinduction
import Coinduction

-- AVL trees
import Data.AVL

-- Finite maps with indexed keys and values, based on AVL trees
import Data.AVL.IndexedMap

-- Finite sets, based on AVL trees
import Data.AVL.Sets

-- A binary representation of natural numbers
import Data.Bin

-- Booleans
import Data.Bool

-- A bunch of properties
import Data.Bool.Properties

-- Showing booleans
import Data.Bool.Show

-- Bounded vectors
import Data.BoundedVec

-- Bounded vectors (inefficient, concrete implementation)
import Data.BoundedVec.Inefficient

-- Characters
import Data.Char

-- "Finite" sets indexed on coinductive "natural" numbers
import Data.Cofin

-- Coinductive lists
import Data.Colist

-- Coinductive "natural" numbers
import Data.Conat

-- Containers, based on the work of Abbott and others
import Data.Container

-- Properties related to ◇
import Data.Container.Any

-- Container combinators
import Data.Container.Combinator

-- Contexts, variables, substitutions, etc.
import Data.Context

-- Coinductive vectors
import Data.Covec

-- Lists with fast append
import Data.DifferenceList

-- Natural numbers with fast addition (for use together with
-- DifferenceVec)
import Data.DifferenceNat

-- Vectors with fast append
import Data.DifferenceVec

-- Digits and digit expansions
import Data.Digit

-- Empty type
import Data.Empty

-- Finite sets
import Data.Fin

-- Decision procedures for finite sets and subsets of finite sets
import Data.Fin.Dec

-- Properties related to Fin, and operations making use of these
-- properties (or other properties not available in Data.Fin)
import Data.Fin.Props

-- Subsets of finite sets
import Data.Fin.Subset

-- Some properties about subsets
import Data.Fin.Subset.Props

-- Substitutions
import Data.Fin.Substitution

-- An example of how Data.Fin.Substitution can be used: a definition
-- of substitution for the untyped λ-calculus, along with some lemmas
import Data.Fin.Substitution.Example

-- Substitution lemmas
import Data.Fin.Substitution.Lemmas

-- Application of substitutions to lists, along with various lemmas
import Data.Fin.Substitution.List

-- Directed acyclic multigraphs
import Data.Graph.Acyclic

-- Integers
import Data.Integer

-- Divisibility and coprimality
import Data.Integer.Divisibility

-- Some properties about integers
import Data.Integer.Properties

-- Lists
import Data.List

-- Lists where all elements satisfy a given property
import Data.List.All

-- Properties relating All to various list functions
import Data.List.All.Properties

-- Lists where at least one element satisfies a given property
import Data.List.Any

-- Properties related to bag and set equality
import Data.List.Any.BagAndSetEquality

-- Properties related to list membership
import Data.List.Any.Membership

-- Properties related to Any
import Data.List.Any.Properties

-- A data structure which keeps track of an upper bound on the number
-- of elements /not/ in a given list
import Data.List.Countdown

-- Non-empty lists
import Data.List.NonEmpty

-- Properties of non-empty lists
import Data.List.NonEmpty.Properties

-- List-related properties
import Data.List.Properties

-- Reverse view
import Data.List.Reverse

-- The Maybe type
import Data.Maybe

-- Natural numbers
import Data.Nat

-- Coprimality
import Data.Nat.Coprimality

-- Integer division
import Data.Nat.DivMod

-- Divisibility
import Data.Nat.Divisibility

-- Greatest common divisor
import Data.Nat.GCD

-- Boring lemmas used in Data.Nat.GCD and Data.Nat.Coprimality
import Data.Nat.GCD.Lemmas

-- Definition of and lemmas related to "true infinitely often"
import Data.Nat.InfinitelyOften

-- Least common multiple
import Data.Nat.LCM

-- A bunch of properties about natural number operations
import Data.Nat.Properties

-- Showing natural numbers
import Data.Nat.Show

-- Transitive closures
import Data.Plus

-- Products
import Data.Product

-- N-ary products
import Data.Product.N-ary

-- Rational numbers
import Data.Rational

-- Reflexive closures
import Data.ReflexiveClosure

-- Signs
import Data.Sign

-- Some properties about signs
import Data.Sign.Properties

-- The reflexive transitive closures of McBride, Norell and Jansson
import Data.Star

-- Bounded vectors (inefficient implementation)
import Data.Star.BoundedVec

-- Decorated star-lists
import Data.Star.Decoration

-- Environments (heterogeneous collections)
import Data.Star.Environment

-- Finite sets defined in terms of Data.Star
import Data.Star.Fin

-- Lists defined in terms of Data.Star
import Data.Star.List

-- Natural numbers defined in terms of Data.Star
import Data.Star.Nat

-- Pointers into star-lists
import Data.Star.Pointer

-- Some properties related to Data.Star
import Data.Star.Properties

-- Vectors defined in terms of Data.Star
import Data.Star.Vec

-- Streams
import Data.Stream

-- Strings
import Data.String

-- Sums (disjoint unions)
import Data.Sum

-- The unit type
import Data.Unit

-- Vectors
import Data.Vec

-- Semi-heterogeneous vector equality
import Data.Vec.Equality

-- Code for converting Vec A n → B to and from n-ary functions
import Data.Vec.N-ary

-- Some Vec-related properties
import Data.Vec.Properties

-- W-types
import Data.W

-- Types used (only) when calling out to Haskell via the FFI
import Foreign.Haskell

-- Simple combinators working solely on and with functions
import Function

-- Bijections
import Function.Bijection

-- Function setoids and related constructions
import Function.Equality

-- Equivalence (coinhabitance)
import Function.Equivalence

-- Injections
import Function.Injection

-- Inverses
import Function.Inverse

-- Various basic type isomorphisms
import Function.Inverse.TypeIsomorphisms

-- Left inverses
import Function.LeftInverse

-- Surjections
import Function.Surjection

-- IO
import IO

-- Primitive IO: simple bindings to Haskell types and functions
import IO.Primitive

-- An abstraction of various forms of recursion/induction
import Induction

-- Lexicographic induction
import Induction.Lexicographic

-- Various forms of induction for natural numbers
import Induction.Nat

-- Well-founded induction
import Induction.WellFounded

-- Universe levels
import Level

-- Support for reflection
import Reflection

-- Properties of homogeneous binary relations
import Relation.Binary

-- Some properties imply others
import Relation.Binary.Consequences

-- Convenient syntax for equational reasoning
import Relation.Binary.EqReasoning

-- Many properties which hold for _∼_ also hold for flip _∼_
import Relation.Binary.Flip

-- Heterogeneous equality
import Relation.Binary.HeterogeneousEquality

-- Indexed binary relations
import Relation.Binary.Indexed

-- Induced preorders
import Relation.Binary.InducedPreorders

-- Lexicographic ordering of lists
import Relation.Binary.List.NonStrictLex

-- Pointwise lifting of relations to lists
import Relation.Binary.List.Pointwise

-- Lexicographic ordering of lists
import Relation.Binary.List.StrictLex

-- Conversion of ≤ to <, along with a number of properties
import Relation.Binary.NonStrictToStrict

-- Many properties which hold for _∼_ also hold for _∼_ on f
import Relation.Binary.On

-- Order morphisms
import Relation.Binary.OrderMorphism

-- Convenient syntax for "equational reasoning" using a partial order
import Relation.Binary.PartialOrderReasoning

-- Convenient syntax for "equational reasoning" using a preorder
import Relation.Binary.PreorderReasoning

-- Lexicographic products of binary relations
import Relation.Binary.Product.NonStrictLex

-- Pointwise products of binary relations
import Relation.Binary.Product.Pointwise

-- Lexicographic products of binary relations
import Relation.Binary.Product.StrictLex

-- Propositional (intensional) equality
import Relation.Binary.PropositionalEquality

-- An equality postulate which evaluates
import Relation.Binary.PropositionalEquality.TrustMe

-- Properties satisfied by decidable total orders
import Relation.Binary.Props.DecTotalOrder

-- Properties satisfied by posets
import Relation.Binary.Props.Poset

-- Properties satisfied by preorders
import Relation.Binary.Props.Preorder

-- Properties satisfied by strict partial orders
import Relation.Binary.Props.StrictPartialOrder

-- Properties satisfied by strict partial orders
import Relation.Binary.Props.StrictTotalOrder

-- Properties satisfied by total orders
import Relation.Binary.Props.TotalOrder

-- Helpers intended to ease the development of "tactics" which use
-- proof by reflection
import Relation.Binary.Reflection

-- Pointwise lifting of binary relations to sigma types
import Relation.Binary.Sigma.Pointwise

-- Some simple binary relations
import Relation.Binary.Simple

-- Convenient syntax for "equational reasoning" using a strict partial
-- order
import Relation.Binary.StrictPartialOrderReasoning

-- Conversion of < to ≤, along with a number of properties
import Relation.Binary.StrictToNonStrict

-- Sums of binary relations
import Relation.Binary.Sum

-- Pointwise lifting of relations to vectors
import Relation.Binary.Vec.Pointwise

-- Operations on nullary relations (like negation and decidability)
import Relation.Nullary

-- Operations on and properties of decidable relations
import Relation.Nullary.Decidable

-- Properties related to negation
import Relation.Nullary.Negation

-- Products of nullary relations
import Relation.Nullary.Product

-- Sums of nullary relations
import Relation.Nullary.Sum

-- A universe of proposition functors, along with some properties
import Relation.Nullary.Universe

-- Unary relations
import Relation.Unary

-- Sizes for Agda's sized types
import Size

-- Universes
import Universe
