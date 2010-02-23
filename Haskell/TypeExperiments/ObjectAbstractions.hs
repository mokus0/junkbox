-- a rough sketch of some ideas in my mind about a way of dividing
-- object-orientation abstractions more sensibly than most mainstream languages do

-- this is not a workable set of classes as it presently stands.
-- it's just meant to illustrate.
module TypeExperiments.ObjectAbstractions where

-- An intensional marker on record types intended to represent interfaces to objects
class Interface i 

class Interface i => Implements obj i where
    implementation :: obj -> i

-- inheritance at the interface level only
class (Interface i, Interface j) => Extends i j where
    upcast :: i -> j

-- the "diamond inheritance problem" for interfaces arises from the
-- existential quantification here over i:
instance (Extends i j, Implements obj i) => Implements obj j where
    implementation = upcast . implementation

