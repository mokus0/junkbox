#!runhaskell
{-# OPTIONS -fglasgow-exts #-}
{-
 -	"/Users/mokus/Desktop/Haskell/Agent/World.hs"
 -	(c) 2007 James Cook
 -}

module World where

import Data.Map
import Control.Concurrent.STM

import Agent

type Generation = Integer
type AgentId = Integer

data World env st obs cmd =
	World {
		world ::	TChan env,
		
		agents ::	Map AgentId (Agent st obs cmd),
		freeAgentIds:: [AgentId],
		
		step ::		env
				 -> Map AgentId (Agent st obs cmd)
				 -> Map AgentId [cmd]
				 -> (env, Map AgentId [obs])
	}

mkWorld initState gameRules = do
	env <- newTChanIO
	atomically (writeTChan env initState)
	return World {
		world = env,
		agents = empty,
		freeAgentIds = [1..],
		step = gameRules
	}

nullRule env _ _ = (env, empty)

addAgent oldWorld agent = oldWorld {
		agents = addAgent' (agents oldWorld) agent, 
		freeAgentIds = tail (freeAgentIds oldWorld)
	} where
		addAgent' agents agent = insert newId agent agents 
			where newId = head (freeAgentIds oldWorld)