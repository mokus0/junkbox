module BOM where

import Control.Applicative
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Logic
import Data.Function
import Data.List
import Data.Monoid
import Data.Ord
import qualified Data.Map as M

bom =
    [ (1, pcb)
    , (1, transceiver)
    
    , (1, crystal)
    , (2, crystalLoadCapacitor)
    
    , (1, inputCapacitor)
    , (6, decouplingCapacitor)
    , (1, emiFilterBead)
    
    , (1, balun)
    , (1, antennaConnector)
    , (1, antenna)
    ]

pcb = 
    [ Part 
        { supplier      = oshPark
        , partNo        = "at86rf breakout"
        , minimumQty    = 3
        , increment     = 3
        , price         = 1.35
        }
    ]

transceiver = basicPart mouser "AT86RF233-ZU"
    [ (1,    4.73)
    , (10,   3.95)
    , (25,   2.97)
    , (100,  2.68)
    , (1000, 2.64)
    ]

balun = basicPart mouser "2450BM15A0015E"
    [ (1,    1.43)
    , (10,   0.843)
    , (100,  0.692)
    , (500,  0.678)
    , (1000, 0.481)
    ]

decouplingCapacitor = basicPart mouser "VJ0603Y104JXJPW1BC"
    [ (1,    0.06)
    , (50,   0.024)
    , (100,  0.019)
    , (500,  0.017)
    , (1000, 0.015)
    ]

crystal = basicPart mouser "TSX-3225 16.0000MF09Z-AC3"
    [ (1,   0.60)
    , (10,  0.50)
    , (100, 0.45)
    ]

-- note; will need to experiment to determine correct load cap size
-- (won't be able to determine total parasitic capacitance till i
-- get the board, but datasheet says transceiver xtal pins are 3 pF)
crystalLoadCapacitor = basicPart mouser "VJ0402A100JXAAC"
    [ (1,    0.05)
    , (50,   0.048)
    , (100,  0.04)
    , (500,  0.03)
    ]

antennaConnector = basicPart mouser "A-1JB"
    [ (1,    0.62)
    , (10,   0.57)
    , (25,   0.43)
    , (100,  0.39)
    , (500,  0.36)
    , (1000, 0.34)
    ]

inputCapacitor = basicPart mouser "AVE476M06C12T-F"
    [ (1,   0.10)
    , (50,  0.078)
    , (100, 0.075)
    , (500, 0.061)
    ]

emiFilterBead = basicPart mouser "436-0102-RC"
    [ (1,      0.05)
    , (100,    0.047)
    , (500,    0.039)
    , (1000,   0.038)
    , (4000,   0.03)
    , (8000,   0.029)
    , (20000,  0.026)
    , (40000,  0.025)
    , (100000, 0.024)
    ]

antenna = basicPart mouser "66089-2430"
    [ (1,    3.29)
    , (25,   3.25)
    , (100,  3.24)
    , (500,  3.21)
    , (1000, 3.05)
    , (2000, 3.03)
    ]

---------------------------------------

data Supplier = Supplier
    { supplierName  :: String
    , shipping      :: Double -- TODO: [(Integer, Part)] -> Double
    } deriving (Eq, Ord, Read, Show)

mouser  = Supplier "Mouser" 4.99
oshPark = Supplier "OSH Park" 0
digikey = Supplier "Digikey" 5.47
newark  = Supplier "Newark" 8.50

data Part = Part
    { supplier      :: Supplier
    , partNo        :: String
    , minimumQty    :: Integer
    , increment     :: Integer
    , price         :: Double
    } deriving (Eq, Ord, Read, Show)

basicPart supp num breaks =
    [ Part supp num moq 1 p
    | (moq, p) <- breaks
    ]

---------------------------------------

unitCost withShipping bom qty = cost / fromIntegral qty
    where 
        cost    | withShipping  = pCost + sCost
                | otherwise     = pCost
        
        (_, pCost, sCost) = selectBOM bom qty

selectParts  bom = (\(a,_,_) -> a    ) . selectBOM bom
orderCost    bom = (\(_,b,c) -> b + c) . selectBOM bom
partsCost    bom = (\(_,b,_) -> b    ) . selectBOM bom
shippingCost bom = (\(_,_,c) ->     c) . selectBOM bom

selectBOM parts qty = (bom, sum partCosts, shippingCost)
    where
        totalCost ((_, x), Sum y) = sum x + y
        ((bom, partCosts), Sum shippingCost) = minimumBy (comparing totalCost) $
            map (\(a,b) -> (unzip a, b)) $ 
                observeAll $
                    (\x -> evalRWST x () M.empty) $
                        flip mapM parts $ \(count, part) -> do
                            selectPart part (count * qty)

selectPart parts qty = do
    let suppliers = nub (map supplier parts)
    selected <- map supplierName <$> filterM selectSupplier suppliers
    let selectedParts   = filter (flip elem selected . supplierName . supplier) parts
    if null selectedParts
        then empty
        else pure (selectPart' selectedParts qty)

selectPart' parts qty = ((actualQty part, part), extendedPrice part)
    where
        part                = minimumBy cmpParts parts
        
        extras        part  = max 0 (qty - minimumQty part)
        increments    part  = ceiling (fromIntegral (extras part) / fromIntegral (increment part))
        actualQty     part  = minimumQty part + increments part * increment part
        extendedPrice part  = price part * fromIntegral (actualQty part)
        
        -- minimize price, break ties by maximizing qty
        cmpParts = mconcat
            [ comparing extendedPrice
            , flip (comparing actualQty)
            ]

-- nondeterministically accept/reject each supplier, 
-- remembering the choice and (if accepting) tallying 
-- the shipping cost
selectSupplier s = do
    mbPrev <- gets (M.lookup (supplierName s))
    case mbPrev of
        Just prev   -> return prev
        Nothing     -> do
            accept <- pure True <|> pure False
            modify (M.insert (supplierName s) accept)
            when accept (tell (Sum (shipping s)))
            return accept
