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
    , (1, mcu)
    -- crystal, crystal caps
    , (4, decouplingCapacitor)
    , (1, resetPullUpResistor)
    ]

pcb = 
    [ Part 
        { supplier      = oshPark
        , partNo        = "xmega a4u target board"
        , minimumQty    = 3
        , increment     = 3
        , price         = 2.85
        }
    , Part
        { supplier      = oshPark
        , partNo        = "xmega a4u target board"
        , minimumQty    = 60
        , increment     = 10
        , price         = 1.7091
        }
    ]


mcu = basicPart mouser "ATXMEGA16A4U-MH"
    [ (1,   2.86)
    , (10,  2.36)
    , (25,  1.80)
    , (100, 1.62)
    ] ++ basicPart digikey "ATXMEGA16A4U-MH"
    [ (1,   2.86)
    , (25,  1.7984)
    , (100, 1.5984)
    ] ++ basicPart newark "ATXMEGA32A4U-MH"
    [ (1,   3.44)
    , (10,  2.88)
    , (25,  2.16)
    , (100, 1.95)
    ]

decouplingCapacitor = basicPart mouser "VJ0603Y104JXJPW1BC"
    [ (1,    0.06)
    , (50,   0.024)
    , (100,  0.019)
    , (500,  0.017)
    , (1000, 0.015)
    ]

resetPullUpResistor = basicPart mouser "CRCW040210K0FKED"
    [ (1,    0.08)
    , (10,   0.044)
    , (100,  0.021)
    , (1000, 0.015)
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
