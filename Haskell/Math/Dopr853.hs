{-# LANGUAGE RecordWildCards, NoMonomorphismRestriction, ParallelListComp, 
    ForeignFunctionInterface, ViewPatterns #-}
module Math.Dopr853 where

import Control.Monad.ST
import Data.List
import Data.STRef
import Math.ODEInt hiding (erf, erfc, c_erf, c_erfc, erfTest)

vsub v1 v2 = zipWith (-) v1 v2

errorNorm853 atols rtols h ys newYs delta1 delta2 = abs h * err1 * sqrt (recip (n * deno))
    where
        n = fromIntegral (length ys)
        deno = case err1 + 0.01*err2 of
            x | x <= 0    -> 1
              | otherwise -> x
        
        (sum -> err1, sum -> err2) = unzip
            [ let sk = atol + rtol * max (abs y) (abs newY)
               in ((yerr1/sk)^2, (yerr2/sk)^2)
            | y     <- ys
            | newY  <- newYs
            | yerr1 <- delta1
            | yerr2 <- delta2
            | atol  <- atols ++ repeat 0
            | rtol  <- rtols ++ repeat 0
            ]

-- eighth-order adaptive runge-kutta stepper (with 5th and 3rd order
-- embedded RK error estimates)
stepperDopr853 :: (Floating a, Ord a) => (a -> [a] -> [a] -> [a] -> [a] -> a) -> ST s (Stepper (ST s) a)
stepperDopr853 errorNorm = do
    reject <- newSTRef False
    return (step reject)
    where
        step reject derivs x dydxs ys h = do
            (newX, newYs, delta1, delta2) <- dy derivs dydxs x ys h
            let err = errorNorm h ys newYs delta1 delta2
                newDydxs = derivs newX newYs
            controllerResult <- controller reject err h
            case controllerResult of
                {- step rejected -} 
                Left hNew -> step reject derivs x dydxs ys hNew
                
                {- step accepted -} 
                Right hNext -> finish newDydxs (x,ys) (newX, newYs) h hNext
        
        finish dydxs from to hUsed hnext =
            return (dydxs, hnext, Step {..})
        dense = error "stepperDopr5: dense output not yet implemented"
        
        controller reject err h
            | err <= 1      = do
                {- accept -}
                let scale   | err == 0  = maxscale
                            | otherwise = min maxscale 
                                        . max minscale 
                                        $ safe * (err ** negate alpha) * (err ** beta)
                r <- readSTRef reject
                let hNext | r         = h * min scale 1
                          | otherwise = h * scale
        
                writeSTRef reject False
                return (Right hNext)
            | otherwise     = do
                {- reject -}
                let scale = max (safe * (err ** negate alpha)) minscale
                writeSTRef reject True
                return (Left (h * scale))
            where
                alpha = 0.125 - beta * 0.2
                beta = 0.08
                safe = 0.9
                minscale = 0.333
                maxscale = 6
        
        dy derivs dydxs x ys h 
            | newX == x = fail "stepperDopr5: stepsize underflow"
            | otherwise = return (newX, newYs, delta1, delta2)
            where
                k = transpose 
                    [ dydxs
                    , derivs (x + c2 *h) (vadd ys (vscale h (map (vdot a02) k)))
                    , derivs (x + c3 *h) (vadd ys (vscale h (map (vdot a03) k)))
                    , derivs (x + c4 *h) (vadd ys (vscale h (map (vdot a04) k)))
                    , derivs (x + c5 *h) (vadd ys (vscale h (map (vdot a05) k)))
                    , derivs (x + c6 *h) (vadd ys (vscale h (map (vdot a06) k)))
                    , derivs (x + c7 *h) (vadd ys (vscale h (map (vdot a07) k)))
                    , derivs (x + c8 *h) (vadd ys (vscale h (map (vdot a08) k)))
                    , derivs (x + c9 *h) (vadd ys (vscale h (map (vdot a09) k)))
                    , derivs (x + c10*h) (vadd ys (vscale h (map (vdot a10) k)))
                    , derivs (x + c11*h) (vadd ys (vscale h (map (vdot a11) k)))
                    , derivs newX        (vadd ys (vscale h (map (vdot a12) k)))
                    ]
                newX = x + h
                newYs = vadd ys (vscale h k13)
                k13 = map (vdot b) k
                
                delta1 = vsub k13 (map (vdot bhh) k)
                delta2 = map (vdot er)  k

-- constants
c2  =  0.526001519587677318785587544488e-01
c3  =  0.789002279381515978178381316732e-01
c4  =  0.118350341907227396726757197510e+00
c5  =  0.281649658092772603273242802490e+00
c6  =  0.333333333333333333333333333333e+00
c7  =  0.25e+00
c8  =  0.307692307692307692307692307692e+00
c9  =  0.651282051282051282051282051282e+00
c10 =  0.6e+00
c11 =  0.857142857142857142857142857142e+00
c14 =  0.1e+00
c15 =  0.2e+00
c16 =  0.777777777777777777777777777778e+00

b01 =  5.42937341165687622380535766363e-2
b02 =  0
b03 =  0
b04 =  0
b05 =  0
b06 =  4.45031289275240888144113950566e0
b07 =  1.89151789931450038304281599044e0
b08 = -5.8012039600105847814672114227e0
b09 =  3.1116436695781989440891606237e-1
b10 = -1.52160949662516078556178806805e-1
b11 = 2.01365400804030348374776537501e-1
b12 = 4.47106157277725905176885569043e-2

bhh01 = 0.244094488188976377952755905512e+00
bhh02 = 0
bhh03 = 0
bhh04 = 0
bhh05 = 0
bhh06 = 0
bhh07 = 0
bhh08 = 0
bhh09 = 0.733846688281611857341361741547e+00
bhh10 = 0
bhh11 = 0
bhh12 = 0.220588235294117647058823529412e-01

er01 =  0.1312004499419488073250102996e-01
er02 =  0
er03 =  0
er04 =  0
er05 =  0
er06 = -0.1225156446376204440720569753e+01
er07 = -0.4957589496572501915214079952e+00
er08 =  0.1664377182454986536961530415e+01
er09 = -0.3503288487499736816886487290e+00
er10 =  0.3341791187130174790297318841e+00
er11 =  0.8192320648511571246570742613e-01
er12 = -0.2235530786388629525884427845e-01

a02 = [a0201]
a03 = [a0301, a0302]
a04 = [a0401, a0402, a0403]
a05 = [a0501, a0502, a0503, a0504]
a06 = [a0601, a0602, a0603, a0604, a0605]
a07 = [a0701, a0702, a0703, a0704, a0705, a0706]
a08 = [a0801, a0802, a0803, a0804, a0805, a0806, a0807]
a09 = [a0901, a0902, a0903, a0904, a0905, a0906, a0907, a0908]
a10 = [a1001, a1002, a1003, a1004, a1005, a1006, a1007, a1008, a1009]
a11 = [a1101, a1102, a1103, a1104, a1105, a1106, a1107, a1108, a1109, a1110]
a12 = [a1201, a1202, a1203, a1204, a1205, a1206, a1207, a1208, a1209, a1210, a1211]

a14 = [a1401, a1402, a1403, a1404, a1405, a1406, a1407, a1408, a1409, a1410, a1411, a1412, a1413]
a15 = [a1501, a1502, a1503, a1504, a1505, a1506, a1507, a1508, a1509, a1510, a1511, a1512, a1513, a1514]
a16 = [a1601, a1602, a1603, a1604, a1605, a1606, a1607, a1608, a1609, a1610, a1611, a1612, a1613, a1614, a1615]

b   = [b01, b02, b03, b04, b05, b06, b07, b08, b09, b10, b11, b12]
bhh = [bhh01, bhh02, bhh03, bhh04, bhh05, bhh06, bhh07, bhh08, bhh09, bhh10, bhh11, bhh12]
er  = [er01, er02, er03, er04, er05, er06, er07, er08, er09, er10, er11, er12]

a0201 =  5.26001519587677318785587544488e-2

a0301 =  1.97250569845378994544595329183e-2
a0302 =  5.91751709536136983633785987549e-2

a0401 =  2.95875854768068491816892993775e-2
a0402 =  0
a0403 =  8.87627564304205475450678981324e-2

a0501 =  2.41365134159266685502369798665e-1
a0502 =  0
a0503 = -8.84549479328286085344864962717e-1
a0504 =  9.24834003261792003115737966543e-1

a0601 =  3.7037037037037037037037037037e-2
a0602 =  0
a0603 =  0
a0604 =  1.70828608729473871279604482173e-1
a0605 =  1.25467687566822425016691814123e-1

a0701 =  3.7109375e-2
a0702 =  0
a0703 =  0
a0704 =  1.70252211019544039314978060272e-1
a0705 =  6.02165389804559606850219397283e-2
a0706 = -1.7578125e-2

a0801 =  3.70920001185047927108779319836e-2
a0802 =  0
a0803 =  0
a0804 =  1.70383925712239993810214054705e-1
a0805 =  1.07262030446373284651809199168e-1
a0806 = -1.53194377486244017527936158236e-2
a0807 =  8.27378916381402288758473766002e-3

a0901 =  6.24110958716075717114429577812e-1
a0902 =  0
a0903 =  0
a0904 = -3.36089262944694129406857109825e0
a0905 = -8.68219346841726006818189891453e-1
a0906 =  2.75920996994467083049415600797e1
a0907 =  2.01540675504778934086186788979e1
a0908 = -4.34898841810699588477366255144e1

a1001 =  4.77662536438264365890433908527e-1
a1002 =  0
a1003 =  0
a1004 = -2.48811461997166764192642586468e0
a1005 = -5.90290826836842996371446475743e-1
a1006 =  2.12300514481811942347288949897e1
a1007 =  1.52792336328824235832596922938e1
a1008 = -3.32882109689848629194453265587e1
a1009 = -2.03312017085086261358222928593e-2

a1101 = -9.3714243008598732571704021658e-1
a1102 =  0
a1103 =  0
a1104 =  5.18637242884406370830023853209e0
a1105 =  1.09143734899672957818500254654e0
a1106 = -8.14978701074692612513997267357e0
a1107 = -1.85200656599969598641566180701e1
a1108 =  2.27394870993505042818970056734e1
a1109 =  2.49360555267965238987089396762e0
a1110 = -3.0467644718982195003823669022e0

a1201 =  2.27331014751653820792359768449e0
a1202 =  0
a1203 =  0
a1204 = -1.05344954667372501984066689879e1
a1205 = -2.00087205822486249909675718444e0
a1206 = -1.79589318631187989172765950534e1
a1207 =  2.79488845294199600508499808837e1
a1208 = -2.85899827713502369474065508674e0
a1209 = -8.87285693353062954433549289258e0
a1210 =  1.23605671757943030647266201528e1
a1211 =  6.43392746015763530355970484046e-1

a1401 =  5.61675022830479523392909219681e-2
a1402 =  0
a1403 =  0
a1404 =  0
a1405 =  0
a1406 =  0
a1407 =  2.53500210216624811088794765333e-1
a1408 = -2.46239037470802489917441475441e-1
a1409 = -1.24191423263816360469010140626e-1
a1410 =  1.5329179827876569731206322685e-1
a1411 =  8.20105229563468988491666602057e-3
a1412 =  7.56789766054569976138603589584e-3
a1413 = -8.298e-3

a1501 =  3.18346481635021405060768473261e-2
a1502 =  0
a1503 =  0
a1504 =  0
a1505 =  0
a1506 =  2.83009096723667755288322961402e-2
a1507 =  5.35419883074385676223797384372e-2
a1508 = -5.49237485713909884646569340306e-2
a1509 =  0
a1510 =  0
a1511 = -1.08347328697249322858509316994e-4
a1512 =  3.82571090835658412954920192323e-4
a1513 = -3.40465008687404560802977114492e-4
a1514 =  1.41312443674632500278074618366e-1

a1601 = -4.28896301583791923408573538692e-1
a1602 =  0
a1603 =  0
a1604 =  0
a1605 =  0
a1606 = -4.69762141536116384314449447206e0
a1607 =  7.68342119606259904184240953878e0
a1608 =  4.06898981839711007970213554331e0
a1609 =  3.56727187455281109270669543021e-1
a1610 =  0
a1611 =  0
a1612 =  0
a1613 = -1.39902416515901462129418009734e-3
a1614 =  2.9475147891527723389556272149e0
a1615 = -9.15095847217987001081870187138e0

d401 = -0.84289382761090128651353491142e+01
d406 =  0.56671495351937776962531783590e+00
d407 = -0.30689499459498916912797304727e+01
d408 =  0.23846676565120698287728149680e+01
d409 =  0.21170345824450282767155149946e+01
d410 = -0.87139158377797299206789907490e+00
d411 =  0.22404374302607882758541771650e+01
d412 =  0.63157877876946881815570249290e+00
d413 = -0.88990336451333310820698117400e-01
d414 =  0.18148505520854727256656404962e+02
d415 = -0.91946323924783554000451984436e+01
d416 = -0.44360363875948939664310572000e+01

d501 =  0.10427508642579134603413151009e+02
d506 =  0.24228349177525818288430175319e+03
d507 =  0.16520045171727028198505394887e+03
d508 = -0.37454675472269020279518312152e+03
d509 = -0.22113666853125306036270938578e+02
d510 =  0.77334326684722638389603898808e+01
d511 = -0.30674084731089398182061213626e+02
d512 = -0.93321305264302278729567221706e+01
d513 =  0.15697238121770843886131091075e+02
d514 = -0.31139403219565177677282850411e+02
d515 = -0.93529243588444783865713862664e+01
d516 =  0.35816841486394083752465898540e+02

d601 =  0.19985053242002433820987653617e+02
d606 = -0.38703730874935176555105901742e+03
d607 = -0.18917813819516756882830838328e+03
d608 =  0.52780815920542364900561016686e+03
d609 = -0.11573902539959630126141871134e+02
d610 =  0.68812326946963000169666922661e+01
d611 = -0.10006050966910838403183860980e+01
d612 =  0.77771377980534432092869265740e+00
d613 = -0.27782057523535084065932004339e+01
d614 = -0.60196695231264120758267380846e+02
d615 =  0.84320405506677161018159903784e+02
d616 =  0.11992291136182789328035130030e+02

d701 = -0.25693933462703749003312586129e+02
d706 = -0.15418974869023643374053993627e+03
d707 = -0.23152937917604549567536039109e+03
d708 =  0.35763911791061412378285349910e+03
d709 =  0.93405324183624310003907691704e+02
d710 = -0.37458323136451633156875139351e+02
d711 =  0.10409964950896230045147246184e+03
d712 =  0.29840293426660503123344363579e+02
d713 = -0.43533456590011143754432175058e+02
d714 =  0.96324553959188282948394950600e+02
d715 = -0.39177261675615439165231486172e+02
d716 = -0.14972683625798562581422125276e+03


-- some tests
erf x1 | abs x1 > 6     = signum x1
erf x1 = min 1 . max (-1) . runST $ do
    let h0 = signum (x1 - x0)
        x0 = 0
        ys0 = [0]
        derivs x [y] = [(2 / sqrt pi) * exp (negate (x^2))]
        
        atol = [1e-12]
        rtol = [1e-12]
    stepper <- stepperDopr853 (errorNorm853 atol rtol)
    ((nok, nbad), steps) <- integrate defaultIntegrationSettings stepper h0 (x0,ys0) x1 derivs
    
    let (outX, [outY]) = to (last steps)
    
    return $ if outX == x1
        then outY
        else error ("erf: did not integrate all the way to x: " ++ show (map sparse steps))

-- this erfc is not really satisfactory.  I just can't get
-- the integrator to put out a function with the required dynamic range.
-- there may be some reparameterization that would work - intuitively it
-- seems to me that the problem is that very long stretches have many 
-- infinitesimal steps (the integrator can't leap through these regions 
-- because they defy taylor series expansion).  Those steps are, beyond
-- some point, always smaller than the epsilon for the current value of
-- the integrand, and so the function hits a constant point and stays 
-- there.  Some kind of an exponential or hyperexponential 
-- reparameterization of x may be enough to allow the many steps to have
-- large enough derivatives to make progress through the same regions,
-- even if not putting the function in a form suitable for larger steps.
erfc x | x <= 5 = 1 - erf x
erfc x1 = runST $ do
    let h0 = signum (x1 - x0)
        x0 = 10
        ys0 = [c_erfc x0]
        derivs x [y] = [((-2) / sqrt pi) * exp (negate (x^2))]
        
        atol = [1e-100]
        rtol = [1e-12]
    stepper <- stepperDopr853 (errorNorm853 atol rtol)
    ((nok, nbad), steps) <- integrate defaultIntegrationSettings stepper h0 (x0,ys0) x1 derivs
    
    let (outX, [outY]) = to (last steps)
    
    return $ if outX == x1
        then outY
        else error ("erfc: did not integrate all the way to x: " ++ show (map sparse steps))

foreign import ccall "math.h erf" c_erf :: Double -> Double
foreign import ccall "math.h erfc" c_erfc :: Double -> Double

erfTest x = erf x / c_erf x - 1
