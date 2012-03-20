val us_denoms = [10000, 2000, 1000, 500, 100, 25, 10, 5, 1];

exception Indivisible (* as long as there is a "1" unit, this should never occur *)
fun change (denoms,    0) = []
  | change ([],        _) = raise Indivisible
  | change ((d :: ds), x) = 
    let
        val q = Int.div(x, d);
        val r = Int.mod(x, d);
    in
        (q, d) :: change(ds, r)
    end;

fun foreach f [] = ()
  | foreach f (x :: xs) = (fn (x,y) => y) (f x, foreach f xs);

local val fmt = Int.fmt StringCvt.DEC;
in fun printChange (  0,     _) = ()
     | printChange (chg, denom) = foreach print [fmt chg, "\t* ", fmt denom, "\n"];
end;

foreach printChange (change (us_denoms, 3783));
