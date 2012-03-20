# some notations
@rnd = float<ieee_32, ne>;
x = rnd(xx);                           # x is a floating-point number
y rnd= x * (1 - x);                    # equivalent to y = rnd(x * rnd(1 - x))
z = x * (1 - x);                       # the value we want to approximate

# the logical property
{ x in [0,1] -> y in [0,0.25] /\ y - z in [-3b-27,3b-27] }

# hints
z -> 0.25 - (x - 0.5) * (x - 0.5);     # x * (1 - x) == 1/4 - (x - 1/2)^2
y $ x;                                 # bound y by splitting the interval on x
y - z $ x;                             # bound y - z by splitting ...
