@rnd = float<ieee_32,ne>;

a1 = 8388676b-24;
a2 = 11184876b-26;
l2 = 12566158b-48;
s1 = 8572288b-23;
s2 = 13833605b-44;

r2 rnd= -n * l2;
r  rnd= r1 + r2;
q  rnd= r * r * (a1 + r * a2);
p  rnd= r1 + (r2 + q);
s  rnd= s1 + s2;
e  rnd= s1 + (s2 + s * p);

R = r1 + r2;
S = s1 + s2;

E  = s1 + (s2 + S * (r1 + (r2 + R * R * (a1 + R * a2))));
Er = S * (1 + R + a1 * R * R + a2 * R * R * R + 0);
E0 = S0 * (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z);

{ Z in [-55b-39,55b-39] /\ S - S0 in [-1b-41,1b-41] /\ R - R0 in [-1b-34,1b-34] /\
  R in [0,0.0217] /\ n in [-10176,10176]
   ->
  e in ? /\ e - E0 in ? }

e - E0 -> (e - E) + (Er - E0);
r1     -> R - r2;
