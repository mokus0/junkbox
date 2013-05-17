v 20110115 2
C 40000 40000 0 0 0 title-B.sym
C 42600 49500 1 90 0 current-1.sym
{
T 41600 50100 5 10 0 0 90 0 1
device=CURRENT_SOURCE
T 42300 50300 5 10 1 1 180 0 1
refdes=I1
T 42300 49700 5 10 1 1 180 0 1
value=600mA
}
C 41500 48300 1 90 1 lm7805-1.sym
{
T 40200 46700 5 10 0 0 270 2 1
device=7805
T 40600 48200 5 10 1 1 0 8 1
refdes=U3
}
C 44200 46900 1 270 1 mosfet-with-diode-1.sym
{
T 44700 47800 5 10 0 0 90 2 1
device=NPN_TRANSISTOR
T 44200 47800 5 10 1 1 180 6 1
refdes=Q3
}
C 43400 47800 1 270 1 mosfet-with-diode-1.sym
{
T 43900 48700 5 10 0 0 90 2 1
device=NPN_TRANSISTOR
T 43400 48700 5 10 1 1 180 6 1
refdes=Q2
}
C 42600 48700 1 270 1 mosfet-with-diode-1.sym
{
T 43100 49600 5 10 0 0 90 2 1
device=NPN_TRANSISTOR
T 42600 49600 5 10 1 1 180 6 1
refdes=Q1
}
C 41200 44700 1 0 0 capacitor-1.sym
{
T 41400 45400 5 10 0 0 0 0 1
device=CAPACITOR
T 41300 45000 5 10 1 1 0 0 1
refdes=C3
T 41400 45600 5 10 0 0 0 0 1
symversion=0.1
T 41800 45000 5 10 1 1 0 0 1
value=0.1u(?)
}
C 42000 46400 1 90 0 capacitor-1.sym
{
T 41300 46600 5 10 0 0 90 0 1
device=CAPACITOR
T 42100 47100 5 10 1 1 180 0 1
refdes=C2
T 41100 46600 5 10 0 0 90 0 1
symversion=0.1
T 41900 46600 5 10 1 1 0 0 1
value=47u
}
C 42000 47700 1 90 0 capacitor-1.sym
{
T 41300 47900 5 10 0 0 90 0 1
device=CAPACITOR
T 42100 48400 5 10 1 1 180 0 1
refdes=C1
T 41100 47900 5 10 0 0 90 0 1
symversion=0.1
T 41800 47900 5 10 1 1 0 0 1
value=100u
}
C 45200 48200 1 0 0 led-3.sym
{
T 46150 48850 5 10 0 0 0 0 1
device=LED
T 45650 48750 5 10 1 1 0 0 1
refdes=Dgrn
}
C 45200 47300 1 0 0 led-3.sym
{
T 46150 47950 5 10 0 0 0 0 1
device=LED
T 45650 47850 5 10 1 1 0 0 1
refdes=Dblu
}
C 45200 49100 1 0 0 led-3.sym
{
T 46150 49750 5 10 0 0 0 0 1
device=LED
T 45650 49650 5 10 1 1 0 0 1
refdes=Dred
}
N 46300 47500 46300 50600 4
N 40900 50600 46300 50600 4
N 42400 49300 42600 49300 4
N 41500 47500 44200 47500 4
N 40900 50600 40900 48300 4
C 43900 49200 1 0 0 resistor-1.sym
{
T 44200 49600 5 10 0 0 0 0 1
device=RESISTOR
T 44100 49500 5 10 1 1 0 0 1
refdes=R4
T 44200 49000 5 10 1 1 0 0 1
value=15
}
N 43900 46500 43900 47800 4
N 43100 46500 43100 48700 4
N 44800 49300 45200 49300 4
N 43900 49300 43600 49300 4
N 44400 48400 45200 48400 4
N 42400 47500 42400 49500 4
N 43400 48400 42400 48400 4
N 42400 50400 42400 50600 4
N 41800 48800 41800 48600 4
C 41000 45100 1 90 0 resistor-1.sym
{
T 40600 45400 5 10 0 0 90 0 1
device=RESISTOR
T 40600 45800 5 10 1 1 180 0 1
refdes=R5
T 40400 45400 5 10 1 1 0 0 1
value=13
}
N 40900 46000 40900 46700 4
N 40900 44100 40900 45100 4
N 40900 46200 41800 46200 4
N 41800 46200 41800 46400 4
N 40900 44900 41200 44900 4
N 42100 44900 42400 44900 4
N 42400 44100 42400 47500 4
N 43100 44200 43100 45600 4
N 43900 44300 43900 45600 4
N 44700 44300 44700 46000 4
N 46100 49300 46300 49300 4
N 46300 48400 46100 48400 4
N 46300 47500 46100 47500 4
N 44700 46500 44700 46900 4
C 43200 45600 1 90 0 resistor-1.sym
{
T 42800 45900 5 10 0 0 90 0 1
device=RESISTOR
T 42900 46400 5 10 1 1 180 0 1
refdes=R1
T 42600 46000 5 10 1 1 0 0 1
value=330
}
C 44000 45600 1 90 0 resistor-1.sym
{
T 43600 45900 5 10 0 0 90 0 1
device=RESISTOR
T 43700 46400 5 10 1 1 180 0 1
refdes=R2
T 43400 46000 5 10 1 1 0 0 1
value=330
}
C 44800 45600 1 90 0 resistor-1.sym
{
T 44400 45900 5 10 0 0 90 0 1
device=RESISTOR
T 44500 46400 5 10 1 1 180 0 1
refdes=R3
T 44200 46000 5 10 1 1 0 0 1
value=330
}
N 41800 47500 41800 47300 4
N 41800 47500 41800 47700 4
N 41800 48800 40900 48800 4
C 46900 43700 1 0 0 attiny2313.sym
{
T 44700 47600 5 10 0 0 0 0 1
device=attiny2313
T 44700 47200 5 10 0 0 0 0 1
footprint=attiny2313
T 47400 47000 5 10 1 1 0 0 1
refdes=AVR?
}
C 53600 44800 1 0 0 SN65176B.sym
{
T 53275 47750 5 10 0 0 0 0 1
device=MAX485
T 53900 46150 5 10 1 1 0 0 1
refdes=U?
T 53275 46950 5 10 0 0 0 0 1
footprint=so8
}
C 46900 48400 1 90 0 ntc-1.sym
{
T 46300 48600 5 10 0 0 90 0 1
device=NTC
T 47000 49000 5 10 1 1 0 0 1
refdes=R?
}
C 51900 48400 1 0 0 resistor-1.sym
{
T 52200 48800 5 10 0 0 0 0 1
device=RESISTOR
T 52100 48700 5 10 1 1 0 0 1
refdes=R?
}
C 53200 49000 1 0 0 photo-transistor-1.sym
{
T 53000 49500 5 6 0 1 0 0 1
device=PS2501-1
T 53460 49100 5 10 1 1 0 0 1
refdes=Q?
T 53160 49000 5 10 0 1 0 0 1
device=photo-transistor
}
