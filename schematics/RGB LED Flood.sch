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
C 41100 42700 1 0 0 Generic_8pin_IC.sym
{
T 41300 44000 5 10 0 1 0 0 1
device=Generic 8 pin IC
T 41300 43800 5 10 1 1 0 0 1
refdes=U1
}
C 42200 42100 1 180 0 Generic_8pin_IC.sym
{
T 42000 40800 5 10 0 1 180 0 1
device=Generic 8 pin IC
T 41500 42300 5 10 1 1 180 0 1
refdes=U2
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
C 41200 44100 1 0 0 capacitor-1.sym
{
T 41400 44800 5 10 0 0 0 0 1
device=CAPACITOR
T 41300 44400 5 10 1 1 0 0 1
refdes=C3
T 41400 45000 5 10 0 0 0 0 1
symversion=0.1
T 41800 44400 5 10 1 1 0 0 1
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
C 46000 43300 1 0 1 capacitor-1.sym
{
T 45800 44000 5 10 0 0 0 6 1
device=CAPACITOR
T 45400 43600 5 10 1 1 0 6 1
refdes=C4
T 45800 44200 5 10 0 0 0 6 1
symversion=0.1
T 45300 43100 5 10 1 1 0 0 1
value=1000p(?)
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
N 40900 43500 40900 45100 4
N 40900 46200 41800 46200 4
N 41800 46200 41800 46400 4
N 40300 43500 41100 43500 4
N 40900 44300 41200 44300 4
N 42100 44300 42400 44300 4
N 42400 43500 42400 47500 4
N 42200 43500 45100 43500 4
N 47500 42500 40900 42500 4
N 40900 42500 40900 42900 4
N 40900 42900 41100 42900 4
N 44900 43000 46400 43000 4
N 44900 43000 44900 43500 4
N 42200 42900 43100 42900 4
N 43100 42900 43100 45600 4
N 42200 43100 43900 43100 4
N 43900 43100 43900 45600 4
N 42200 43300 44700 43300 4
N 44700 43300 44700 45600 4
N 41100 43100 40700 43100 4
N 40700 41900 40700 43100 4
N 40700 41900 41100 41900 4
N 41100 41700 40500 41700 4
N 40500 41700 40500 43300 4
N 40500 43300 41100 43300 4
N 42400 40900 42400 43500 4
N 42400 41300 42200 41300 4
N 42400 41500 42200 41500 4
N 42400 41700 42200 41700 4
N 42400 41900 42200 41900 4
N 42400 40900 40900 40900 4
N 40900 40900 40900 41300 4
N 40900 41300 41100 41300 4
N 41100 41500 40300 41500 4
N 40300 41500 40300 43500 4
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
N 46600 43500 46000 43500 4
N 46200 43500 46200 44900 4
N 40900 44900 46200 44900 4
C 46400 42900 1 0 0 resistor-1.sym
{
T 46700 43300 5 10 0 0 0 0 1
device=RESISTOR
T 46600 43200 5 10 1 1 0 0 1
refdes=R?
}
N 47000 43500 47500 43500 4
N 47500 42500 47500 43500 4
N 47500 43000 47300 43000 4
C 46600 43700 1 270 0 photo-transistor-1.sym
{
T 47100 43900 5 6 0 1 270 0 1
device=PS2501-1
T 46460 43700 5 10 1 1 0 0 1
refdes=Q?
T 46600 43740 5 10 0 1 270 0 1
device=photo-transistor
}
