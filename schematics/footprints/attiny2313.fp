# =====================================================================================================
# author: Max Christian Pohle, 2010
# email: gedasymbols@wireme.de
# url: http://www.entwicklerseite.de/
# dist-license: GPL3, http://www.gnu.org/licenses/gpl-3.0.txt
# use-license: unlimited
# 
# *SYNTAX*
# Element[...](...) ===================================================================================
#
# [SFlags]
# symbolic flags like ["", "edge2", "square", "octagon", "onsolder", "showname", "hole"]
#
# [Name, Value, Desc]
# leave blank (filled out automatically by gsch2pcb)
#
# [MX,MY]
# elements source-X, sourceY (source of all relative coordinates)
#
# [TX,TY]
# initial text-position for values Name,Value,Desc
#
# [TDir]
# relative text-direction ["0"->0deg. (horizontal); "1"->90deg., "2"->180deg., "3"->240deg.]
#
# [TScale]
# text-scaling (percentage of default-font), leave 100 if applicable
#
# [TSFlags]
# leave blank (don't know what )
#
# =====================================================================================================
#       SFlags    "Desc" "Name" "Value"  MX      MY     TX      TY     TDir  TScale TSFlags]
Element[""        ""     ""     ""       10      10     25000   45000  3     100   "showname"]
(
  # ===================================================================================================
  # Pin[...]
  #     rX       rY       Thickness  Clearance   Mask      Drill     "Name"          "Number" SFlags]
    Pin[0          5000   5000       10240       1000      2000      ""              "1"       "square"]
    Pin[0         15000   5000       10240       1000      2000      ""              "2"       ""]
    Pin[0         25000   5000       10240       1000      2000      ""              "3"       ""]
    Pin[0         35000   5000       10240       1000      2000      ""              "4"       ""]
    Pin[0         45000   5000       10240       1000      2000      ""              "5"       ""]
    Pin[0         55000   5000       10240       1000      2000      ""              "6"       ""]
    Pin[0         65000   5000       10240       1000      2000      ""              "7"       ""]
    Pin[0         75000   5000       10240       1000      2000      ""              "8"       ""]
    Pin[0         85000   5000       10240       1000      2000      ""              "9"       "showname"]
    Pin[0         95000   5000       10240       1000      2000      ""              "10"      "showname"]
    Pin[40000      5000   5000       10240       1000      2000      ""              "20"      ""]
    Pin[40000     15000   5000       10240       1000      2000      ""              "19"      ""]
    Pin[40000     25000   5000       10240       1000      2000      ""              "18"      ""]
    Pin[40000     35000   5000       10240       1000      2000      ""              "17"      ""]
    Pin[40000     45000   5000       10240       1000      2000      ""              "16"      ""]
    Pin[40000     55000   5000       10240       1000      2000      ""              "15"      ""]
    Pin[40000     65000   5000       10240       1000      2000      ""              "14"      ""]
    Pin[40000     75000   5000       10240       1000      2000      ""              "13"      ""]
    Pin[40000     85000   5000       10240       1000      2000      ""              "12"      ""]
    Pin[40000     95000   5000       10240       1000      2000      ""              "11"      ""]
  # ===================================================================================================
  # ElementLine[...]
  #             rX1         rY1        rX2         rY2       Thickness
    ElementLine[4000        0          36000       0          2000]
    ElementLine[36000       0          36000       100000     2000]
    ElementLine[36000       100000     4000        100000     2000]
    ElementLine[4000        100000     4000        0          2000]


  # ===================================================================================================
  # ElementArc[...]
  # to make it look nicer - draw some arcs around the pins
  # notice: the values are directly copy&pasted from the pins (values are the same for X and Y)
  #
  #            rX       rY            Width       Height    StartAngle       DeltaAngle       Thickness
    ElementArc[20000    0             4000        4000      0                180              1024]

  # ===================================================================================================
  # [pins]
    # left side
    ElementLine[0      5000 4000 5000      1024]
    ElementLine[0     15000 4000 15000     1024]
    ElementLine[0     25000 4000 25000     1024]
    ElementLine[0     35000 4000 35000     1024]
    ElementLine[0     45000 4000 45000     1024]
    ElementLine[0     55000 4000 55000     1024]
    ElementLine[0     65000 4000 65000     1024]
    ElementLine[0     75000 4000 75000     1024]
    ElementLine[0     85000 4000 85000     1024]
    ElementLine[0     95000 4000 95000     1024]
    # right side
    ElementLine[36000      5000 40000 5000      1024]
    ElementLine[36000     15000 40000 15000     1024]
    ElementLine[36000     25000 40000 25000     1024]
    ElementLine[36000     35000 40000 35000     1024]
    ElementLine[36000     45000 40000 45000     1024]
    ElementLine[36000     55000 40000 55000     1024]
    ElementLine[36000     65000 40000 65000     1024]
    ElementLine[36000     75000 40000 75000     1024]
    ElementLine[36000     85000 40000 85000     1024]
    ElementLine[36000     95000 40000 95000     1024]
    # 'first pin'-rect...
    ElementLine[-3150       1850       3150        1850       1020]
    ElementLine[3150        1850       3150        8150       1020]
    ElementLine[3150        8150       -3150       8150       1020]
    ElementLine[-3150       8150       -3150       1850       1020]
  # ===================================================================================================
  # ElementArc[...]
  # to make it look nicer - draw some arcs around the pins
  # notice: the values are directly copy&pasted from the pins (values are the same for X and Y)
  #
  #            rX       rY            Width       Height    StartAngle       DeltaAngle       Thickness
  # ElementArc[0          5000        3000        3000      0                360             540]
    ElementArc[0         15000        3000        3000      0                360             540]
    ElementArc[0         25000        3000        3000      0                360             540]
    ElementArc[0         35000        3000        3000      0                360             540]
    ElementArc[0         45000        3000        3000      0                360             540]
    ElementArc[0         55000        3000        3000      0                360             540]
    ElementArc[0         65000        3000        3000      0                360             540]
    ElementArc[0         75000        3000        3000      0                360             540]
    ElementArc[0         85000        3000        3000      0                360             540]
    ElementArc[0         95000        3000        3000      0                360             540]
  # other side...
    ElementArc[40000      5000        3000        3000      0                360             540]
    ElementArc[40000     15000        3000        3000      0                360             540]
    ElementArc[40000     25000        3000        3000      0                360             540]
    ElementArc[40000     35000        3000        3000      0                360             540]
    ElementArc[40000     45000        3000        3000      0                360             540]
    ElementArc[40000     55000        3000        3000      0                360             540]
    ElementArc[40000     65000        3000        3000      0                360             540]
    ElementArc[40000     75000        3000        3000      0                360             540]
    ElementArc[40000     85000        3000        3000      0                360             540]
    ElementArc[40000     95000        3000        3000      0                360             540]
  # ===================================================================================================

)
# =====================================================================================================
