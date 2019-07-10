#!/run/current-system/sw/bin/wish

set price 0.1
set limit 30
set ini1 2
set ini2 2

proc doit {} {
    global limit price ini1 ini2
    set rez [exec "./a.out" $price $limit $ini1 $ini2]
    .l1 configure -text $rez
}

entry  .f1 -textvariable price 
entry  .f2 -textvariable limit
entry  .f3 -textvariable ini1
entry  .f4 -textvariable ini2
label  .l1 -text ""
button .b1 -command doit -text "do it now" -width 10
button .b2 -command exit -text "quit" -width 10

pack .f1 .f2 .f3 .f4 .l1 .b1 .b2 -pady 5 
