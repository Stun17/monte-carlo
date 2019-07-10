#!/run/current-system/sw/bin/wish

set price 1
set limit 30
set init1 5
set init2 5 

proc doit { } {
    global limit price init1 init2

    set rez [exec "./a.out" $price $limit $init1 $init2]
    .lans.l1 configure -text "paths $rez"
}

labelframe .llim -text limit
listbox .llim.lb -yscrollcommand {.llim.s set} -height 5 
for {set i 10} { $i < 101 } { incr i 5} {.llim.lb insert end $i} 
scrollbar .llim.s -orient vertical -command {.llim.lb yview}
bind .llim.lb <<ListboxSelect>> { setLimit [%W curselection] }

proc setLimit { num } {
  global limit 

  set limit [.llim.lb get $num]
}

scale .f1 -label price -variable price -orient horizontal -from 0 -to 20  -length 100 -showvalue 1 
scale .f2 -label init1 -variable init1 -orient horizontal -from 0 -to 100 -length 100 -showvalue 1 
scale .f3 -label init2 -variable init2 -orient horizontal -from 0 -to 100 -length 100 -showvalue 1 

button .b1 -command doit  -text "calc"  -width 10
button .b2 -command reset -text "reset" -width 10
button .b3 -command exit  -text "quit"  -width 10

proc reset {} {
  global limit init1 init2 price

  set price 1
  set limit 30
  set init1 5
  set init2 5 
}

labelframe .lans -text ответ 
label .lans.l1 -text "no results!"

pack .llim -side left 
pack .llim.lb -side left -padx 15 -pady 15
pack .llim.s -side left -fill y
pack .f1 .f2 .f3 -side left -padx 20
pack .f3 -side left -padx 10 
pack .b1 .b2 .b3 -side left 
pack .lans -side left -padx 20
pack .lans.l1 -side bottom -padx 10 -pady 10  
