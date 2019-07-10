#!/run/current-system/sw/bin/wish

set price 1
set limit 30
set init1 5
set init2 5 

proc doit { } {
    global limit price init1 init2

    set rez [exec "./a.out" $price $limit $init1 $init2]
    .lans.l1 configure -text "распределение: $rez"
}

frame .f9 

labelframe .f9.llim -text "пропускная\nспособность" 
listbox .f9.llim.lb -yscrollcommand {.f9.llim.s set} -height 5 -width 5
for {set i 10} { $i < 101 } { incr i 5} {.f9.llim.lb insert end $i} 
scrollbar .f9.llim.s -orient vertical -command {.f9.llim.lb yview}
bind .f9.llim.lb <<ListboxSelect>> { setLimit [%W curselection] }

scale .f9.t1 -label цена -variable price -orient horizontal -from 0 -to 20  -length 100 -showvalue 1 
scale .f9.t2 -label стартХ -variable init1 -orient horizontal -from 0 -to [expr "$limit/2"] -length 100 -showvalue 1 
scale .f9.t3 -label стартУ -variable init2 -orient horizontal -from 0 -to [expr "$limit/2"] -length 100 -showvalue 1 

proc setLimit { num } {
  global limit 

  set limit [.f9.llim.lb get $num]
  .f9.t2 configure -to [expr "$limit / 2 - 1"]
  .f9.t3 configure -to [expr "$limit / 2 - 1"]
}


frame   .f9.f0 
button  .f9.f0.b1 -command doit  -text "посчитать"  -width 10
button  .f9.f0.b2 -command reset -text "в исходное" -width 10
button  .f9.f0.b3 -command exit  -text "выйти"  -width 10

proc reset {} {
  global limit init1 init2 price

  set price 1
  set limit 30
  set init1 5
  set init2 5 
  .f9.t2 configure -to 14 
  .f9.t3 configure -to 14 
}

labelframe .lans -text ответ 
label .lans.l1 -text "нет результатов!" -width 25

image create photo diag -file "diagram.png" 
label .p -image diag

pack .p -padx 20 -pady 30 
pack .f9
pack .f9.llim -side left -padx 25 
pack .f9.llim.lb -side left -padx 1 -pady 15 -expand yes 
pack .f9.llim.s -side left -padx 5 -pady 15 -fill y
pack .f9.t1 .f9.t2 .f9.t3 -side left -padx 20
pack .f9.f0 -side left -padx 10 
pack .f9.f0.b1 .f9.f0.b2 .f9.f0.b3 -side top 
pack .lans -padx 100 -pady 20 -fill x  
pack .lans.l1 -padx 10 -pady 10 
