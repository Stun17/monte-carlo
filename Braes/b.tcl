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

labelframe .llim -text "пропускная способность" 
listbox .llim.lb -yscrollcommand {.llim.s set} -height 5 
for {set i 10} { $i < 101 } { incr i 5} {.llim.lb insert end $i} 
scrollbar .llim.s -orient vertical -command {.llim.lb yview}
bind .llim.lb <<ListboxSelect>> { setLimit [%W curselection] }

proc setLimit { num } {
  global limit 

  set limit [.llim.lb get $num]
}

scale .t1 -label цена -variable price -orient horizontal -from 0 -to 20  -length 100 -showvalue 1 
scale .t2 -label старт1 -variable init1 -orient horizontal -from 0 -to 100 -length 100 -showvalue 1 
scale .t3 -label старт2 -variable init2 -orient horizontal -from 0 -to 100 -length 100 -showvalue 1 

frame   .f0 
button  .f0.b1 -command doit  -text "посчитать"  -width 10
button  .f0.b2 -command reset -text "в исходное" -width 10
button  .f0.b3 -command exit  -text "выйти"  -width 10

proc reset {} {
  global limit init1 init2 price

  set price 1
  set limit 30
  set init1 5
  set init2 5 
}

labelframe .lans -text ответ 
label .lans.l1 -text "нет результатов!" -width 25

image create photo diag -file "diagram.png" 
label .p -image diag

pack .p -padx 20 -pady 30 
pack .llim -side left -padx 25 
pack .llim.lb -side left -padx 1 -pady 15
pack .llim.s -side left -pady 15 -fill y
pack .t1 .t2 .t3 -side left -padx 20
pack .f0 -side left -padx 10 
pack .f0.b1 .f0.b2 .f0.b3 -side top 
pack .lans -side left -padx 20
pack .lans.l1 -side bottom -padx 10 -pady 10  
