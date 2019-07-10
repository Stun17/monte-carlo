#!/run/current-system/sw/bin/wish

set limits [list 10 20 30 40 50 60 70 80 90]
set price 1 ; set limit 30 ; set init1 5 ; set init2 5 

proc calculateit { } {
    global limit price init1 init2

    set rez [exec "./a.out" $price $limit $init1 $init2]
    .z.l1 configure -text "распределение: $rez"
}

#-----------------------------------------------------------------------

labelframe .f -text "исходные данные" 

labelframe .f.llim -text "пропускная\nспособность" 

listbox .f.llim.lb -yscrollcommand {.f.llim.sb set} -height 4 -width 2 \
  -listvariable limits -selectmode single
.f.llim.lb activate 2 
.f.llim.lb see 2 
.f.llim.lb selection set 0 
bind .f.llim.lb <<ListboxSelect>> { setLimit [%W curselection] }

scrollbar .f.llim.sb -orient vertical -command {.f.llim.lb yview}

frame .f.fc

scale .f.fc.sc1 -label цена   -variable price -orient horizontal -from -2 -to 22 \
  -length 120 -showvalue 1 
scale .f.fc.sc2 -label стартХ -variable init1 -orient horizontal -from 0 -to [expr "$limit/2"] \
  -length 120 -showvalue 1 
scale .f.fc.sc3 -label стартУ -variable init2 -orient horizontal -from 0 -to [expr "$limit/2"] \
  -length 120 -showvalue 1 

proc setLimit { num } {
  global limit 

  set limit [.f.llim.lb get $num]
  .f.fc.sc2 configure -to [expr "$limit / 2 - 1"]
  .f.fc.sc3 configure -to [expr "$limit / 2 - 1"]
}

frame .f.fb 

button .f.fb.b1 -command calculateit  -text "посчитать"  -width 10
button .f.fb.b2 -command reset -text "в исходное" -width 10

proc reset { } {
  global limit init1 init2 price

  set price 1
  set limit 30
  set init1 5
  set init2 5 
  set n [.f.llim.lb curselection] 
  .f.llim.lb selection clear $n 
  .f.llim.lb activate 2 
  .f.llim.lb see 2 
  .f.llim.lb selection set 2 
  .f.fc.sc2 configure -to 14 
  .f.fc.sc3 configure -to 14 
  .z.l1 configure -text "нет результатов!" 
}

#-----------------------------------------------------------------------

labelframe .z -text ответ 
label .z.l1 -text "нет результатов!" -width 40 

image create photo diag -file "diagram.png" 
label .p -image diag

button .b3 -command exit -text "выйти" -width 15

#-----------------------------------------------------------------------

pack .b3 -side top  -padx 180 -pady 30 -fill none -anchor nw 

pack .f  -side top  -padx 20  -pady 10            -anchor nw
pack .z  -side left -padx 50  -pady 20 -fill x    -anchor nw 
pack .p  -side top  -padx 20  -pady 10 

pack .f.llim -side left -padx 25 -pady 15 
pack .f.llim.lb -side left -padx 1 -pady 15 -expand yes 
pack .f.llim.sb -side left -padx 5 -pady 15 -fill y

pack .f.fc -side left 
pack .f.fc.sc1 .f.fc.sc2 .f.fc.sc3 -side top -padx 20 -pady 5

pack .f.fb -side left -padx 20 
pack .f.fb.b1 .f.fb.b2 -side top 

pack .z.l1 -padx 10 -pady 10 
