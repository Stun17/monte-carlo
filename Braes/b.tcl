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

scale .f.fc.sc1 -label цена   -variable price -orient horizontal -from -22 -to 22 \
  -length 120 -showvalue 1 
scale .f.fc.sc2 -label "нач.условия Х" -variable init1 -orient horizontal -from 0 -to [expr "$limit/2"] \
  -length 120 -showvalue 1 
scale .f.fc.sc3 -label "нач.условия Y" -variable init2 -orient horizontal -from 0 -to [expr "$limit/2"] \
  -length 120 -showvalue 1 

proc setLimit { num } {
  global limit 

  set limit [.f.llim.lb get $num]
  .f.fc.sc2 configure -to [expr "$limit / 2 - 1"]
  .f.fc.sc3 configure -to [expr "$limit / 2 - 1"]
}

frame .g

label .g.lb2 -justify left -wraplength 590 -font myfont -text "есть две дороги из А в В - одна через пункт 1 и другая через пункт 2. цена за проезд из А в 1 постоянна и равна 10. такая же цена за проезд из 2 в В. цены за проезд из 1 в В и из А в 2 зависят от текущего количества машин, выбравших то или иное направление и расчитывается по формуле:\n price(1B) = 20 * x / (x + y) ; price(A2) = 20 * y / (x + y), \nгде x - число машин выбравших путь через 1, а у - число машин, выбравших путь через 2\n тут открыли дорогу из 1 в 2 и установили плату (от -22 до +22). как размер платы за новую дорогу повлияет на распределение потока машин?"

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
label .z.l1 -text "нет результатов!" -width 30 

image create photo diag -file "diagram.png" 
label .p -image diag

button .b3 -command exit -text "выйти" -width 15

#-----------------------------------------------------------------------

place .g -x 20 -y 20 ; pack .g.lb2 -padx 20 -pady 10 ; place .p -x 20 -y 250 

pack .b3 -side right -padx 10 -pady 10 -fill none -anchor se 

pack .f -side right -padx 20 -pady 80 -anchor nw
pack .z -side bottom -padx 10 -pady 10 -expand no -anchor se

pack .f.llim -side left -padx 25 -pady 15 
pack .f.llim.lb -side left -padx 0 -pady 15 -expand yes 
pack .f.llim.sb -side left -padx 0 -pady 15 -fill y -expand yes 

pack .f.fc -side left 
pack .f.fc.sc1 .f.fc.sc2 .f.fc.sc3 -side top -padx 20 -pady 5

pack .f.fb -side left -padx 20 
pack .f.fb.b1 .f.fb.b2 -side top 

pack .z.l1 -padx 0 -pady 10 -anchor sw 
