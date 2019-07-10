#!/run/current-system/sw/bin/wish

set limits [list 10 20 30 40 50 60 70 80 90]
set price 1 ; set limit 30 ; set init1 5 ; set init2 5 

proc calculateit { } {
    global limit price init1 init2

    set rez [exec "./a.out" $price $limit $init1 $init2]
    .f.l1 configure -text "распределение: $rez"
}

#-----------------------------------------------------------------------

labelframe .f -text "исходные данные" 

label .f.ll -text "пропускная\nспособность" 

listbox .f.lb -height 9 -width 3 -listvariable limits -selectmode single
.f.lb activate 2 
.f.lb see 2 
.f.lb selection set 0 
bind .f.lb <<ListboxSelect>> { setLimit [%W curselection] }

scale .f.s1 -label цена   -variable price -orient horizontal -from -22 -to 22 \
  -length 120 -showvalue 1 
scale .f.s2 -label "нач.условия Х" -variable init1 -orient horizontal -from 0 -to [expr "$limit/2"] \
  -length 120 -showvalue 1 
scale .f.s3 -label "нач.условия Y" -variable init2 -orient horizontal -from 0 -to [expr "$limit/2"] \
  -length 120 -showvalue 1 

proc setLimit { num } {
  global limit 

  set limit [.f.lb get $num]
  .f.s2 configure -to [expr "$limit / 2 - 1"]
  .f.s3 configure -to [expr "$limit / 2 - 1"]
}

label .t -justify left -wraplength 590 -font myfont -text "\nесть две дороги из А в В - одна через пункт 1 и другая через пункт 2. цена за проезд из А в 1 постоянна и равна 10. такая же цена за проезд из 2 в В. цены за проезд из 1 в В и из А в 2 зависят от текущего количества машин, выбравших то или иное направление и расчитывается по формуле:\n price(1B) = 20 * x / (x + y) \n price(A2) = 20 * y / (x + y), \nгде x - число машин, выбравших путь через 1\n    у - число машин, выбравших путь через 2\nтут открыли дорогу из 1 в 2 и установили плату (от -22 до +22)\nкак размер платы за новую дорогу повлияет на распределение потока машин?"

frame .f.fb 

button .f.fb.b1 -command calculateit  -text "посчитать"  -width 10
button .f.fb.b2 -command reset -text "в исходное" -width 10

proc reset { } {
  global limit init1 init2 price

  set price 1
  set limit 30
  set init1 5
  set init2 5 
  set n [.f.lb curselection] 
  .f.lb selection clear $n 
  .f.lb activate 2 
  .f.lb see 2 
  .f.lb selection set 2 
  .f.s2 configure -to 14 
  .f.s3 configure -to 14 
  .f.l1 configure -text "нет результатов!" 
}

#-----------------------------------------------------------------------

label .f.l1 -text "нет результатов!" -width 30 -font { -size 12 } -fg navy 

image create photo diag -file "diagram.png" 
label .p -image diag

button .f.b3 -command exit -text "выйти" -width 15

#-----------------------------------------------------------------------

place .t -x  20 -y 330  
place .p -x 650 -y 330 

grid .f -column 0 -row 0 -padx 30 -pady 30

grid .f.lb -column 0 -rowspan 3 
grid .f.l1 -column 2 -row 1 
grid .f.s1 -column 1 -row 0 
grid .f.s2 -column 1 -row 1
grid .f.s3 -column 1 -row 2
grid .f.fb -column 0 -row 3 ; grid .f.fb.b1 .f.fb.b2 -pady 20 -padx 20
grid .f.b3 -column 2 -row 3 -padx 20 -pady 20 
