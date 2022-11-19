$title Cost objective calculations for UC models
$iftheni %obj% == "pwl" costmodel(gen)= 1;
$elseifi %obj% == "quad" costmodel(gen)= 2;
$elseifi %obj% == "linear" costmodel(gen)= 2;
$elseifi %obj% == "0" costmodel(gen)= 0;
$else $abort "Fix invalid option --obj=%obj%";
$endif

*-- Convexity Check
* Not part of system of equations
* LP/QCP/NLP can't handle nonconvex piecewise linear cost functions
set thisgen(gen);



* Objective function
c_obj..
    V_objcost =e=

*    + 0.01*sum((bus,t)$(atBusSwitchedBs(bus) and (ord(t) ge 2)), sqr(V_shunt(bus,t)  - V_shunt(bus,t-1)))

*    + 0.01*sum((i,j,c,t)$(transformer(i,j,c) and (ord(t) ge 2)), sqr(V_ratio(i,j,c,t) - V_ratio(i,j,c,t-1)))

*    + 100000*sum((bus,t)$(Vref(bus,t)), sqr(V_V(bus,t)-Vref(bus,t)))

*    + 100*sum((gen,t), sqr(V_Q(gen,t)))




$iftheni.obj0 %obj% == 0
    + 0

*    + 300000*sum((bus,t)$(businfo(bus,'type','given') eq 1), sqr(V_V(bus,t) - 1))
*    + 30000*sum((bus,t), sqr(V_V(bus,t) - 1))


*IEEE 57
*1E-1 TQD
*3E4  TVD
*1E2  TQG
*    + 0.1*sum((bus,t)$(atBusSwitchedBs(bus) and (ord(t) ge 2)), sqr(V_shunt(bus,t)  - V_shunt(bus,t-1)))

*    + 30000*sum((bus,t)$(Vref(bus,t)), sqr(V_V(bus,t) - Vref(bus,t)))

*    + 100*sum((gen,t), sqr(V_Q(gen,t)))


**    + 0.1*sum((i,j,c,t)$(transformer(i,j,c) and (ord(t) ge 2)), V_ratioP(i,j,c,t)  - V_ratioN(i,j,c,t)))
**    + 0.01*sum((i,j,c,t)$(transformer(i,j,c)), V_Uratio(i,j,c,t))
**    + 0.5*sum((i,j,c,t)$(transformer(i,j,c) and (ord(t) ge 2)), sqr(V_ratio(i,j,c,t) - V_ratio(i,j,c,t-1)))





*IEEE 118
*1E-2
*3E5
*1E2
    + 0.01*sum((bus,t)$(atBusSwitchedBs(bus) and (ord(t) ge 2)), sqr(V_shunt(bus,t)  - V_shunt(bus,t-1)))

    + 300000*sum((bus,t)$(Vref(bus,t)), sqr(V_V(bus,t) - Vref(bus,t)))

    + 100*sum((gen,t), sqr(V_Q(gen,t)))




$else.obj0

$iftheni.sol %obj% == "pwl"
* Piecewise linear objective function
   +0

$elseifi.sol %obj% == "quad"
* Quadratic objective function

$elseifi.sol %obj% == "linear"
* Linear objective function

$endif.sol
$endif.obj0
;
