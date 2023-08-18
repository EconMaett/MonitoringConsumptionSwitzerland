
clear 
cls
graph drop _all
macro drop _all

import delimited "d:\VadeMecum\projects\SIX\MonitoringConsumption\data\SIX.csv"
*import delimited  "https://raw.githubusercontent.com/statistikZH/covid19monitoring/master/covid19socialmonitoring.csv", encoding(UTF-8)

keep if source=="SIX Payment Services"

drop topic description update public source

gen date2 = date(date, "YMD")
format date2 %td 

order date date2 

drop if variable_short == "debiteinsatz_ausland"
keep if date2<=td(5apr2020)

** CHOOSE HERE WHICH SERIES TO MODEL: total, cash, pos

*global PLOTTYPE "TOTAL" 
*global PLOTTYPE "CASH" 
global PLOTTYPE "POS" 

if "$PLOTTYPE" == "TOTAL" {
	collapse (sum) value, by (date2)
}
else if "$PLOTTYPE" == "CASH" {
	keep if variable_short == "bezug_bargeld"
}
else if "$PLOTTYPE" == "POS" {
	keep if variable_short == "stat_einkauf"
}
else {
	exit
}

rename value total


gen actual = total 
replace total =. if  date2>=td(16mar2020) // make sure that prediction does not pick up something not it shouldnt know


tsset date2 
gen dow = dow(date2)

** make the weekday dummies
gen mo = 1 if  dow==1 
replace mo=0 if mo==.
gen tu = 1 if dow==2 
replace tu=0 if tu==.
gen we = 1 if dow==3 
replace we=0 if we==.
gen th = 1 if dow==4 
replace th=0 if th==.

gen fr = 1 if dow==5 
replace fr=0 if fr==.
gen sa = 1 if dow==6 
replace sa=0 if sa==.

** make the paydate dummies 
** not very smart implementation 
gen payday = (date2==td(24jan2020)|date2==td(25jan2020)|date2==td(27jan2020)|date2==td(25feb2020)|date2==td(26feb2020)|date2==td(27feb2020)|date2==td(25mar2020)|date2==td(26mar2020)|date2==td(27mar2020))

* estimate model up to and inclusive 15mar2020
arima total mo tu we th fr sa payday if tin(, 15mar2020), ar(1)  vce(r)

* extract coefficients to get the standarderror of predictions right
matrix def est_coeff  = e(b)
global phi  = est_coeff[1,9]
dis $phi
global sd_ehat = est_coeff[1,10]
dis $sd_ehat



*newey total mo tu we th fr sa payday l.total  if date2<td(16mar2020), lag(1) 
*predict ehat, res

** use newey west because erors are serially correlated 
*newey total mo tu we th fr sa payday if date2<td(16mar2020), lag(1) 
* an arima model on the errors suggests one lag is good. 
* predict ehat1, res
* arima ehat1 if date2<td(16mar2020), arima(1,0,0) nocon


predict ehat, res
ac ehat
predict iis if date2< td(16mar2020)
predict oos, dynamic(td(16mar2020)) y

gen losses    = actual-oos  if date2>=td(16mar2020)
gen cumlosses = sum(losses) if date2>=td(16mar2020)

** helper variables for standard deviation computaion 
* assumes AR(1) in error term 
gen fhorizon = date2-td(15mar2020) if date2>=td(16mar2020)

gen pow = 2*fhorizon 
gen pow2 = sqrt(sum($phi^pow) + 1)

*gen   oos1=oos+$sd_ehat*sqrt($nenner*($phi^fhorizon))
*gen   oos2=oos-$sd_ehat*sqrt($nenner*($phi^fhorizon))

*gen   oos12=oos+2*$sd_ehat*sqrt($nenner*($phi^fhorizon))
*gen   oos22=oos-2*$sd_ehat*sqrt($nenner*($phi^fhorizon))

gen   oos1=oos  +$sd_ehat*pow2
gen   oos2=oos  -$sd_ehat*pow2
gen   oos12=oos +2*$sd_ehat*pow2
gen   oos22=oos -2*$sd_ehat*pow2

* make pics

format date2 %tdddMonYY
twoway line actual date2 if  date2>=td(16mar2020), color(black) lwidth(thick)|| ///
line oos date2 if  date2>=td(16mar2020), color(red)   lwidth(thick) || ///
rarea   oos1    oos2  date2 if  date2>=td(16mar2020),  fcolor(red%50)  lcolor(white) || ///
rarea   oos12   oos22 date2 if  date2>=td(16mar2020), fcolor(red%10)  lcolor(white) ///
xtitle("Time since Lockdown")   ///
legend(order(1 "Actual" 2 "Forecast" 3 "Forecast +/- SE"  4 "Forecast +/- 2SE") pos(6) row(1)) ///
name(graph1) title($PLOTTYPE)



format date2 %tdddMonYY
twoway line losses date2 if  date2>=td(16mar2020), color(black) lwidth(thick) || ///
line cumlosses date2 if  date2>=td(16mar2020), color(red)   lwidth(thick)  ///
xtitle("Time since Lockdown")   ///
legend(order(1 "Actual-Forecast" 2 "Cum. Losses") pos(6) row(1)) ///
ytitle("Mio. CHF", size(small))  name(graph3) title($PLOTTYPE)
 

format date2 %tdddMonYY
tsline  actual  oos iis ,   legend(label(1 "Total Turnover") /// 
label(3 "In-sample Predicted Turnover") label(2 "Predicted Turnover")) /// 
lpattern(dash) xtitle("Time",size(small)) ytitle("Mio. CHF", size(small)) name(graph2) title($PLOTTYPE)



format date2 %td 

exit


keep date2 losses cumlosses 
keep if date2>=td(16mar2020)
cd d:\VadeMecum\projects\SIX\MonitoringConsumption\data\
export delimited  $PLOTTYPE.losses.csv
