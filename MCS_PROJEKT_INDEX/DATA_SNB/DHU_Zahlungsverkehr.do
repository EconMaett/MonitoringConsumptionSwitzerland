
/*
***************************
** Import Konsumindikatoren
***************************
import delimited "https://data.snb.ch/api/cube/conretail/data/csv/en", delimiter(";") encoding(UTF-8) clear 

drop in 1
drop in 1

rename v1 datum
rename v2 indicator
rename v3 value
drop in 1
destring value, replace
keep if indicator=="I1"
replace indicator = "DHU"
gen edate = date(datum, "YM")
gen year= year(edate)
gen month=month(edate)
gen day= day(edate)
table year
outsheet using snb_conretail_2002.csv , comma  replace


***********************************
*** merge konsum & zahlungsverkehr"
********************************

rename value DHU
drop indicator
sort edate
save test1, replace

use test, clear
keep if measure=="BMF"
keep if transaction=="ZT"
keep if instrument=="D"
keep if holder=="IZ"
keep if location=="II"
rename value ZT_D_IZ_II_BMF
sort edate
save test2, replace

use test, clear
keep if measure=="BMF"
keep if transaction=="ZT"
keep if instrument=="K"
keep if holder=="IZ"
keep if location=="II"
rename value ZT_K_IZ_II_BMF
sort edate
save test3, replace




use test1, replace
merge 1:1 edate using test2
drop if _merge <3
drop _merge
merge 1:1 edate using test3
drop if _merge <3
drop _merge
drop measure location holder instrument transaction
label var ZT_D_IZ_II_BMF "Debit card payments"
label var ZT_K_IZ_II_BMF "Credit card payments"
gen ZT_IZ_II_BMF = ZT_D_IZ_II_BMF + ZT_K_IZ_II_BMF
label var ZT_IZ_II_BMF "Total card payments"
label var DHU "Retail trade index"
order datum edate year month day
outsheet using payment_retailtrade.csv , comma  replace
save payment_retail, replace

 */

