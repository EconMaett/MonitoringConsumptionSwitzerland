cd "C:\Users\marti\SWITCHdrive\Covid19 Data"


use payment_retail, clear
rename ZT_D_IZ_II_BMF DCTV

keep if year < 2020

by year, sort: egen DHU_meanyear = mean(DHU)
by year, sort: egen DCTV_meanyear = mean(DCTV)

gen DHU_detrend = DHU / DHU_meanyear*100
gen DCTV_detrend = DCTV / DCTV_meanyear*100

scatter DHU_detrend DCTV_detrend
scatter DHU_detrend month
scatter DCTV_detrend month


gen log_DHU_detrend = ln(DHU_detrend)
gen log_DCTV_detrend = ln(DCTV_detrend)

scatter log_DHU_detrend log_DCTV_detrend


outsheet using payment_retail_detrend.csv , comma  replace


reg log_DHU_detrend log_DCTV_detrend
reg log_DHU_detrend log_DCTV_detrend if year > 2009
reg log_DHU_detrend log_DCTV_detrend if year > 2014
 