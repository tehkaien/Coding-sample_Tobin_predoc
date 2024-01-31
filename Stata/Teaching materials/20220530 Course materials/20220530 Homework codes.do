**20220530 Homework codes**
*Import Excel data without using copy & paste*
//Note：資料放在哪裡要清楚//
import excel "C:\Users\User\Documents\Quantitative research methods in SS\110 (2) TA\Textbooks\20220530 Course materials\20220530 Homework_Data.xlsx", ///
sheet("Variables") firstrow

//Data cleaning//
tab B1
tab K4b
tab K2b
recode B1 (1=3)(2=2)(3=1)(4=0)(96/98=.), gen(polint)
recode K4b (95/98=.), gen(handemand)
recode K2b (95/98=.), gen(hanpre)
label variable polint  "Political interest scale"
label variable handemand "Han's demand response scale"
label variable hanpre "Han's personal prefernce scale"
tab polint
tab handemand
tab hanpre

//Density plots of each variable//
*polint*
kdensity polint, kernel(gaussian) student(1673) ytitle("Density") ///
title("Sample distribution of 'polint' variable")
*handemand*
kdensity handemand, kernel(gaussian) student(1579) ytitle("Density") ///
title("Sample distribution of 'handemand' variable")
*hanpre*
kdensity handemand, kernel(gaussian) student(1617) ytitle("Density") ///
title("Sample distribution of 'handemand' variable")

*Question 1*
regress hanpre polint 
*Question 2*
regress hanpre handemand
//Two-way scatterplot//
twoway (scatter polint hanpre) (lfit polint hanpre), ///
ytitle("Political interest scale") ///
title("Linear relationship between variables polint and hanpre")
twoway (scatter handemand hanpre) (lfit handemand hanpre), ///
ytitle("Han's demand response scale") ///
title("Linear relationship between variables handemand and hanpre")
