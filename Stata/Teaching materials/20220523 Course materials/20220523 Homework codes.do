*20220523 Homework codes**
*Import Excel data without using copy & paste*
//Note：資料放在哪裡要清楚//
import excel "C:\Users\User\Documents\Quantitative research methods in SS\110 (2) TA\Textbooks\20220523 Course materials\20220523 Homework_Data.xlsx", ///
sheet("Variables") firstrow

*Question 1*
//Data cleaning//
tab K5b
tab K5c
recode K5b (95/98=.), gen(hantwbenefit)
recode K5c (95/98=.), gen(tsaitwbenefit)
//Density plot//
twoway kdensity hantwbenefit, xtitle("Score") ytitle(Density) ///
title(Comparison of hantwbenefit and tsaitwbenefit) ///
color(blue*.5) lcolor(blue) lwidth(medthick) || ///
kdensity tsaitwbenefit, ///
color(green*.1) lcolor(green) lpattern(dash) lwidth(medthick) ///
legend(order(1 "Scores of hantwbenefit" 2 "Scores of tsaitwbenefit") col(1) pos(1) ring(0))
//Independent t-test//
mean tsaitwbenefit hantwbenefit
ttest tsaitwbenefit == hantwbenefit, unpaired
//Box plot//
graph box tsaitwbenefit hantwbenefit, ytitle(Scores) ///
title(Comparison between variable 'hantwbenefit' & 'tsaitwbenefit') ///
legend(order(1 "Tsai" 2 "Han") )

*Question 2*
//Data cleaning//
tab Q1b
tab H1
recode Q1b (3/99=.), gen(partyid)
recode H1 (95/98=.), gen(democogn)
label variable partyid  "Party identification"
label variable democogn "Democracy cognition"
//Chi-squared test//
tab partyid democogn, row chi2
//Chi distribution graph//
search chidemo
chidemo 2 .001
//Bar plot//
graph bar (count), over(partyid) over(democogn) asyvars ///
title(partyid and democogn) ///
legend(order(1 "Kuomintang" 2 "DPP") ) ///
b1title("Categories of democracy cognition") ytitle("Respondents") 
