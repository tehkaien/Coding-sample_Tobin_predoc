**20220516 Homework codes**
*Import Excel data without using copy & paste*
//Note：資料放在哪裡要清楚//
import excel "C:\Users\Kai\Desktop\Quarantine\110 (2) TA\Textbooks\20220516 Course materials\20220516 Homework_Data.xlsx", ///
sheet("Variables") firstrow

*Question 1*
mean after before
mean before after
ttest after == before
ttest before == after
//Box plot//
graph box after before, ytitle(Height) ///
title(Comparison of vertical jump height)
//Density plot//
twoway kdensity before, xtitle("Height") ytitle(Density) ///
title(Comparison of vertical jump height) ///
color(blue*.5) lcolor(blue) lwidth(medthick) || ///
kdensity after, ///
color(red*.1) lcolor(red) lpattern(dash) lwidth(medthick) ///
legend(order(1 "Vertical jump heights before PT" 2 "Vertical jump heights after PT") col(1) pos(1) ring(0))

*Question 2*
mean tw_beforedem tw_afterdem
ttest tw_beforedem == tw_afterdem
//Box plot//
graph box tw_beforedem tw_afterdem, ytitle(Index) ///
title(Comparison of liberal democratization index of Taiwan)
//Density plot//
twoway kdensity tw_beforedem, xtitle("Index") ytitle(Density) ///
title(Comparison of liberal democratization index of Taiwan) ///
color(blue*.5) lcolor(blue) lwidth(medthick) || ///
kdensity tw_afterdem, ///
color(red*.1) lcolor(red) lpattern(dash) lwidth(medthick) ///
legend(order(1 "Democratization index before 1987" 2 "Democratization index after 1987") col(1) pos(1) ring(0))

//ASDOC output//
asdoc ttest after == before, replace title(Paired T-test results of 0516 Homework)
asdoc ttes