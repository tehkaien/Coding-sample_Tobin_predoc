**20220509 Homework codes**
//First checking//
tab q2
tab q4
//Generating new variables//
recode q2 (95/98=.), gen(kmtpre)
recode q4 (1/2=1)(3/4=2)(5=3)(6=4)(95/98=.), gen(cshandling)
**Note: Groups of variable 'cshandling'**
*Group 1：KMT處理兩岸議題能力比較好
*Group 2：DPP處理兩岸議題能力比較好
*Group 3：兩大黨處理兩岸議題能力都不錯
*Group 4：兩大黨處理兩岸議題能力都不ok
//Second checking//
tab kmtpre
tab cshandling
//One-way ANOVA//
mean kmtpre, over(cshandling)
oneway kmtpre cshandling, scheffe
//Boxplot//
graph box kmtpre, over(cshandling)



