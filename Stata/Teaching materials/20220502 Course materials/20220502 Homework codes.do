**20220502 Homework codes**
//First checking//
tab k3a
tab k3b
tab k3c
//Generating new variables//
recode k3a (95/98=.), gen(absong)
recode k3b (95/98=.), gen(abhan)
recode k3c (95/98=.), gen(abtsai)
//Second checking//
tab absong
tab abhan
tab abtsai
//Pearson Correlation//
pwcorr abhan abtsai, star(10)
pwcorr abhan abtsai, star(5)
pwcorr abhan abtsai, star(1)
pwcorr abhan abtsai, star(0.1)
//Scatterplot with Fitted line//
twoway (scatter abhan abtsai)(lfit abhan abtsai)
twoway (scatter abhan abtsai)(lfitci abhan abtsai)
//Correlation matrix output//
//ssc install asdoc, replace//
asdoc pwcorr abhan abtsai , star(all) replace title(Correlation matrix between variable abhan and abtsai)
