**20220509 Homework codes**
//First checking//
tab q2
tab q4
//Generating new variables//
recode q2 (95/98=.), gen(kmtpre)
recode q4 (1/2=1)(3/4=2)(5=3)(6=4)(95/98=.), gen(cshandling)
**Note: Groups of variable 'cshandling'**
*Group 1�GKMT�B�z�⩤ĳ�D��O����n
*Group 2�GDPP�B�z�⩤ĳ�D��O����n
*Group 3�G��j�ҳB�z�⩤ĳ�D��O������
*Group 4�G��j�ҳB�z�⩤ĳ�D��O����ok
//Second checking//
tab kmtpre
tab cshandling
//One-way ANOVA//
mean kmtpre, over(cshandling)
oneway kmtpre cshandling, scheffe
//Boxplot//
graph box kmtpre, over(cshandling)



