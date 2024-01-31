##Packages##
#install.packages('readxl')
#install.packages('ggplot2')
#install.packages('tidyverse')

##Libraries##
library(readxl)
library(ggplot2)
library(tidyverse)
library(plyr)
library(dplyr)

##Importing Data##
data01 <- read_excel('C:/Users/User/Documents/Shelly/Statistics/data.xlsx')

##Visualizing amounts##
#Bar plot#
#Q1b: Party ID#
#H1: Cognition#

#Select variables from dataset#
democogndata <- data01 %>% select(Q1b, H1)
#Data cleaning#
data01_1 <- subset(democogndata,Q1b!='3'& Q1b!='4'& Q1b!='9' & Q1b!='12' &
                     Q1b!='17' & Q1b!='19' & Q1b!='21' & Q1b!='41' &
                     Q1b!='95' & H1!='95')
data01_2 <- subset(data01_1, Q1b!='96' & H1!='96')
data01_3 <- subset(data01_2, Q1b!='97' & H1!='97')
democogn <- subset(data01_3, Q1b!='98' & Q1b!='99' & H1!='98')
head(democogn)
democogn <- democogn %>% rename(c("Q1b"="partyid", "H1"="cogn"))
democogn <- democogn %>% mutate(partyid = recode(partyid, 
                                                 "1" = "KMT",
                                                 "2" = "DPP"))
democogn <- democogn %>% mutate(cogn = recode(cogn,
                                              "1" = "Cognition 1",
                                              "2" = "Cognition 2",
                                              "3" = "Cognition 3"))
democogntb <- table(democogn$partyid, democogn$cogn)
democogntb

democognplot <- ggplot(democogn, aes(cogn, fill = partyid)) + 
  geom_bar(position = "dodge")+
  scale_fill_manual(values=c('green','blue'))+ #Changing bar colors
  labs(x = "Cognitions", y = "Count")  #Changing axis titles
democognplot


#Line plot#
#Donald Trump's presidential approval ratings#
trump_approval <- data01 %>% select(date, approve_trump, disapprove_trump)
trump_approval <- trump_approval %>%
  rename(c("approve_trump" = "Approve", "disapprove_trump" = "Disapprove"))

#Converting to long data#
trump_approval.long <- trump_approval %>%
  gather(key = "group", value = "rate", Approve, Disapprove)
head(trump_approval.long)

trump_approval.long$date <- as.Date(trump_approval.long$date, '%m/%d/%Y')

ggplot(trump_approval.long,
       aes(x = date, y = rate, col = group))+
  geom_line()

#Adding details#
ggplot(trump_approval.long,
       aes(x = date, y = rate, col = group))+
  geom_line()+
  labs(x = "Date" , y = "Rates")+
  scale_color_manual(name="Group", 
                     values=c(Approve="blue", Disapprove="red"))+   #Changing colors
  scale_x_date(date_labels = "%Y-%m")+    #Showing "Year-Month" in x-axis
  ggtitle("Donald Trump's Presidential Approval Ratings, 2017-2021")


##Visualizing distribution##
#Histogram#
#Create a long data#
set.seed(1234)
df_weight <- data.frame(Sex = factor(rep(c("Female", "Male"), each = 200)),
  Weight=round(c(rnorm(200, mean=55, sd=5), rnorm(200, mean=65, sd=5))))
head(df_weight)

df_weight %>%
  ggplot(aes(x=Weight)) + geom_histogram()

df_weight %>%
  ggplot(aes(x=Weight)) + geom_histogram(bindwidth=1)

df_weight %>%
  ggplot(aes(x=Weight)) +
  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=mean(Weight)),
             color="red", linetype="dashed", size=0.8)

#With groups (Male & Female)#
df_weight %>%
  ggplot(aes(x=Weight, color=Sex)) +
  geom_histogram(fill="white")

df_weight %>%
  ggplot(aes(x=Weight, color=Sex)) +
  geom_histogram(fill="white", position="identity")

weight_mean <- ddply(df_weight, "Sex", summarize, mean=mean(Weight))
weight_mean

df_weight %>%
  ggplot(aes(x=Weight, color=Sex)) +
  geom_histogram(fill="white", position="identity") +
  geom_vline(data=weight_mean, aes(xintercept=mean, color=Sex),
           linetype="dashed", size=0.8)

#Box plot#
#K5b：韓國瑜維護台灣利益分數
#K5c：蔡英文維護台灣利益分數

benefit_score <- data01 %>% dplyr::select(K5b, K5c)
data01_4 <- subset(benefit_score, K5b!='95' & K5c!='95')
data01_5 <- subset(data01_4, K5b!='96' & K5c!='96')
data01_6 <- subset(data01_5, K5b!='97' & K5c!='97')
twbenefit <- subset(data01_6, K5b!='98' & K5c!='98')
twbenefit <- twbenefit %>% rename(c("K5b"="Han", "K5c"="Tsai"))
head(twbenefit)

summary(twbenefit)

twbenefit.long <- twbenefit %>%
  gather(key = "candidate", value = "score", Han, Tsai)
head(twbenefit.long)

ggplot(twbenefit.long,
       aes(x = candidate, y = score))+
  geom_boxplot()+
  labs(x = "Candidate" , y = "Score")

#Scatter plot#
#Loading built-in data: Violent Crime Rates by US State in 1973#
data(USArrests)
#Murder: 每十萬人中因謀殺而被逮捕的人數
#Assault: 每十萬人中因襲擊而被逮捕的人數
#Rape: 每十萬人中因強姦而被逮捕的人數
#UrbanPop: 該州都市人口百分比

USArrests %>%
  ggplot(aes(x=Murder, y=UrbanPop)) + geom_point()

USArrests %>%
  ggplot(aes(x=Assault, y=UrbanPop)) + geom_point()

USArrests %>%
  ggplot(aes(x=Rape, y=UrbanPop)) + geom_point()

#Adding regression line and CI#
murder_scatter <- USArrests %>%
  ggplot(aes(x=Murder, y=UrbanPop)) + geom_point() +
  geom_smooth(method=lm, level=0.95) +
  ggtitle("Murder")

assault_scatter <- USArrests %>%
  ggplot(aes(x=Assault, y=UrbanPop)) + geom_point() +
  geom_smooth(method=lm, level=0.95) +
  ggtitle("Assault")

rape_scatter <- USArrests %>%
  ggplot(aes(x=Rape, y=UrbanPop)) + geom_point() +
  geom_smooth(method=lm, level=0.95) +
  ggtitle("Rape")

#Formula of regression line#
lmrape <- lm(UrbanPop~Rape, data=USArrests)
summary(lmrape)

#install.packages("cowplot")
library(cowplot)
plot_grid(murder_scatter, assault_scatter, rape_scatter, nrow=1, 
          align = "v")

##Hypothesis testing##
#Paired t-test#
#Q: 政治學界著名的跨國調查資料庫V-Dem (Varieties of Democracy)在2022年3月釋出的
#最新資料中，其中有個變項是蒐集台灣西元1900~2021年間的自由民主指數(Liberal democracy index)。
#指數範圍落在0~1之間，指數越高越民主，反之則不然。今天有一研究台灣的外國學者預期:
#在西元1987年(不包含1987年)解嚴前後各34年期間的自由民主指數有顯著差異，並進行檢定。
#請問他的預期結果顯著嗎？顯著在哪一個顯著層級(信賴區間)之下？

#YEAR_ID：自西元1987年起前/後1~34年
#TW_beforedem：1987年之前的自由民主指數
#TW_afterdem：1987年之後的自由民主指數

#Ha: μ西元1987年以前的台灣自由民主指數 ≠ μ西元1987年以後的台灣自由民主指數
#H0: μ西元1987年以前的台灣自由民主指數 = μ西元1987年以後的台灣自由民主指數

twdemo <- data01 %>% select(YEAR_ID, TW_beforedem, TW_afterdem)
twdemo.long <- twdemo %>%
  gather(key = "group", value = "index", TW_beforedem, TW_afterdem)
head(twdemo.long)

twdemomean <- group_by(twdemo.long, group) %>%
  dplyr::summarize(count = n(),
            mean = mean(index, na.rm = TRUE),
            sd = sd(index, na.rm = TRUE))

#Density plot#
twdemo.long %>%
  ggplot(aes(index,
             color=group, 
             fill=group))+
  geom_density(alpha = 0.2)+
  geom_vline(data=twdemomean, aes(xintercept = mean, color=group),
             linetype="dashed", linewidth=0.8)+
  theme_bw()

#Testing#
twdemotest <- t.test(index~group, data = twdemo.long, paired = TRUE)
twdemotest

#Adding details#
#install.packages('rstatix')
library(rstatix)
twdemo.long %>%
  t_test(index~group, paired = TRUE, detailed = TRUE) %>%
  add_significance
#外國學者認為在西元1987年解嚴前後各34年期間的自由民主指數確實有顯著差異，
#此結果顯著在顯著水準p<.001之下(p=6.32e-22)

#Visualization: Box plot#
#install.packages('ggpubr')
library(ggpubr)
twdemoplot <- ggboxplot(twdemo.long, "group", "index", 
                        fill = "group", add = "jitter")+
  stat_compare_means(method = "t.test", 
                     paired = TRUE,
                     label.x = 1.3)+
  labs(x = "Groups", y = "Index")
twdemoplot


#Independent t-test#
#Q: 試用獨立t檢定回答，是否2020年總統大選中的選民認為:
#「蔡英文比韓國瑜還能夠維護台灣利益」?

#那如果0表示您覺得這個候選人「根本不能」維護臺灣利益，10表示「完全能夠」維護臺灣利益。
#K5b：韓國瑜維護台灣利益分數
#K5c：蔡英文維護台灣利益分數

#Ha: μ蔡英文維護台灣利益分數 > μ韓國瑜維護台灣利益分數  #選民普遍認為蔡英文比韓國瑜還能夠維護台灣利益 
#H0: μ蔡英文維護台灣利益分數 ≤ μ韓國瑜維護台灣利益分數  #民眾認為韓國瑜比蔡英文還能夠維護台灣利益，或者認為兩者不相上下

#benefit_score <- data01 %>% dplyr::select(K5b, K5c)
#data01_4 <- subset(benefit_score, K5b!='95' & K5c!='95')
#data01_5 <- subset(data01_4, K5b!='96' & K5c!='96')
#data01_6 <- subset(data01_5, K5b!='97' & K5c!='97')
#twbenefit <- subset(data01_6, K5b!='98' & K5c!='98')
#twbenefit <- twbenefit %>% rename(c("K5b"="Han", "K5c"="Tsai"))
#head(twbenefit)

#twbenefit.long <- twbenefit %>% gather(key = "candidate", value = "score", Han, Tsai)
#head(twbenefit.long)

twbenefitmean <- group_by(twbenefit.long, candidate) %>%
  dplyr::summarize(count = n(),
            mean = mean(score, na.rm = TRUE),
            sd = sd(score, na.rm = TRUE))
twbenefitmean

#Density plot#
twbenefit.long %>%
  ggplot(aes(score,
             color=candidate, 
             fill=candidate))+
  geom_density(alpha = 0.2)+
  geom_vline(data=twbenefitmean, aes(xintercept = mean, color=candidate),
             linetype="dashed", size=0.8)+
  theme_bw()

#Bartlett's test#
bartlett.test(score ~ candidate, twbenefit.long)
#結果證明蔡英文與韓國瑜分數的母體變異數相異(heteroscedasticity)#
#p=0.000000003577 (3.577e-09)#

#Testing#
twbenefittest <- t.test(score ~ candidate, data = twbenefit.long,
                        var.equal = TRUE, alternative = "less")  #Han > Tsai
twbenefittest

#Adding details#
twbenefit.long %>%
  t_test(score~candidate, paired = FALSE, 
         mu=0, alternative = 'less', detailed = TRUE) %>%
  add_significance()
#2020年大選的選民認為蔡英文比韓國瑜還能夠維護台灣利益，
#此結果顯著在p<.001的層級之下(p=1.74e-164)。

#Box plot#
twbenefitplot <- ggboxplot(twbenefit.long, "candidate", "score", 
                           fill = "candidate", add = "jitter",
                           palette = c("blue", "green"))+
  stat_compare_means(method = "t.test", 
                     paired = FALSE,
                     method.args = list(mu=0, alternative = "greater"),
                     label.x = 1.3)+
  labs(x = "Candidate", y= "Score")
twbenefitplot


#One-way ANOVA#
#Q: 試用One-way ANOVA比較民眾對台灣兩大黨處理兩岸議題的好壞的態度
#對於民眾對兩大黨的喜好程度的差別

#Q2: 兩大黨喜好分數(0~10分)
#Q4: 民眾對兩大黨處理兩岸議題的好壞的態度
# 1 = 國民黨好很多
# 2 = 國民黨好一些
# 3 = 民進黨好一些
# 4 = 民進黨好很多
# 5 = 兩個政黨都不錯
# 6 = 兩個政黨都不好

#自變數分四組，以方便分析
# 1 = 國民黨處理兩岸議題能力比較好 (態度一)
# 2 = 民進黨處理兩岸議題能力比較好 (態度二)
# 3 = 兩黨處理兩岸議題能力都不錯 (態度三)
# 4 = 兩黨處理兩岸議題能力都不OK (態度四)

#Ha: μ態度一 ≠ μ態度二 ≠ μ態度三 ≠ μ態度四
#民眾對於台灣兩大黨處理兩岸關係議題好壞的不同態度之間給予國民黨的喜好分數有顯著差異
#H0: μ態度一 = μ態度二 = μ態度三 = μ態度四
#民眾對於台灣兩大黨處理兩岸關係議題好壞的不同態度之間給予國民黨的喜好分數沒有顯著差異

#Data cleaning#
cshandlingdata <- data01 %>% select(Q2, Q4)
data01_7 <- subset(cshandlingdata, Q2!='95' & Q4!='95')
data01_8 <- subset(data01_7, Q2!='96' & Q4!='96')
data01_9 <- subset(data01_8, Q2!='97' & Q4!='97')
cshandling <- subset(data01_9, Q2!='98' & Q4!='98')
head(cshandling)

#Re-group#
#態度一：將編碼1(國民黨好很多)、2(國民黨好一些)歸類為「國民黨處理兩岸議題能力比較好」
#態度二：將編碼3(民進黨好很多)、4(民進黨好一些)歸類為「民進黨處理兩岸議題能力比較好」
#態度三：將編碼5(兩個政黨都不錯)歸類為「兩黨處理兩岸議題能力都不錯」(不變)
#態度四：將編碼6(兩個政黨都不好)歸類為「兩黨處理兩岸議題能力都不OK」(不變)

cshandling_ <- cshandling %>% mutate(Group = recode(Q4, 
                                                    "1" = "Group 1",
                                                    "2" = "Group 1",
                                                    "3" = "Group 2",
                                                    "4" = "Group 2",
                                                    "5" = "Group 3",
                                                    "6" = "Group 4"))
cshandling_

#Bartlett's test#
bartlett.test(Q2 ~ Group, cshandling_)
#當p值大於顯著水準.01之下(90%信賴區間)，四個態度組別之間的變異數相等

#Testing#
onewayanovamodel <- aov(Q2 ~ Group, data = cshandling_)
summary(onewayanovamodel)
#民眾對於台灣兩大黨處理兩岸關係議題好壞的不同態度之間給予國民黨的喜好分數有顯著差異，此結果顯著在p<.001之下(99.9%信賴區間)

#Box plot#
cshandling_ %>% 
  ggplot(aes(x=Group, y=Q2, fill=Group))+
  geom_boxplot()+
  labs(x = "Group", y = "KMT Score")+
  scale_fill_manual(values=c("Group 1" = 'blue', 
                              "Group 2" = 'green', 
                              "Group 3" = 'red',
                              "Group 4" = 'grey'))

#Post-hoc test#
#install.packages("DescTools")
library(DescTools)
ScheffeTest(onewayanovamodel)
#傾向態度二的民眾的國民黨喜好分數比傾向態度一的民眾還少了大約2.62分(p<.001)
#傾向態度三的民眾的國民黨喜好分數比傾向態度一的民眾還少了大約0.43分(p>.10)
#傾向態度四的民眾的國民黨喜好分數比傾向態度一的民眾還少了大約1.77分(p<.001)
#傾向態度三的民眾的國民黨喜好分數比傾向態度二的民眾還多了大約2.19分(p<.001)
#傾向態度四的民眾的國民黨喜好分數比傾向態度二的民眾還多了大約0.85分(p<.001)
#傾向態度四的民眾的國民黨喜好分數比傾向態度三的民眾還少了大約1.34分(p<.001)

#Cross-tabulation analysis#
#Q: 試用交叉檢定證明「民眾的民主認知與政黨傾向」之間有沒有關係?

#Q1b: 民眾的政黨傾向
#H1: 民眾的「民主認知」，編碼如下：
# 1 = 不管什麼情況，民主政治都是最好的體制 (認知一：民主好)
# 2 = 在有些情況下，威權的政治體制比民主政治好 (認知二：威權好)
# 3 = 對我而言，任何一種政治體制都是一樣 (認知三：兩種政體差不多)

#Ha: 民眾的民主認知與他們的政黨傾向有關係
#H0: 民眾的民主認知與他們的政黨傾向無關

#Data cleaning# #詳見上面長條圖處理步驟
#Q1b: 清除與回答無關的值，留下國民黨與民進黨的民眾
#H1: 清除與回答無關的值，留下民眾的三個民主認知

#Test#
democogntb <- table(democogn$partyid, democogn$cogn)
democogntb  #先製表
democognchisq <- chisq.test(democogn$partyid, democogn$cogn, 
                            correct=FALSE)  #計算檢定值
#在χ^2=87.1755，df=2的情況之下，可以知道民眾的民主認知以及他們的政黨傾向有關係
#統計結果顯著在p<.001層級之下

democognchisq
democognchisq$expected  #搭配期望值
prop.table(democogntb, 1)  #搭配百分比，以橫加總100%為基準
#比較兩黨民眾

#Balloon plot#
#install.packages('gplots')
library(gplots)
balloonplot(t(democogntb), main = "Democracy cognition of KMT and DPP respondents", 
            xlab = "cogn", ylab= "partyid",
            label = TRUE, show.margins = TRUE)
#Correlation plot#
#install.packages('corrplot')
library(corrplot)
corrplot(democogntb, is.cor = FALSE) #Bubbles
corrplot(democogntb, is.cor = FALSE,
         method = "number")          #Replacing bubbles with numbers
#Bar plot#
democognplot <- ggplot(democogn, aes(cogn, fill = partyid)) + 
  geom_bar(position = "dodge")+
  scale_fill_manual(values=c('green','blue'))+
  labs(x = "Democracy Cognitions", y = "Count")
democognplot

#OLS regression#

#Logistic regression#

#Multinomial logistic regression#


