#install.packages('readxl')
library(readxl)
data01 = read_excel('C:/Users/User/Documents/Quantitative research methods in SS/110 (2) TA/Textbooks/20220523 Course materials/Data_Q1.xlsx')
data01
data02 = read_excel('C:/Users/User/Documents/Quantitative research methods in SS/110 (2) TA/Textbooks/20220523 Course materials/Data_Q2.xlsx')
data02

data01_1 <- subset(data01, K5b!='95' & K5c!='95')
data01_2 <- subset(data01_1, K5b!='96' & K5c!='96')
data01_3 <- subset(data01_2, K5b!='97' & K5c!='97')
twbenefit <- subset(data01_3, K5b!='98' & K5c!='98')
head(twbenefit)

data02_1 <- subset(data02,Q1b!='3'& Q1b!='4'& Q1b!='9' & Q1b!='12' &
                     Q1b!='17' & Q1b!='19' & Q1b!='21' & Q1b!='41' &
                     Q1b!='95' & H1!='95')
data02_2 <- subset(data02_1, Q1b!='96' & H1!='96')
data02_3 <- subset(data02_2, Q1b!='97' & H1!='97')
democogn <- subset(data02_3, Q1b!='98' & Q1b!='99' & H1!='98')
head(democogn)

#install.packages('tidyverse')
library(tidyverse)
twbenefit <- rename(twbenefit, Han = K5b, Tsai = K5c)
democogn <- rename(democogn, partyid = Q1b, cogn = H1)
twbenefit.long <- twbenefit %>%
  gather(key = "candidate", value = "score", Han, Tsai)
head(twbenefit.long)
democogn <- democogn %>% mutate(partyid = recode(partyid, 
                                 "1" = "KMT",
                                 "2" = "DPP",
                                 .default = "Mgmt"))
democogn <- democogn %>% mutate(cogn = recode(cogn,
              "1" = "cogn1",
              "2" = "cogn2",
              "3" = "cogn3",
              .default = "Mgmt"))

#install.packages('dplyr')
library(dplyr)
twbenefitmean <- group_by(twbenefit.long, candidate) %>%
  summarize(count = n(),
            mean = mean(score, na.rm = TRUE),
            sd = sd(score, na.rm = TRUE))
twbenefitmean

###Question 1###
library(ggplot2)
twbenefit.long %>%
ggplot(aes(score,
           color=candidate, 
           fill=candidate))+
  geom_density(alpha = 0.2)+
  geom_vline(data=twbenefitmean, aes(xintercept = mean, color=candidate),
             linetype="dashed", size=0.8)+
             theme_bw()

twbenefittest <- t.test(score ~ candidate, data = twbenefit.long,
                        var.equal = TRUE, alternative = "less")
twbenefittest

#install.packages('rstatix')
library(rstatix)
twbenefit.long %>%
  t_test(score~candidate, paired = FALSE, 
         mu=0, alternative = 'less', detailed = TRUE) %>%
  add_significance()
#install.packages('ggpubr')
library(ggpubr)
compare_means(score ~ candidate, data = twbenefit.long, 
              method = "t.test",
              paired = FALSE,
              method.args = list(mu=0, alternative = "greater"))
twbenefitplot <- ggboxplot(twbenefit.long, "candidate", "score", 
                       fill = "candidate", add = "jitter",
                       palette = c("blue", "green"))+
                 stat_compare_means(method = "t.test", 
                       paired = FALSE,
                       method.args = list(mu=0, alternative = "greater"),
                       label.x = 1.4)
twbenefitplot

###Question 2###
democogntb <- table(democogn$partyid, democogn$cogn)
democogntb
democognchisq <- chisq.test(democogn$partyid, democogn$cogn, 
                            correct=FALSE)
democognchisq
democognchisq$expected

#install.packages('gplots')
library(gplots)
balloonplot(t(democogntb), main ="Democracy cognition of KMT and DPP respondents", 
            xlab ="cogn", ylab="partyid",
            label = TRUE, show.margins = TRUE)
library(corrplot)
corrplot(democogntb, is.cor = FALSE)
corrplot(democogntb, is.cor = FALSE,
         method = "number")
democognplot <- ggplot(democogn, aes(cogn, fill = partyid)) + 
                geom_bar(position = "dodge")+
                scale_fill_manual(values=c('green','blue'))
democognplot
           