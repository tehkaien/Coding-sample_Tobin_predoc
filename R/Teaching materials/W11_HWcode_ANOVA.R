##Week 11 Homework##
library(haven)
library(tidyverse)
library(DescTools)
library(sjmisc)
library(stats)
#gc(reset = TRUE, full = TRUE)

#Q1#
C00390_2 <- read_dta('C:/Users/User/Documents/R/Workspace/112 (1) Statistics@NTU/Week 11/HW/C00390_2/tscs2021q2.dta')

#將1~5分的編碼轉變成類別變數，方法一：cut()#
cut(c(1:5), breaks = c(0, 2, 3, 5),
    label = c("Fair", "Neutral", "Unfair"))

q1_data <- C00390_2 %>%
  select(d1d, g3b, g5f) %>%
  mutate(g3b_f = cut(g3b, breaks = c(0, 2, 3, 5),
                     label = c("Fair", 
                               "Neutral",
                               "Unfair"))) %>%
  mutate(g5f_f = cut(g5f, breaks = c(0, 2, 3, 5),
                     label = c("Approve",
                               "Neutral",
                               "Disapprove")))

table(q1_data$d1d)
table(q1_data$g3b)
table(q1_data$g3b_f)
table(q1_data$g5f)
table(q1_data$g5f_f)

ds <- q1_data %>%
  group_by(g3b_f) %>%
  summarize(N=n(),
            Mean=mean(d1d, na.rm = TRUE),
            Variance=var(d1d, na.rm = TRUE))

aov(d1d ~ g3b_f * g5f_f, data = q1_data) %>%
  summary()

q1_anova <- aov(d1d ~ g3b_f*g5f_f, data = q1_data)
TukeyHSD(q1_anova)

#將1~5分的編碼轉變成類別變數，方法二：case_when()#
x <- 1:5
case_when(
  x < 3 ~ "Fair",
  x %in% c(4, 5) ~ "Unfair",
  x == 3 ~ "Neutral")

C00390_2 %>%
  select(d1d, g3b, g5f) %>%
  mutate(g3b_f = case_when(
    g3b < 3 ~ "Fair",
    g3b %in% c(4, 5) ~ "Unfair",
    g3b == 3 ~ "Neutral"),
    g5f_f = case_when(
      g5f < 3 ~ "Approve",
      g5f %in% c(4, 5) ~ "Disapprove",
      g5f == 3 ~ "Neutral")) %>%
  aov(d1d ~ g3b_f*g5f_f, data=.) %>%
  summary()

interaction.plot(x.factor = q1_data$g3b_f,
                 trace.factor = q1_data$g5f_f,
                 response = q1_data$d1d,
                 fun = mean,
                 col = c("blue", "red", "grey"),
                 ylab = "Position on organ transplantation",
                 xlab = "Medical care",
                 trace.label = "Tested positive for covid")

#事先去除遺漏值#
q1_data_na_remove <- C00390_2 %>%
  select(d1d, g3b, g5f) %>%
  set_na(d1d, na=c(94:97,"NA")) %>%
  set_na(g3b, na=c(94, "NA")) %>%
  set_na(g5f, na=c(94, "NA"))
q1_data_na_remove <- q1_data_na_remove[complete.cases(q1_data_na_remove), ] #遺漏值排除後剩下1529個樣本

q1_data_na_remove %>%
  mutate(g3b_f = cut(g3b, breaks = c(0, 2, 3, 5),
                     label = c("Fair", 
                               "Neutral",
                               "Unfair"))) %>%
  mutate(g5f_f = cut(g5f, breaks = c(0, 2, 3, 5),
                     label = c("Approve",
                               "Neutral",
                               "Disapprove"))) %>%
  group_by(g3b_f, g5f_f) %>%
  summarize(N=n(),
            Mean=mean(d1d, na.rm = TRUE),
            Variance=var(d1d, na.rm = TRUE))

q1_data_na_remove %>%
  mutate(g3b_f = cut(g3b, breaks = c(0, 2, 3, 5),
                     label = c("Fair", 
                               "Neutral",
                               "Unfair"))) %>%
  mutate(g5f_f = cut(g5f, breaks = c(0, 2, 3, 5),
                     label = c("Approve",
                               "Neutral",
                               "Disapprove"))) %>%
  aov(d1d ~ g3b_f*g5f_f, data=.) %>%
  summary()
  
#Q2#
q2_data <- C00390_2 %>%
  select(d3, d4, c3a, c3b) %>%
  set_na(d3, na=c(94:98,"NA")) %>%
  set_na(d4, na=c(94:98, "NA")) %>%
  set_na(c3a, na=c(94, "NA")) %>%
  set_na(c3b, na=c(94, "NA"))
q2_data <- q2_data[complete.cases(q2_data), ] #遺漏值排除後剩下1521個樣本

table(q2_data$d3)
table(q2_data$d4)
table(q2_data$c3a)
table(q2_data$c3b)

cut(c(1:5), breaks = c(0, 2, 3, 5),
    label = c("Easy", "Neutral", "Difficult"))

q2_data <- q2_data %>%
  mutate(c3a_f = cut(c3a, breaks = c(0, 2, 3, 5),
                     label = c("Easy", 
                               "Neutral",
                               "Difficult"))) %>%
  mutate(c3b_f = cut(c3b, breaks = c(0, 2, 3, 5),
                     label = c("Easy", 
                               "Neutral",
                               "Difficult"))) %>%
  mutate(d3_f = factor(d3, c(1, 2, 3),
                       labels = c("30y", "70y", "Inappropriate"))) %>%
  mutate(d4_f = factor(d4, c(1, 2, 3),
                       labels = c("Yes", "No", "Inappropriate")))
#Q2a#
q2_data %>%
  select(d3_f, c3b_f) %>%
  table()

q2_data %>%
  select(d3_f, c3b_f) %>%
  table() %>%
  chisq.test(correct = FALSE)

#Q2b#
q2_data %>%
  select(d3_f, c3a_f) %>%
  table()

q2_data %>%
  select(d3_f, c3a_f) %>%
  table() %>%
  chisq.test(correct = F)

#Q2c#
q2_data %>%
  select(d4_f, c3b_f) %>%
  table()

#q2_data %>%
  #select(d4_f, c3b_f) %>%
  #table() %>%
  #chisq.test(correct = F)

q2_data %>%
  select(d4_f, c3b_f) %>%
  table() %>%
  fisher.test(alternative = "two.sides", simulate.p.value = TRUE)

#Q2d#
q2_data %>%
  select(d4_f, c3a_f) %>%
  table()

q2_data %>%
  select(d4_f, c3a_f) %>%
  table() %>%
  fisher.test(alternative = "two.sides", simulate.p.value = TRUE)

