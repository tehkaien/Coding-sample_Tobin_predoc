#install.packages('readxl')
library(readxl)
vjump = read_excel('C:/Users/User/Documents/Quantitative research methods in SS/110 (2) TA/Textbooks/20220516 Course materials/data01.xlsx')
vjump
twdemo = read_excel('C:/Users/User/Documents/Quantitative research methods in SS/110 (2) TA/Textbooks/20220516 Course materials/data02.xlsx')
twdemo

#install.packages('tidyverse')
library(tidyverse)
vjump.long <- vjump %>%
  gather(key = "group", value = "height", before, after)
head(vjump.long)
twdemo.long <- twdemo %>%
  gather(key = "group", value = "index", TW_beforedem, TW_afterdem)
head(twdemo.long)

#install.packages('dplyr')
library(dplyr)
vjumpmean <- group_by(vjump.long, group) %>%
  summarize(
    count = n(),
    mean = mean(height, na.rm = TRUE),
    sd = sd(height, na.rm = TRUE)
  )

twdemomean <- group_by(twdemo.long, group) %>%
  summarize(
    count = n(),
    mean = mean(index, na.rm = TRUE),
    sd = sd(index, na.rm = TRUE)
  )
library(ggplot2)
vjump.long %>%
  ggplot(aes(height,
             color=group, 
             fill=group))+
  geom_density(alpha = 0.2)+
  geom_vline(data=vjumpmean, aes(xintercept = mean, color=group),
             linetype="dashed", size=0.8)+
  theme_bw()
twdemo.long %>%
  ggplot(aes(index,
             color=group, 
             fill=group))+
  geom_density(alpha = 0.2)+
  geom_vline(data=twdemomean, aes(xintercept = mean, color=group),
             linetype="dashed", size=0.8)+
  theme_bw()

vjumpttest <- t.test(height~group, data = vjump.long, paired = TRUE,
                     mu=0, alternative = 'greater')
vjumpttest
twdemotest <- t.test(index~group, data = twdemo.long, paired = TRUE)
twdemotest
#install.packages('rstatix')
library(rstatix)
vjump.long %>%
  t_test(height~group, paired = TRUE, 
         mu=0, alternative = 'greater', detailed = TRUE) %>%
  add_significance()
twdemo.long %>%
  t_test(index~group, paired = TRUE, detailed = TRUE) %>%
  add_significance

install.packages('PairedData')
library(PairedData)
vjump.before <- subset(vjump.long, group == "before", height,
                 drop = TRUE)
vjump.after <- subset(vjump.long,  group == "after", height,
                drop = TRUE)
vjump.pd <- paired(vjump.before, vjump.after)
summary(vjump.pd)
plot(vjump.pd, type = "profile") + theme_bw()

twdemo.before <- subset(twdemo.long, group == "TW_beforedem", index,
                       drop = TRUE)
twdemo.after <- subset(twdemo.long,  group == "TW_afterdem", index,
                      drop = TRUE)
twdemo.pd <- paired(twdemo.before, twdemo.after)
summary(twdemo.pd)
plot(twdemo.pd, type = "profile") + theme_bw()

#install.packages('ggpubr')
library(ggpubr)
compare_means(height ~ group, data = vjump.long, 
              method = "t.test",
              paired = TRUE,
              method.args = list(mu=0, alternative = "greater")
              )
compare_means(index ~ group, data = twdemo.long, 
              method = "t.test",
              paired = TRUE,
              )
vjumpplot <- ggboxplot(vjump.long, "group", "height", 
              fill = "group", add = "jitter")+
  stat_compare_means(method = "t.test", 
                     paired = TRUE,
                     method.args = list(mu=0, alternative = "greater"),
                     label.x = 1.4)
vjumpplot
twdemoplot <- ggboxplot(twdemo.long, "group", "index", 
              fill = "group", add = "jitter")+
  stat_compare_means(method = "t.test", 
                     paired = TRUE,
                     label.x = 1.4)
twdemoplot
