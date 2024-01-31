#install.packages('readxl')
library(readxl)
data = read_excel('C:/Users/Kai/Desktop/Quarantine/110 (2) TA/Textbooks/20220509 Course materials/Data.xlsx')
data

newdata1 <- subset(data, Q2!='95' & Q4!='95')
newdata2 <- subset(newdata1, Q2!='96' & Q4!='96')
newdata3 <- subset(newdata2, Q2!='97' & Q4!='97')
rundata <- subset(newdata3, Q2!='98' & Q4!='98')
head(rundata)

#install.packages('dplyr')
library(dplyr)
grouping <- rundata %>% group_by(Q4)
grouping
#install.packages('tidyverse')
library(tidyverse)
rundata_ <- rundata %>% mutate(cshandling = recode(Q4, 
                                                   "1" = "Group 1",
                                                   "2" = "Group 1",
                                                   "3" = "Group 2",
                                                   "4" = "Group 2",
                                                   "5" = "Group 3",
                                                   "6" = "Group 4",
                                                   .default = "Mgmt"))
rundata_

library(ggplot2)
ggplot(data=rundata_, aes(x=cshandling, y=Q2, fill = cshandling))+
  geom_boxplot()
str(rundata_)
aggregate(rundata_$Q2, list(rundata_$cshandling), FUN=mean)
onewayanovamodel<-aov(rundata_$Q2~rundata_$cshandling)
summary(onewayanovamodel)

#install.packages('DescTools')
library(DescTools)
ScheffeTest(onewayanovamodel)
