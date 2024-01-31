#install.packages('readxl')
library(readxl)
data = read_excel('C:/Users/User/Documents/Quantitative research methods in SS/110 (2) TA/Textbooks/20220502 Course materials/Data.xlsx')
data

newdata1 <- subset(data, k3a!='95' & k3b!='95' & k3c!='95')
newdata2 <- subset(newdata1, k3a!='96' & k3b!='96' & k3c!='96')
newdata3 <- subset(newdata2, k3a!='97' & k3b!='97' & k3c!='97')
rundata <- subset(newdata3, k3a!='98' & k3b!='98' & k3c!='98')
rundata

cor(rundata$k3b, rundata$k3c)
plot(rundata$k3b, rundata$k3c)

#install.packages('correlation')
library(correlation)
pcsum <- cor.test(rundata$k3b, rundata$k3c)
pcsum
pctab <- correlation::correlation(rundata,
                               include_factors = TRUE, method = "auto")
pctab

#install.packages('ggplot2')
library(ggplot2)
ggplot(rundata) +
  aes(x = k3c, y = k3b) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

#install.packages('ggstatsplot')
#install.packages('ggside')
library(ggstatsplot)
ggscatterstats(
  data = rundata,
  x = k3c,
  y = k3b,
  bf.message = FALSE
)

#install.packages('corrplot')
library(corrplot)
corrplot(cor(rundata),
         method = "number",
         type = "upper"
)
