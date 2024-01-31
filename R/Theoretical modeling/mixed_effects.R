library(readxl)
library(haven)
library(tidyverse)
library(sjPlot)
library(psych)
library(irr)
library(lme4)
library(ggplot2)
library(ggpubr)
#gc(reset = TRUE, full = TRUE)

irr_data <- read_excel('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/ICC/irr.xlsx') %>%
  select(R1, R2, R3)
#irr_data$combination <- paste(irr_data$R1, irr_data$R2, irr_data$R3, sep="_")
#table(irr_data$combination)

ICC(irr_data[, c(1, 2)])
ICC(irr_data[, c(1, 3)])
ICC(irr_data[, c(2, 3)])
ICC(irr_data)

icc(irr_data, model = "twoway", type = "agreement", unit = "average", conf.level = 0.95)
icc(irr_data[, c(1, 2)], model = "twoway", type = "agreement", unit = "single", conf.level = 0.95)
icc(irr_data[, c(1, 3)], model = "twoway", type = "agreement", unit = "single", conf.level = 0.95)
icc(irr_data[, c(2, 3)], model = "twoway", type = "agreement", unit = "single", conf.level = 0.95)


Ma_data <- read_dta('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/Stata/Ma_admin.dta')
Tsai_data <- read_dta('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/Stata/Tsai_admin.dta')

Ma_data$government <- factor(Ma_data$government, c(0,1), 
                             labels=c('Opposition', 'Ruling party'))
Ma_data$session <- factor(Ma_data$session, c(1,2,3), 
                             labels=c('Plenary', 'Committee', 'Public Hearing'))
Ma_data$pub_oriented <- factor(Ma_data$pub_oriented, c(0,1), 
                             labels=c('No', 'Yes'))
Ma_data$disboth <- factor(Ma_data$disboth, c(0,1), 
                             labels=c('No', 'Yes'))
Ma_data$ls_type <- factor(Ma_data$ls_type, c(1,2,3), 
                             labels=c('Partylist', 'District_elected', 'Aborigine'))
Ma_data$KMT <- factor(Ma_data$KMT, c(0,1), 
                             labels=c('No', 'Yes'))
Ma_data$DPP <- factor(Ma_data$DPP, c(0,1), 
                             labels=c('No', 'Yes'))
Ma_data$ambition <- factor(Ma_data$ambition, c(0,1), 
                             labels=c('No', 'Yes'))

Tsai_data$government <- factor(Tsai_data$government, c(0,1), 
                             labels=c('Opposition', 'Ruling party'))
Tsai_data$session <- factor(Tsai_data$session, c(1,2,3), 
                             labels=c('Plenary', 'Committee', 'Caucus'))
Tsai_data$pub_oriented <- factor(Tsai_data$pub_oriented, c(0,1), 
                             labels=c('No', 'Yes'))
Tsai_data$disboth <- factor(Tsai_data$disboth, c(0,1), 
                             labels=c('No', 'Yes'))
Tsai_data$ls_type <- factor(Tsai_data$ls_type, c(1,2,3), 
                             labels=c('Partylist', 'District_elected', 'Aborigine'))
Tsai_data$KMT <- factor(Tsai_data$KMT, c(0,1), 
                             labels=c('No', 'Yes'))
Tsai_data$DPP <- factor(Tsai_data$DPP, c(0,1), 
                             labels=c('No', 'Yes'))
Tsai_data$ambition <- factor(Tsai_data$ambition, c(0,1), 
                             labels=c('No', 'Yes'))

ma_session <- relevel(Ma_data$session, ref="Public Hearing")
tsai_session <- relevel(Tsai_data$session, ref="Caucus")

Ma_mvar <- Ma_data %>%
  select(imports, government, disboth, pub_oriented, ls_type, session, ambition)
Tsai_mvar <- Tsai_data %>%
  select(imports, government, disboth, pub_oriented, ls_type, session, ambition)
summary(Ma_mvar)
summary(Tsai_mvar)

ds_Ma_imports <- Ma_mvar %>%
  summarize(N = n(),
            Mean = mean(imports),
            SD = sd(imports),
            Max. = max(imports),
            Min. = min(imports))
prop.table(table(Ma_mvar$government))
prop.table(table(Ma_mvar$disboth))
prop.table(table(Ma_mvar$pub_oriented))
prop.table(table(Ma_mvar$ls_type))
prop.table(table(Ma_mvar$session))
prop.table(table(Ma_mvar$ambition))

ds_Tsai_imports <- Tsai_mvar %>%
  summarize(N = n(),
            Mean = mean(imports),
            SD = sd(imports),
            Max. = max(imports),
            Min. = min(imports))
prop.table(table(Tsai_mvar$government))
prop.table(table(Tsai_mvar$disboth))
prop.table(table(Tsai_mvar$pub_oriented))
prop.table(table(Tsai_mvar$ls_type))
prop.table(table(Tsai_mvar$session))
prop.table(table(Tsai_mvar$ambition))


model1 <- lmer(imports ~ government +
                 (1 + government|ambition),
               data = Ma_data,
               REML = FALSE)
model2 <- lmer(imports ~ government + disboth + pub_oriented + ls_type + ma_session +
                 (1 + government|ambition),
               data = Ma_data,
               REML = FALSE)
model1
model2

Ma_data %>%
  ggplot(aes(x=government, y=imports, fill=government)) +
  geom_boxplot() + 
  scale_fill_manual(values=c("green", "blue")) +
  labs(x="Legislators", y="Imports score")
m <- Ma_data %>%
  ggplot(aes(x=government, y=imports, fill=government)) +
  geom_boxplot() +
  facet_grid(ambition~.) +
  scale_fill_manual(values=c("green", "blue")) +
  coord_flip() +
  labs(x="Legislators (Ma's presidency)", y="Imports score", fill="Legislators") +
  theme(axis.title.y=element_text(size=7), axis.title.x=element_text(size=8),
        legend.title=element_text(size=8))

Tsai_data %>%
  ggplot(aes(x=government, y=imports, fill=government)) +
  geom_boxplot() +
  scale_fill_manual(values=c("blue", "green")) +
  labs(x="Legislators", y="Imports score")
t <- Tsai_data %>%
  ggplot(aes(x=government, y=imports, fill=government)) +
  geom_boxplot() +
  facet_grid(ambition~.) +
  scale_fill_manual(values=c("blue", "green")) +
  coord_flip() +
  labs(x="Legislators (Tsai's presidency)", y="Imports score", fill="Legislators") +
  theme(axis.title.y=element_text(size=7), axis.title.x=element_text(size=8),
        legend.title=element_text(size=8))
m
t

mt <- ggarrange(m, t, ncol = 1, nrow = 2)
mt
ggsave(plot = mt, path = 'C:/Users/User/Documents/Operation Holy Grail/Phase 4/Thesis/Figures',
       filename = 'Figure 6.png', device='tiff', dpi=300)

ag <- read_excel('C:/Users/User/Downloads/ag_CHN.xlsx')
ag$year <- as.factor(ag$year)

ag_bar <- ag %>%
  ggplot(aes(x=year, y=import)) +
  geom_bar(stat = "identity", position = "dodge", color = "#e9ecef") +
  geom_text(aes(label = rank), vjust = -0.4, size = 2.5) +
  labs(x="Year", y="Import value (Million USD)")
ag_bar

ggsave(plot = ag_bar, path = 'C:/Users/User/Documents/Operation Holy Grail/Phase 4/Thesis/Figures',
       filename = 'Figure 10.png', device='tiff', dpi=300)



