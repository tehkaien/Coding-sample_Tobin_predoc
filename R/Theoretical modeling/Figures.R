library(readxl)
library(haven)
library(tidyverse)
library(ggplot2)
library(ggstance)
library(ggpubr)
library(ggtext)
library(GGally)
library(ggthemes)
library(hrbrthemes)
library(coefplot)
library(nnet)
library(mlogit)
library(survival)
library(xtable)
library(DescTools)
library(radiant)
library(knitr)
library(grid)
library(gridExtra)
library(lubridate)
library(scales)
library(flextable)

#Imports scores time series#
#Line graph (Drop minor parties)#
rawdata01 <- read_excel('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/ts_test_dropminor.xlsx')
group <- group_by(rawdata01, party, sittingno)
imports_mean <- summarize(group, imports_mean=mean(imports))
#us_mean <- summarize(group, us_mean=mean(prous))
#list_imports <- split(imports_mean, imports_mean$party)
#list_us <- split(us_mean, us_mean$party)

imports_mean_trend <- imports_mean %>%
  ggplot(aes(sittingno, imports_mean,
             color = party))+
  geom_line(linewidth = 1)+
  expand_limits(x = c(1, 20))+
  scale_color_manual(values = c('green', 'blue'))+
  geom_vline(xintercept = 18, color = 'red', linetype='dotted', linewidth = 1)+
  labs(x = "Legislative session" , y = "Imports score (Mean)")+
  labs(color = "Legislators")

imports_mean_trend
ggsave(path = 'C:/Users/User/Documents/Operation Holy Grail/Phase 4/Thesis/Figures/',
       plot = imports_mean_trend, filename = 'Figure 8.png',
       width = 8.5, height = 5, device='tiff', dpi=300)

#Senior legislators versus otherwise#
rawdata02 <- read_excel('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/aggregate_dropminor.xlsx')
rawdata02$disboth <- factor(rawdata02$disboth, c(0,1), 
                                 labels=c('Non-senior legislators', 'Senior legislators'))
group02 <- group_by(rawdata02, disboth, party, sittingno)
imports_mean02 <- summarize(group02, imports_meandis=mean(imports))

imports_mean02 <- imports_mean02 %>%
  complete(sittingno = seq(min(sittingno), max(sittingno), by = 1), 
           fill = list(imports_meandis = NA))

imports_mean_trend_disboth <- imports_mean02 %>%
  ggplot(aes(x = sittingno, y = imports_meandis, color = factor(party))) +
  geom_path(na.rm = TRUE, linewidth = 1) +
  facet_wrap(~disboth, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c('green', 'blue')) +
  geom_vline(xintercept = 18, color = 'red', linetype='dotted', linewidth = 1) +
  labs(x = "Legislative session", y = "Imports score (Mean)", color = "Legislators")

imports_mean_trend_disboth

ggsave(path = 'C:/Users/User/Documents/Operation Holy Grail/Phase 4/Thesis/Figures',
       plot = imports_mean_trend_disboth, filename = 'Figure 9.png',
       width = 7, height = 5, device='tiff', dpi=300)

#imports_mean_trend_disboth <- imports_mean02 %>%
  #ggplot(aes(x=sittingno, y=imports_meandis))+
  #geom_point(aes(size=disboth, color=party), alpha=0.7)+
  #expand_limits(x = c(1, 20))+
  #scale_color_manual(values = c('green', 'lightgreen', 'blue', 'yellow', 'violetred3',
                                #'orange', 'turquoise1', 'darkred', 'tan1'))+
  #geom_vline(xintercept = 18, linetype='dotted', linewidth = 1, color = 'red')+
  #labs(x = "Legislative session" , y = "Imports score (Mean)")+
  #labs(size = "Experienced legislators", color = "Party")

#imports_mean_trend_disboth


#Speech count time series (sittingno)#
count_data <- read_excel('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/agg_count.xlsx')

speech_count_plot <- ggplot(count_data, aes(sittingno, count)) +
  geom_bar(stat="identity", na.rm = TRUE) +
  xlab("Sitting") + ylab("Count") +
  geom_vline(xintercept = 17, linetype='dotted', linewidth = 1, color = 'red')+
  #scale_x_date(labels=date_format ("%Y/%m/%d"), breaks=date_breaks("13 year")) +
  theme(text = element_text(size = 10))

speech_count_plot

#Speech count time series (date_full)#
#count_data$date_full <- as.Date(count_data$date_full, format = "%Y/%m/%d")
#count_data$date_tsm <- as.Date(count_data$date_tsm, format = "%Y/%m/%d")
#count_data$date_tst <- as.Date(count_data$date_tst, format = "%Y/%m/%d")
#is.na(count_data$count_full) <- count_data$count_full == 0

#speech_count_tsplot <- count_data %>%
  #ggplot(aes(date_full, count_full)) +
  #geom_bar(stat="identity", na.rm = TRUE) +
  #xlab("Date") + ylab("Count") +
  #scale_x_date(date_labels = "%Y-%m", breaks = date_breaks("1 year"))+
  #theme(text = element_text(size = 10))+
  #geom_col()

#speech_count_tsplot

#Coefficient plots for Model 1 to 4#
ma_era_rawdata <- read_excel('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/Ma_admin.xlsx')
tsai_era_rawdata <- read_excel('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/Tsai_admin.xlsx')

ma_era_rawdata$us_appraise <- factor(ma_era_rawdata$us_appraise, c(1,2,3),
                                     labels=c('Pro-U.S.', 'U.S.-skeptic', 'Unknown'))
ma_era_rawdata$government <- factor(ma_era_rawdata$government, c(0,1), 
                                    labels=c('Opposition', 'Government'))
ma_era_rawdata$session <- factor(ma_era_rawdata$session, c(1,2,3), 
                                 labels=c('Plenary', 'Committee', 'Public Hearing'))
ma_era_rawdata$pub_oriented <- factor(ma_era_rawdata$pub_oriented, c(0,1), 
                                    labels=c('No', 'Yes'))
ma_era_rawdata$disboth <- factor(ma_era_rawdata$disboth, c(0,1), 
                                      labels=c('No', 'Yes'))
ma_era_rawdata$ls_type <- factor(ma_era_rawdata$ls_type, c(1,2,3), 
                                 labels=c('Partylist', 'Districtelected', 'Aborigine'))
ma_era_rawdata$male <- factor(ma_era_rawdata$male, c(0,1), 
                                 labels=c('Female', 'Male'))
ma_era_rawdata$KMT <- factor(ma_era_rawdata$KMT, c(0,1), 
                              labels=c('No', 'Yes'))
ma_era_rawdata$DPP <- factor(ma_era_rawdata$DPP, c(0,1), 
                             labels=c('No', 'Yes'))

tsai_era_rawdata$us_appraise <- factor(tsai_era_rawdata$us_appraise, c(1,2,3),
                                     labels=c('Pro-U.S.', 'U.S.-skeptic', 'Unknown'))
tsai_era_rawdata$government <- factor(tsai_era_rawdata$government, c(0,1), 
                                    labels=c('Opposition', 'Government'))
tsai_era_rawdata$session <- factor(tsai_era_rawdata$session, c(1,2,3), 
                                   labels=c('Plenary', 'Committee', 'Caucus'))
tsai_era_rawdata$pub_oriented <- factor(tsai_era_rawdata$pub_oriented, c(0,1), 
                                      labels=c('No', 'Yes'))
tsai_era_rawdata$disboth <- factor(tsai_era_rawdata$disboth, c(0,1), 
                                 labels=c('No', 'Yes'))
tsai_era_rawdata$ls_type <- factor(tsai_era_rawdata$ls_type, c(1,2,3), 
                                 labels=c('Partylist', 'Districtelected', 'Aborigine'))
tsai_era_rawdata$male <- factor(tsai_era_rawdata$male, c(0,1), 
                              labels=c('Female', 'Male'))
tsai_era_rawdata$KMT <- factor(tsai_era_rawdata$KMT, c(0,1), 
                             labels=c('No', 'Yes'))
tsai_era_rawdata$DPP <- factor(tsai_era_rawdata$DPP, c(0,1), 
                             labels=c('No', 'Yes'))

table(ma_era_rawdata$session)
table(tsai_era_rawdata$session)
table(ma_era_rawdata$us_appraise)
table(tsai_era_rawdata$us_appraise)
ma_session <- relevel(ma_era_rawdata$session, ref="PH")
tsai_session <- relevel(tsai_era_rawdata$session, ref="Caucus")

#Party v.s. legislative session#
ps <- ma_era_rawdata %>%
  select(party, session) %>%
  table() %>%
  data.frame()

ps_ds <- ma_era_rawdata %>%
  group_by(party, session) %>%
  summarize(N = n(),
            imports_mean = mean(imports),
            SD = sd(imports)) %>%
  data.frame()

qflextable(ps_ds)
  
#Stacked#
ps %>%
  ggplot(aes(x = session, y = Freq, fill = party)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = Freq), size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c('green', 'blue', 'yellow', 'orange', 'khaki')) +
  labs(x = "Legislative session's type", y = "Count", fill = "Party")

#Panel#
ps %>%
  ggplot(aes(x = party, y = Freq, fill = party)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = Freq), size = 3, position = position_stack(vjust = 0.5)) +
  facet_wrap(~session, ncol = 3) +
  scale_fill_manual(values = c('green', 'blue', 'yellow', 'orange', 'khaki')) +
  labs(x = "Legislator's party", y = "Count", fill = "Legislator's party")

#Public attitude towards U.S.: 2008 v.s. 2020#
rawdata03 <- read_dta('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/Stata/Pro-US score 08 and 20.dta') %>%
  select(ma2008_prty, ma2008_score_new) %>%
  rename(Identifiers = ma2008_prty, US_att_score = ma2008_score_new) %>%
  mutate(Pres_elec = "2008") %>%
  select(Pres_elec, everything())

rawdata04 <- read_dta('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/Stata/Pro-US score 08 and 20.dta') %>%
  select(tsai2020_prty, tsai2020_score_new) %>%
  rename(Identifiers = tsai2020_prty, US_att_score = tsai2020_score_new) %>%
  mutate(Pres_elec = "2020") %>%
  select(Pres_elec, everything())

rawdata03$Identifiers <- factor(rawdata03$Identifiers, c(1, 2), labels = c('KMT indentifiers', 'DPP identifiers'))
rawdata04$Identifiers <- factor(rawdata04$Identifiers, c(1, 2), labels = c('KMT indentifiers', 'DPP identifiers'))
rawdata03 <- rawdata03[complete.cases(rawdata03), ]
rawdata04 <- rawdata04[complete.cases(rawdata04), ]

US_att_score_0820 <- bind_rows(rawdata03, rawdata04)

rawdata03 %>%
  group_by(Identifiers) %>%
  summarize(N=n(),
            Mean=round(mean(US_att_score, na.rm = TRUE), 2),
            SD=round(sd(US_att_score, na.rm = TRUE), 2))
rawdata04 %>%
  group_by(Identifiers) %>%
  summarize(N=n(),
            Mean=round(mean(US_att_score, na.rm = TRUE), 2),
            SD=round(sd(US_att_score, na.rm = TRUE), 2))

ds_US_att_score <- US_att_score_0820 %>%
  group_by(Pres_elec, Identifiers) %>%
  summarize(N=n(),
            Mean=round(mean(US_att_score, na.rm = TRUE), 2),
            SD=round(sd(US_att_score, na.rm = TRUE), 2), 
            SE=round(SD/sqrt(N), 2))

US_att_score_plot <- ds_US_att_score %>%
  ggplot(aes(x = Pres_elec, y = Mean, fill = Identifiers)) +
  geom_bar(stat = "Identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width = 0.15,
                position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("blue", "green")) +
  labs(x = "Presidential Election", y = "Attitude score (Mean)", fill = "Party identifiers")

US_att_score_plot

ggsave(path = 'C:/Users/User/Documents/Operation Holy Grail/Phase 4/Thesis/Figures',
       plot = US_att_score_plot, filename = 'Figure 7.png',
       width = 7.5, height = 5, device='tiff', dpi=300)


ma_era_fb_rawdata <- read_excel('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/Ma_admin_firstbf.xlsx')
tsai_era_fb_rawdata <- read_excel('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/Tsai_admin_firstbf.xlsx')

ma_era_fb_rawdata$us_appraise <- factor(ma_era_fb_rawdata$us_appraise, c(1,2,3),
                                     labels=c('Pro-U.S.', 'U.S.-skeptic', 'Unknown'))
ma_era_fb_rawdata$government <- factor(ma_era_fb_rawdata$government, c(0,1), 
                                    labels=c('Opposition', 'Government'))
ma_era_fb_rawdata$session <- factor(ma_era_fb_rawdata$session, c(1,2,3), 
                                 labels=c('Plenary', 'Committee', 'PH'))
ma_era_fb_rawdata$pub_oriented <- factor(ma_era_fb_rawdata$pub_oriented, c(0,1), 
                                      labels=c('No', 'Yes'))
ma_era_fb_rawdata$disboth <- factor(ma_era_fb_rawdata$disboth, c(0,1), 
                                 labels=c('No', 'Yes'))
ma_era_fb_rawdata$ls_type <- factor(ma_era_fb_rawdata$ls_type, c(1,2,3), 
                                 labels=c('Partylist', 'Districtelected', 'Aborigine'))
ma_era_fb_rawdata$male <- factor(ma_era_fb_rawdata$male, c(0,1), 
                              labels=c('Female', 'Male'))

tsai_era_fb_rawdata$us_appraise <- factor(tsai_era_fb_rawdata$us_appraise, c(1,2,3),
                                       labels=c('Pro-U.S.', 'U.S.-skeptic', 'Unknown'))
tsai_era_fb_rawdata$government <- factor(tsai_era_fb_rawdata$government, c(0,1), 
                                      labels=c('Opposition', 'Government'))
tsai_era_fb_rawdata$session <- factor(tsai_era_fb_rawdata$session, c(1,2,3), 
                                   labels=c('Plenary', 'Committee', 'Caucus'))
tsai_era_fb_rawdata$pub_oriented <- factor(tsai_era_fb_rawdata$pub_oriented, c(0,1), 
                                        labels=c('No', 'Yes'))
tsai_era_fb_rawdata$disboth <- factor(tsai_era_fb_rawdata$disboth, c(0,1), 
                                   labels=c('No', 'Yes'))
tsai_era_fb_rawdata$ls_type <- factor(tsai_era_fb_rawdata$ls_type, c(1,2,3), 
                                   labels=c('Partylist', 'Districtelected', 'Aborigine'))
tsai_era_fb_rawdata$male <- factor(tsai_era_fb_rawdata$male, c(0,1), 
                                labels=c('Female', 'Male'))

ma_era_sb_rawdata <- read_excel('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/Ma_admin_secondbf.xlsx')
tsai_era_sb_rawdata <- read_excel('C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/Tsai_admin_secondbf.xlsx')

ma_era_sb_rawdata$us_appraise <- factor(ma_era_sb_rawdata$us_appraise, c(1,2,3),
                                        labels=c('Pro-U.S.', 'U.S.-skeptic', 'Unknown'))
ma_era_sb_rawdata$government <- factor(ma_era_sb_rawdata$government, c(0,1), 
                                       labels=c('Opposition', 'Government'))
ma_era_sb_rawdata$session <- factor(ma_era_sb_rawdata$session, c(1,2,3), 
                                    labels=c('Plenary', 'Committee', 'PH'))
ma_era_sb_rawdata$pub_oriented <- factor(ma_era_sb_rawdata$pub_oriented, c(0,1), 
                                         labels=c('No', 'Yes'))
ma_era_sb_rawdata$disboth <- factor(ma_era_sb_rawdata$disboth, c(0,1), 
                                    labels=c('No', 'Yes'))
ma_era_sb_rawdata$ls_type <- factor(ma_era_sb_rawdata$ls_type, c(1,2,3), 
                                    labels=c('Partylist', 'Districtelected', 'Aborigine'))
ma_era_sb_rawdata$male <- factor(ma_era_sb_rawdata$male, c(0,1), 
                                 labels=c('Female', 'Male'))

tsai_era_sb_rawdata$us_appraise <- factor(tsai_era_sb_rawdata$us_appraise, c(1,2,3),
                                          labels=c('Pro-U.S.', 'U.S.-skeptic', 'Unknown'))
tsai_era_sb_rawdata$government <- factor(tsai_era_sb_rawdata$government, c(0,1), 
                                         labels=c('Opposition', 'Government'))
tsai_era_sb_rawdata$session <- factor(tsai_era_sb_rawdata$session, c(1,2,3), 
                                      labels=c('Plenary', 'Committee', 'Caucus'))
tsai_era_sb_rawdata$pub_oriented <- factor(tsai_era_sb_rawdata$pub_oriented, c(0,1), 
                                           labels=c('No', 'Yes'))
tsai_era_sb_rawdata$disboth <- factor(tsai_era_sb_rawdata$disboth, c(0,1), 
                                      labels=c('No', 'Yes'))
tsai_era_sb_rawdata$ls_type <- factor(tsai_era_sb_rawdata$ls_type, c(1,2,3), 
                                   labels=c('Partylist', 'Districtelected', 'Aborigine'))
tsai_era_sb_rawdata$male <- factor(tsai_era_sb_rawdata$male, c(0,1), 
                                   labels=c('Female', 'Male'))

#Model 1#
lm_mafb <- lm(imports~government, data = ma_era_fb_rawdata)
#lm_mafb <- survreg(imports~government, data = ma_era_rawdata, dist = "gaussian")
model1 <- summary(lm_mafb)
model1

#Model 2#
lm_masb <- lm(imports~government, data = ma_era_sb_rawdata)
model2 <- summary(lm_masb)
model2

#Model 3#
lm_tsaifb <- lm(imports~government, data = tsai_era_fb_rawdata)
model3 <- summary(lm_tsaifb)
model3

#Model 4#
lm_tsaisb <- lm(imports~government, data = tsai_era_sb_rawdata)
model4 <- summary(lm_tsaisb)
model4

#Full model#
lm_ma <- lm(imports~government+pub_oriented+disboth+ls_type+ma_session+male, 
            data = ma_era_rawdata)
lm_tsai <- lm(imports~government+pub_oriented+disboth+ls_type+tsai_session+male, 
              data = tsai_era_rawdata)
summary(lm_ma)
summary(lm_tsai)

AIC(lm_mafb)
AIC(lm_tsaifb)
BIC(lm_mafb) 
BIC(lm_tsaifb)
AIC(lm_ma)
BIC(lm_tsai)

#Full model#
fm <- multiplot(lm_ma, lm_tsai, title = NULL, intercept = FALSE, 
          innerCI = 2, outerCI = 0, single = FALSE, horizontal = FALSE,
          zeroColor = "red", zeroLWD = 0.3, color = "blue",
          xlab = "Coefficients", ylab = "Variables",
          coefficients = c("governmentGovernment", "pub_orientedYes",
                           "disbothYes", "ls_typeDistrictelected",
                           "ls_typeAborigine", "ma_sessionPlenary", 
                           "ma_sessionCommittee", "tsai_sessionPlenary",
                           "tsai_sessionCommittee", "maleMale"),
          newNames = c(governmentGovernment = 'Ruling party legislators',
               pub_orientedYes = 'Legislators mentioned public-related terms',
               disbothYes = 'Experienced legislators',
               ls_typeDistrictelected = 'District-elected v.s. Party-list',
               ls_typeAborigine = 'Aborigine v.s. Party-list',
               ma_sessionPlenary = 'Plenary v.s. Public Hearings',
               ma_sessionCommittee = 'Committee v.s. Public Hearings',
               tsai_sessionPlenary = 'Plenary v.s. Caucus',
               tsai_sessionCommittee = 'Committee v.s. Caucus',
               maleMale = 'Male legislators'),
               names=c(lm_ma = "Model 1 (Ma era)", lm_tsai= "Model 2 (Tsai era)"))+
  scale_color_manual(values=c("blue","green"))+
  theme_bw()+
  theme(legend.position="none")
fm

#Model 1 to 4#
multiplot(lm_mafb, lm_masb, lm_tsaifb, lm_tsaisb, title = NULL, intercept = FALSE, 
          innerCI = 2, outerCI = 0, single = FALSE, horizontal = FALSE,
          zeroColor = "red", zeroLWD = 0.3, color = "blue", ncol = 2,
          xlab = "Coefficients", ylab = "Variables",
          coefficients = c("governmentGovernment"),
          newNames = c(governmentGovernment = 'Ruling party legislators'),
          names=c(lm_mafb = "Model 1 (Ma’s first ban-lifting)", lm_masb= "Model 2 (Ma’s second ban-lifting)",
          lm_tsaifb = "Model 3 (Before Tsai’s ban-lifting)", lm_masb= "Model 4 (After Tsai’s ban-lifting)"))+
  scale_color_manual(values=c("blue","red", "green", "red"))+
  theme_bw()+
  theme(legend.position="none")

ggsave(path = 'C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/',
       filename = 'm1m4.png', width = 8, height = 5, device='tiff', dpi=300)
          
#Coefficient plot for Model 9 & 10#
ma_usappraisal <- relevel(ma_era_rawdata$us_appraise, ref="U.S.-skeptic")
tsai_usappraisal <- relevel(tsai_era_rawdata$us_appraise, ref="U.S.-skeptic")
summary(ma_era_rawdata)

mlogit_ma <- multinom(ma_usappraisal~government, data=ma_era_rawdata)
summary(mlogit_ma)
z_ma <- summary(mlogit_ma)$coefficients/summary(mlogit_ma)$standard.errors
z_ma
p_ma <- (1 - pnorm(abs(z_ma), 0, 1)) * 2
p_ma
pseudor2_ma <- PseudoR2(mlogit_ma)
pseudor2_ma

mlogit_tsai <- multinom(tsai_usappraisal~government, data=tsai_era_rawdata)
summary(mlogit_tsai)
z_tsai <- summary(mlogit_tsai)$coefficients/summary(mlogit_tsai)$standard.errors
z_tsai
p_tsai <- (1 - pnorm(abs(z_tsai), 0, 1)) * 2
p_tsai
pseudor2_tsai <- PseudoR2(mlogit_tsai)
pseudor2_tsai

AIC(mlogit_ma)
AIC(mlogit_tsai)
BIC(mlogit_ma)
BIC(mlogit_tsai)

excoef_ma <- tidy(mlogit_ma , conf.int = TRUE)
excoef_ma <- filter(excoef_ma, term!= "(Intercept)", term!= "ls_typeAborigine")
excoef_tsai <- tidy(mlogit_tsai, conf.int = TRUE)
excoef_tsai <- filter(excoef_tsai, term!= "(Intercept)", term!= "ls_typeAborigine")

excoef_ma$term <- fct_recode(excoef_ma$term,
                             "Ruling party legislators" = "governmentGovernment")
                             #"'Public' terms" = "pub_orientedYes",
                             #"Legislators gave speeches in both eras" = "disbothYes",
                             #"District v.s. Party-list" = "ls_typeDistrictelected",
                             #"Plenary v.s. PH" = "ma_sessionPlenary",
                             #"Committee v.s. PH" = "ma_sessionCommittee",
                             #"Male legislators" = "maleMale")
excoef_ma_reorder <- excoef_ma
excoef_ma_reorder$term <- factor(excoef_ma_reorder$term,
                                 levels = c("Ruling party legislators"))
                                            #"'Public' terms",
                                            #"Legislators gave speeches in both eras", 
                                            #"District v.s. Party-list", 
                                            #"Plenary v.s. PH",
                                            #"Committee v.s. PH", 
                                            #"Male legislators"))
excoef_ma_reorder$title <- "Model 9 (Ma era)"

m9 <- ggplot(excoef_ma_reorder, aes(x=estimate , y=term , color=y.level))+
  geom_pointrangeh(aes(xmin=conf.low , xmax=conf.high),
                   position=position_dodgev(height=0.75))+
  geom_vline(xintercept = 0, linetype='dotted', linewidth = 1, color = 'red')+
  theme_bw()+
  xlab("Coefficients")+
  ylab("Variables")+
  labs(color = "Appraisals (v.s. U.S.-skeptism)")+
  facet_grid(. ~title)
m9

excoef_tsai$term <- fct_recode(excoef_tsai$term,
                               "Ruling party legislators" = "governmentGovernment")
                               #"'Public' terms" = "pub_orientedYes",
                               #"Legislators gave speeches in both eras" = "disbothYes",
                               #"District v.s. Party-list" = "ls_typeDistrictelected",
                               #"Plenary v.s. Caucus" = "tsai_sessionPlenary",
                               #"Committee v.s. Caucus" = "tsai_sessionCommittee",
                               #"Male legislators" = "maleMale")
excoef_tsai_reorder <- excoef_tsai
excoef_tsai_reorder$term <- factor(excoef_tsai_reorder$term,
                                   levels = c("Ruling party legislators"))
                                              #"'Public' terms",
                                              #"Legislators gave speeches in both eras", 
                                              #"District v.s. Party-list",
                                              #"Plenary v.s. Caucus", 
                                              #"Committee v.s. Caucus",
                                              #"Male legislators"))
excoef_tsai_reorder$title <- "Model 10 (Tsai era)"

m10 <- ggplot(excoef_tsai_reorder, aes(x=estimate , y=term , color=y.level))+
  geom_pointrangeh(aes(xmin=conf.low , xmax=conf.high),
                   position=position_dodgev(height=0.75))+
  geom_vline(xintercept = 0, linetype='dotted', linewidth = 1, color = 'red')+
  theme_bw()+
  xlab("Coefficients")+
  ylab("Variables")+
  labs(color = "Appraisals (v.s. U.S.-skeptism)")+
  facet_grid(. ~title)
m10

combined <- ggarrange(m9 + scale_color_manual(values = c("red", "blue")) + rremove("ylab"),
                      m10 + scale_color_manual(values = c("red", "blue")) + rremove("ylab"),
                      common.legend = TRUE, legend = "bottom", ncol = 2)
combined

annotate_figure(combined, left = textGrob("Variables", rot = 90, gp = gpar(fontsize = 12)))
ggsave(path = 'C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/',
       filename = 'm9m10.png', width = 8.5, height = 5, device='tiff', dpi=300)
                