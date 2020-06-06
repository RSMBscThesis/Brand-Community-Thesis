U <- read.csv2("Thesis_data_30 April 2020_14.58 GEBRUIKEN.csv", sep = ";")

install.packages("dummies")

install.packages("interactions")

library(dummies)
colnames(U)
colnames(U)[33] <- c("Age")
colnames(U)[34] <- c("Gender")
colnames(U)[35] <- c("Education")
colnames(U)[37] <- c("Employment")

U.dummies <- dummy.data.frame(U, names = c("Online.Offline","Hedonic.Utilitarian","Condition","Age","Gender","Education","Employment"))

colnames(U.dummies)

colnames(U.dummies)[19] <- c("ConditionHedonicOffline")
colnames(U.dummies)[20] <- c("ConditionHedonicOnline")
colnames(U.dummies)[21] <- c("ConditionUtililtarianOffline")
colnames(U.dummies)[22] <- c("ConditionUtilitarianOnline")
colnames(U.dummies)[23] <- c("Offline")
colnames(U.dummies)[24] <- c("Online")
colnames(U.dummies)[25] <- c("Hedonic")
colnames(U.dummies)[26] <- c("Utilitarian")
colnames(U.dummies)[38] <- c("Age1")
colnames(U.dummies)[39] <- c("Age2")
colnames(U.dummies)[40] <- c("Age3")
colnames(U.dummies)[41] <- c("Age4")
colnames(U.dummies)[42] <- c("Age5")
colnames(U.dummies)[43] <- c("Age6")
colnames(U.dummies)[44] <- c("Age7")
colnames(U.dummies)[47] <- c("GenderPreferNotToSay")
colnames(U.dummies)[48] <- c("EducationBachelor")
colnames(U.dummies)[50] <- c("EducationHighSchool")
colnames(U.dummies)[51] <- c("EducationMaster")
colnames(U.dummies)[52] <- c("EducationOther")
colnames(U.dummies)[54] <- c("EmploymentFullTime")
colnames(U.dummies)[55] <- c("EmploymentPartTime")
colnames(U.dummies)[57] <- c("EmploymentSelfEmployed")
colnames(U.dummies)[59] <- c("EmploymentUnableToWork")
colnames(U.dummies)[60] <- c("EmploymentUnemployedLooking")
colnames(U.dummies)[61] <- c("EmploymentUnemployednotlooking")

colnames(U.dummies)

summary(U.dummies)
#30-04-2020:
Model2 <- lm(scale(Average.participation) ~ 0 + scale(Hedonic) * scale(Offline) + scale(Quantitative.car) + scale(Quantitative.real.world) + scale(Quantitative.computers) + scale(Age1) + scale(Age2) + scale(Age3) + scale(Age4) + scale(Age5) + scale(Age6) + scale(Age7) + scale(GenderFemale) + scale(GenderMale) + scale(GenderPreferNotToSay) + scale(EducationBachelor) + scale(EducationDoctorate) + scale(EducationMaster) + scale(EducationHighSchool) + scale(EducationOther) +  scale(EmploymentPartTime) + scale(EmploymentFullTime) + scale(EmploymentRetired) + scale(EmploymentSelfEmployed) + scale(EmploymentStudent)  + scale(EmploymentUnableToWork) + scale(EmploymentUnemployedLooking) + scale(EmploymentUnemployednotlooking), data=U.dummies)

summary(Model2)


#01-05-2020:

Model2.1 <- lm(scale(Average.participation) ~ 0 + scale(Hedonic) * scale(Offline) + scale(Offline)*(scale(Quantitative.car) + scale(Quantitative.real.world) + scale(Quantitative.computers)+scale(Age1)+scale(Age2)+scale(Age3)+scale(Age4)+scale(Age5)+scale(Age6)+scale(Age7)+scale(GenderMale)+scale(GenderPreferNotToSay)+scale(EducationBachelor)+scale(EducationDoctorate)+scale(EducationMaster)+scale(EducationHighSchool)+scale(EmploymentPartTime)+scale(EmploymentFullTime)+scale(EmploymentRetired)+scale(EmploymentSelfEmployed)+scale(EmploymentStudent)+scale(EmploymentUnableToWork)+scale(EmploymentUnemployedLooking)+scale(EmploymentUnemployednotlooking)), data=U.dummies)

summary(Model2.1)

#Installed Functions

install.packages('data.table')
install.packages('tidyverse')
install.packages('reshape2')
install.packages('magrittr')
install.packages('dplyr')
no
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)
if(!require(gridExtra)) install.packages('gridExtra')
library('magrittr')

#Plots coefficients

plotOffline <- summary(Model2.1)$coefficients %>% 
  as.data.frame() %>% 
  mutate(coefficient_names = rownames(.),
         lower = Estimate-`Std. Error`, upper = Estimate+`Std. Error`) %>% 
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline)',
                                    'Condition Offline', coefficient_names)) %>%
  filter(coefficient_names == c('Condition Offline')) %>%
  arrange(coefficient_names) %>% 
  ggplot(aes(y = coefficient_names, x = Estimate)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = .1) +
  geom_vline(xintercept = 0, color = 'black', linetype = 3) +
  labs(x= 'Mean Estimate', y = 'Treatment') +
  theme_minimal()

plotHedonic <- summary(Model2.1)$coefficients %>% 
  as.data.frame() %>% 
  mutate(coefficient_names = rownames(.),
         lower = Estimate-`Std. Error`, upper = Estimate+`Std. Error`) %>% 
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Hedonic)',
                                    'Treatment Hedonic', coefficient_names)) %>%
  filter(coefficient_names == c('Treatment Hedonic')) %>%
  arrange(coefficient_names) %>% 
  ggplot(aes(y = coefficient_names, x = Estimate)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = .1) +
  geom_vline(xintercept = 0, color = 'black', linetype = 3) +
  labs(x= 'Mean Estimate', y = 'Treatment') +
  theme_minimal()

plotT <- summary(Model2.1)$coefficients %>% 
  as.data.frame() %>% 
  mutate(coefficient_names = rownames(.),
         lower = Estimate-`Std. Error`, upper = Estimate+`Std. Error`) %>% 
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Hedonic):scale(Offline)',
                                    'Hedonic ', coefficient_names)) %>%
  filter(coefficient_names %like% c('Hedonic ')) %>%
  arrange(coefficient_names) %>% 
  ggplot(aes(y = coefficient_names, x = Estimate)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = .1) +
  geom_vline(xintercept = 0, color = 'black', linetype = 3) +
  labs(x= 'Mean Estimate', y = 'Interaction effect Offline +') +
  theme_minimal()

plotCar <- summary(Model2.1)$coefficients %>% 
  as.data.frame() %>% 
  mutate(coefficient_names = rownames(.),
         lower = Estimate-`Std. Error`, upper = Estimate+`Std. Error`) %>% 
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(Quantitative.car)',
                                    'Car Enthusiast', coefficient_names)) %>%
  filter(coefficient_names == c('Car Enthusiast')) %>%
  arrange(coefficient_names) %>% 
  ggplot(aes(y = coefficient_names, x = Estimate)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = .1) +
  geom_vline(xintercept = 0, color = 'black', linetype = 3) +
  labs(x= 'Mean Estimate', y = 'Interaction effect Offline +') +
  theme_minimal()

plotComputer <- summary(Model2.1)$coefficients %>% 
  as.data.frame() %>% 
  mutate(coefficient_names = rownames(.),
         lower = Estimate-`Std. Error`, upper = Estimate+`Std. Error`) %>% 
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(Quantitative.computers)',
                                    'Proficient with computers', coefficient_names)) %>%
  filter(coefficient_names == c('Proficient with computers')) %>%
  arrange(coefficient_names) %>% 
  ggplot(aes(y = coefficient_names, x = Estimate)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = .1) +
  geom_vline(xintercept = 0, color = 'black', linetype = 3) +
  labs(x= 'Mean Estimate', y = 'Interaction effect Offline +') +
  theme_minimal()

plotrealworld <- summary(Model2.1)$coefficients %>% 
  as.data.frame() %>% 
  mutate(coefficient_names = rownames(.),
         lower = Estimate-`Std. Error`, upper = Estimate+`Std. Error`) %>% 
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(Quantitative.real.world)',
                                    'Brand Community member in real world', coefficient_names)) %>%
  filter(coefficient_names == c('Brand Community member in real world')) %>%
  arrange(coefficient_names) %>% 
  ggplot(aes(y = coefficient_names, x = Estimate)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = .1) +
  geom_vline(xintercept = 0, color = 'black', linetype = 3) +
  labs(x= 'Mean Estimate', y = 'Interaction effect Offline +') +
  theme_minimal()

plotCV1 <- summary(Model2.1)$coefficients %>% 
  as.data.frame() %>% 
  mutate(coefficient_names = rownames(.),
         lower = Estimate-`Std. Error`, upper = Estimate+`Std. Error`) %>% 
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(Age1)',
                                    'Age < 20', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(Age2)',
                                    'Age 20-29', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(Age3)',
                                    'Age 30-39', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(Age4)',
                                    'Age 40-49', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(Age5)',
                                    'Age 50-59', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(Age6)',
                                    'Age 60-69', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(Age7)',
                                    'Age 70-79', coefficient_names)) %>%
  filter(coefficient_names %like% c('Age ')) %>%
  arrange(coefficient_names) %>% 
  ggplot(aes(y = coefficient_names, x = Estimate)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = .1) +
  geom_vline(xintercept = 0, color = 'black', linetype = 3) +
  labs(x= 'Mean Estimate', y = 'Interaction effect Offline +') +
  theme_minimal()

plotCV2 <- summary(Model2.1)$coefficients %>% 
  as.data.frame() %>% 
  mutate(coefficient_names = rownames(.),
         lower = Estimate-`Std. Error`, upper = Estimate+`Std. Error`) %>% 
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(GenderPreferNotToSay)',
                                    'Gender - prefer not to say', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(GenderMale)',
                                    'Gender - Male', coefficient_names)) %>%
  filter(coefficient_names %like% c('Gender ')) %>%
  arrange(coefficient_names) %>% 
  ggplot(aes(y = coefficient_names, x = Estimate)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = .1) +
  geom_vline(xintercept = 0, color = 'black', linetype = 3) +
  labs(x= 'Mean Estimate', y = 'Interaction effect Offline +') +
  theme_minimal()


plotCV3 <- summary(Model2.1)$coefficients %>% 
  as.data.frame() %>% 
  mutate(coefficient_names = rownames(.),
         lower = Estimate-`Std. Error`, upper = Estimate+`Std. Error`) %>% 
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(EducationMaster)',
                                    'Education - Master', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(EducationDoctorate)',
                                    'Education - Doctorate', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(EducationBachelor)',
                                    'Education - Bachelor', coefficient_names)) %>%
  filter(coefficient_names %like% c('Education ')) %>%
  arrange(coefficient_names) %>% 
  ggplot(aes(y = coefficient_names, x = Estimate)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = .1) +
  geom_vline(xintercept = 0, color = 'black', linetype = 3) +
  labs(x= 'Mean Estimate', y = 'Interaction effect Offline +') +
  theme_minimal()

plotCV4 <- summary(Model2.1)$coefficients %>% 
  as.data.frame() %>% 
  mutate(coefficient_names = rownames(.),
         lower = Estimate-`Std. Error`, upper = Estimate+`Std. Error`) %>% 
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(EmploymentUnemployedLooking)',
                                    'Employment Unemployed - looking for work', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(EmploymentUnableToWork)',
                                    'Employment Unable to work', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(EmploymentStudent)',
                                    'Employment Student', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(EmploymentRetired)',
                                    'Employment Retired', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(EmploymentSelfEmployed)',
                                    'Employment Self-employed', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(EmploymentPartTime)',
                                    'Employment Part-time (<36h/w)', coefficient_names)) %>%
  mutate(coefficient_names = ifelse(coefficient_names=='scale(Offline):scale(EmploymentFullTime)',
                                    'Employment Full-time (>=36h/w)', coefficient_names)) %>%
  filter(coefficient_names %like% c('Employment ')) %>%
  arrange(coefficient_names) %>% 
  ggplot(aes(y = coefficient_names, x = Estimate)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = .1) +
  geom_vline(xintercept = 0, color = 'black', linetype = 3) +
  labs(x= 'Mean Estimate', y = 'Interaction effect Offline &' ) +
  theme_minimal()

gridExtra::grid.arrange(plotOffline)

gridExtra::grid.arrange(plotHedonic)

gridExtra::grid.arrange(plotT)

gridExtra::grid.arrange(plotCar)

gridExtra::grid.arrange(plotComputer)

gridExtra::grid.arrange(plotrealworld)

gridExtra::grid.arrange(plotCV1)

gridExtra::grid.arrange(plotCV2)

gridExtra::grid.arrange(plotCV3)

gridExtra::grid.arrange(plotCV4)



#Data matrix

counts <- table(U$Condition, U.dummies$Average.participation)
staafdiagram <- barplot(counts, main="Average Participation per condition",
        xlab="Average Participation", col=c("darkblue","lightblue","darkgreen","lightgreen"),
        legend = rownames(counts), beside=TRUE)

p <- ggplot(data = U, aes(x=Average.participation, y=Condition)) + 
  geom_boxplot() +
  theme_bw()

p

#Distribution Control variables

U.dummies %>% 
  group_by(Offline, Hedonic) %>% 
  summarise(n_Education = sum(EmploymentUnemployednotlooking)) %>% 
  ungroup %>% 
  mutate(n_Education = n_Education)

U.dummies %>% 
  group_by(Offline, Hedonic) %>% 
  summarise(n_Age2 = sum(Age2)) %>% 
  ungroup %>% 
  mutate(n_Age2 = n_Age2/sum(n_Age2))

U.dummies %>% 
  group_by(Offline, Hedonic) %>% 
  summarise(n_Age1 = sum(Age1)) %>% 
  ungroup %>% 
  mutate(n_Age1 = n_Age1)

U.dummies %>% 
  group_by(Offline, Hedonic) %>% 
  summarise(n_Hedonic = sum(Hedonic)) %>% 
  ungroup %>% 
  mutate(n_Hedonic = n_Hedonic/sum(n_Hedonic))

U.dummies %>% 
  group_by(Offline, Hedonic) %>% 
  summarise(n_Utilitarian = sum(Utilitarian)) %>% 
  ungroup %>% 
  mutate(n_Utilitarian = n_Utilitarian/sum(n_Utilitarian))

#Redundant
#29-04-2020:
Model1 <- lm(scale(Average.participation) ~ 0 + scale(ConditionHedonicOnline) + scale(ConditionHedonicOffline) + scale(ConditionUtililtarianOffline) + scale(ConditionUtilitarianOnline) + scale(Quantitative.car) + scale(Quantitative.real.world) + scale(Quantitative.computers) + scale(Age1) + scale(Age2) + scale(Age3) + scale(Age4) + scale(Age5) + scale(Age6) + scale(Age7) + scale(GenderFemale) + scale(GenderMale) + scale(GenderPreferNotToSay) + scale(EducationBachelor) + scale(EducationDoctorate) + scale(EducationMaster) + scale(EducationHighSchool) + scale(EducationOther) +  scale(EmploymentPartTime) + scale(EmploymentFullTime) + scale(EmploymentRetired) + scale(EmploymentSelfEmployed) + scale(EmploymentStudent)  + scale(EmploymentUnableToWork) + scale(EmploymentUnemployedLooking) + scale(EmploymentUnemployednotlooking), data=U.dummies)

summary(Model1)

