#Libraries----
library(lme4)
library(lmerTest)
library(GGally)
library(broom)
library(broom.mixed)
library(tidyverse)
library(MASS)
library(MuMIn)
library(plyr)
library(ggpubr)
library(viridis)
library(AICcmodavg)

#Set WD----
setwd("your file location")

#Load data----
#run analyses below separately for summer and winter data
data <- read.csv("France Spain Corine Used Avail Names Summer 2020_2022.csv", stringsAsFactors = TRUE)
data <- read.csv("France Spain Corine Used Avail Names Winter 2020_2022.csv", stringsAsFactors = TRUE)

#rename the original land classes as 'arable' or 'Non-arable'
levels(data$Land_class)[levels(data$Land_class)=="Urban"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Arable (non-irrigated)"] <- "Arable"
levels(data$Land_class)[levels(data$Land_class)=="Arable (irrigated)"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Rice fields"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Orchards and vineyards"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Permanent grasslands"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Annual crops"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Agriculture/natural vegetation"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Complex cultivation patterns"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Woodland"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Natural grasslands, heath and scrub"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Rocks, bare ground & permanent snow"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Coastal water"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Inland marshes and bogs"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Inland water"] <- "Non-arable"
levels(data$Land_class)[levels(data$Land_class)=="Intertidal"] <- "Non-arable"
data$Land_class <- factor(data$Land_class,levels(data$Land_class)[c(2,1)])
levels(data$Land_class)
names(data)

#Test of significance----
#Global model with interactions (testing 3-way interaction (without sex))----
m1<-glmer(pres~Land_class*status*country +(1|ID),data=data,family="binomial",na.action="na.fail",control = glmerControl(calc.derivs = FALSE))

m2<-glmer(pres~Land_class*status+Land_class*country+(1|ID),data=data,family="binomial",na.action="na.fail",control = glmerControl(calc.derivs = FALSE))
#Status and country interaction with land class

m3<-glmer(pres~Land_class*status+country+(1|ID),data=data,family="binomial",na.action="na.fail",control = glmerControl(calc.derivs = FALSE))
#Without country interaction with land class

m4<-glmer(pres~Land_class*country+status+(1|ID),data=data,family="binomial",na.action="na.fail",control = glmerControl(calc.derivs = FALSE))
#Without status interaction with land class

m5<-glmer(pres~Land_class+status+country+(1|ID),data=data,family="binomial",na.action="na.fail",control = glmerControl(calc.derivs = FALSE))
#Main effects with land class only 

#AIC comparison----
models <- list(m1,m2,m3,m4,m5)
model.names <- c('Full Model','Status and country interaction with land class','Status interaction with land class only','Country interaction with land class only', 'Main effects with land class only')
aictab(cand.set = models, modnames = model.names)

#Plotting----
p1<-glmer(pres~Land_class:status:country-1+(1|ID),data=data,family="binomial",na.action="na.fail",control = glmerControl(calc.derivs = FALSE))

p1.coefs <- tidy(p1, conf.int = TRUE,conf.level = 0.975) %>% 
  subset(std.error < 1) %>%
  mutate(type = ifelse(grepl("", term), ""),
         term = gsub("", "", term),
         significance = cut(p.value, c(0,.024,.025,1))) %>% 
  mutate(term = factor(term, levels = term[order(desc(term))]))#Bonferoni correction

#specify order of land classses
p1.coefs$term <- factor(p1.coefs$term, levels=c("Land_classArable:statusmigrant:countryfrance","Land_classNon-arable:statusmigrant:countryfrance","Land_classArable:statusresident:countryfrance","Land_classNon-arable:statusresident:countryfrance","Land_classArable:statusmigrant:countryspain","Land_classNon-arable:statusmigrant:countryspain","Land_classArable:statusresident:countryspain","Land_classNon-arable:statusresident:countryspain"))


#plot - summer dat
ggplot(p1.coefs, aes(estimate, term, xmin = conf.low, xmax = conf.high, col=term)) +
  geom_errorbarh(height = 0.2) + geom_point(size=.2) + geom_vline(xintercept = 0, lty = 2, lwd = .5)+
  geom_hline(yintercept=c(2.5,4.5,6.5), linetype='solid',size=0.8,colour="white") +
  scale_color_manual(values=c("tan4","tomato3","tan4","tomato3","tomato3","tomato3","tan4","tomato3"))+
  theme(strip.text = element_blank()) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1))+
  labs(x="Estimate (log odds)", y="Variable") +
  theme(legend.position = "none") +
  theme(axis.ticks = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_y_discrete(limits=,labels=c("Land_classArable:statusmigrant:countryfrance" = "Arable migrant french","Land_classNon-arable:statusmigrant:countryfrance" = "Non-arable migrant french","Land_classArable:statusresident:countryfrance" ="Arable resident french", "Land_classNon-arable:statusresident:countryfrance" = "Non-arable resident french","Land_classArable:statusmigrant:countryspain" = "Arable migrant spanish","Land_classNon-arable:statusmigrant:countryspain" = "Non-arable migrant spanish","Land_classArable:statusresident:countryspain" = "Arable resident spanish","Land_classNon-arable:statusresident:countryspain" = "Non-arable resident spanish")) +
  theme(axis.text.x = element_text(colour = c("tan4","tomato3","tan4","tomato3","tomato3","tomato3","tan4","tomato3")))+
  coord_flip() 

#plot - winter data
ggplot(p1.coefs, aes(estimate, term, xmin = conf.low, xmax = conf.high, col=term)) +
  geom_errorbarh(height = 0.2) + geom_point(size=.2) + geom_vline(xintercept = 0, lty = 2, lwd = .5)+
  geom_hline(yintercept=c(2.5,4.5,6.5), linetype='solid',size=0.8,colour="white") +
  scale_color_manual(values=c("tan4","tomato3","tan4","tomato3","red","tomato3","tan4","tomato3"))+
  theme(strip.text = element_blank()) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1))+
  labs(x="Estimate (log odds)", y="Variable") +
  theme(legend.position = "none") +
  theme(axis.ticks = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_y_discrete(limits=,labels=c("Land_classArable:statusmigrant:countryfrance" = "Arable migrant french","Land_classNon-arable:statusmigrant:countryfrance" = "Non-arable migrant french","Land_classArable:statusresident:countryfrance" ="Arable resident french", "Land_classNon-arable:statusresident:countryfrance" = "Non-arable resident french","Land_classArable:statusmigrant:countryspain" = "Arable migrant spanish","Land_classNon-arable:statusmigrant:countryspain" = "Non-arable migrant spanish","Land_classArable:statusresident:countryspain" = "Arable resident spanish","Land_classNon-arable:statusresident:countryspain" = "Non-arable resident spanish")) +
  theme(axis.text.x = element_text(colour = c("tan4","tomato3","tan4","tomato3","red","tomato3","tan4","tomato3")))+
  coord_flip() 

