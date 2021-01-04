rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
library (RJSONIO)
library (lme4)
library(lmerTest)
library (ggplot2)
library(gplots)
library (emmeans)

setwd("C:/Users/bryan/Documents/Master/Méthode en SC1 Mesures comportementales/Project/RStat/data")
files <- list.files("C:/Users/bryan/Documents/Master/Méthode en SC1 Mesures comportementales/Project/RStat/data")
files
subject <- congruence <- preference <- number <- c ()

subj <- 1


for (f in files) {
  
  d <- fromJSON (f)
  
  subject <- c (subject, subj)
  preference <- c (preference, "p")
  congruence <- c (congruence, "c")
  number <- c (number, d[1])
  
  subject <- c (subject, subj)
  preference <- c (preference, "p")
  congruence <- c (congruence, "i")
  number <- c (number, d[2])
  
  subject <- c (subject, subj)
  preference <- c (preference, "d")
  congruence <- c (congruence, "c")
  number <- c (number, d[3])
  
  subject <- c (subject, subj)
  preference <- c (preference, "d")
  congruence <- c (congruence, "i")
  number <- c (number, d[4])
  
  subject <- c (subject, subj)
  preference <- c (preference, "n")
  congruence <- c (congruence, "c")
  number <- c (number, d[5])
  
  subject <- c (subject, subj)
  preference <- c (preference, "n")
  congruence <- c (congruence, "i")
  number <- c (number, d[6])
  
  subj <- subj + 1
}

dat <- data.frame (subject = subject, preference = preference,
                   congruence = congruence, number = number)

boxplot(number ~  congruence * preference, dat,
        col  = c("#56B4E9","#E69F00","#56B4E9","#E69F00","#56B4E9","#E69F00"))

boxplot(number ~ preference * congruence + (1|subject), dat,
        col  = c("#56B4E9","#E69F00","#56B4E9","#E69F00","#56B4E9","#E69F00"))

"effet simple"
boxplot(number ~ preference, dat,  col  = c("#56B4E9","#E69F00","#E45F00"))
boxplot(number ~ congruence, dat,  col  = c("#56B4E9","#E69F00"))
        
fm <- lmer(number ~ preference * congruence + (1|subject), data=dat)
fm
emmeans (fm, pairwise ~ preference)
anova(fm)


poissonStat <- glmer(number ~ preference * congruence + (1|subject), dat, family = poisson())
emmeans (poissonStat, pairwise ~ preference)
anova(poissonStat)
summary(poissonStat)
