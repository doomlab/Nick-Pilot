####set working directory####
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Nick pilot/1 data sets")

####import file####
master = read.csv("Melted Data.csv")

####data screening####
table(master$Judgement1)
cor(master[ , 4:8], use = "pairwise.complete.obs")

####z score the variables####
master$Judged.Value = scale(master$Judged.Value)
master$COS = scale(master$COS)
master$LSA = scale(master$LSA)
master$FSG = scale(master$FSG)

####log regression mlm####
library(lme4)
library(nlme)
options(scipen = 999)

####intercept only model###
##gls = generalized linear model
model1 = glm(Recall ~ 1, 
             data = master, 
             family = binomial(), 
             na.action = "na.omit")
summary(model1)

####random intercept model####
model2 = glmer(Recall ~ (1|Partno),
                data = master,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
summary(model2)

##compare those two models
anova(model1, model2)

####main effects model####
model3 = glmer(Recall ~ (1|Partno) + Judgement1 + Judged.Value + COS + LSA + FSG,
               data = master,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(model3)

####interaction model####
model4 = glmer(Recall ~ (1|Partno) + Judgement1 * Judged.Value + COS + LSA + FSG,
               data = master,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(model4)

####maybe two sets of interactions####
model5 = glmer(Recall ~ (1|Partno) + Judgement1 * Judged.Value + COS * LSA * FSG,
               data = master,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(model5)

####ugh a mess - 5 way interaction####
model5 = glmer(Recall ~ (1|Partno) + Judgement1 * Judged.Value * COS * LSA * FSG,
               data = master,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(model5)
