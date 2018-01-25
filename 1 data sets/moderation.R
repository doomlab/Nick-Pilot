##setup
mod_dat = read.csv("Melted Data.csv")
colnames(mod_dat)[2] = "Judgement"

options(scipen = 999)
library(lme4)
library(nlme)

####data screening####
##accuracy
summary(mod_dat)

##outliers
##using mahal on judged value and recall
mahal = mahalanobis(mod_dat[ , c(4,5)],
                    colMeans(mod_dat[ , c(4,5)], na.rm = TRUE),
                    cov(mod_dat[ , c(4,5)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(mod_dat[ , c(4,5)]))
cutoff 
summary(mahal < cutoff)

noout = mod_dat[mahal < cutoff , ]
summary(noout)

##aditivity
nomiss = na.omit(noout) ##correlations aren't all that high
correl = cor(nomiss[ , -c(1:3)])
symnum(correl)

##what to do with the missing data though?

##descriptives
m1 = mean(nomiss$Judged.Value)
m2 = mean(nomiss$COS)
m3 = mean(nomiss$LSA)
m4 = mean(nomiss$FSG)

sd1 = sd(nomiss$Judged.Value)
sd2 = sd(nomiss$COS)
sd3 = sd(nomiss$LSA)
sd4 = sd(nomiss$FSG)

m1;m2;m3;m4
sd1;sd2;sd3;sd4

##do by judgement condition (recall and judged value)

####running model 5 from log mlm script####
##scaling the variables

##zscoring original dataset
#mod_dat$Judged.Value = scale(mod_dat$Judged.Value)
#mod_dat$COS = scale(mod_dat$COS)
#mod_dat$LSA = scale(mod_dat$LSA)
#mod_dat$FSG = scale(mod_dat$FSG)

##going to use the no outlier dataset for now

##zscoring no outlier dataset
noout$Judged.Value = scale(noout$Judged.Value)
noout$COS = scale(noout$COS)
##noOut$LSA = scale(noout$LSA) ##this was fucking it up.
noout$LSA = scale(noout$LSA)
noout$FSG = scale(noout$FSG)

##model 5 -- two interaction model
model5 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS * LSA * FSG,
               data = noout,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(model5)

####moderation analysis####
##looking at recall for "low", "average", "high" LSA/COS/FSG
##looking at the slopes for judged values at each of the strengths.

##cos moderation
##NA's are messing with getting the sd.
#nomiss = na.omit(noout)
##temporary fix.  Need to ask about what to actually do with the missing data.
##you added this thing earlier, so don't need to do it here
##you could also do sd(noout$LSA, na.rm = T) to remove NAs

##re-running model5 with the nomiss dataset
##I don't know that I would use this dataset because it's causing some non convergence issues, use noout 
model5 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS * LSA * FSG,
               data = nomiss,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(model5) ##significant ivs don't seem to have changed much using the new dataset.

##Now to do moderation for cosine
nomiss$COS_lOW = nomiss$COS + sd(nomiss$COS)
nomiss$COS_high = nomiss$COS - sd(nomiss$COS)

##putting it in the original model?
##looking at everything?
##high and low cos in the original models
lowcos.output1 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS_lOW * LSA * FSG,
               data = nomiss,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(lowcos.output1)

highcos.output1 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS_high * LSA * FSG,
                        data = nomiss,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"),
                        nAGQ = 1)
summary(highcos.output1)

##okay, so if i am thinking about this in the right way, these are showing how lsa and fsg
##are affected by the differing COS strengths

####LSA####
##setup
nomiss$LSA_lOW = nomiss$LSA + sd(nomiss$LSA)
nomiss$LSA_high = nomiss$LSA - sd(nomiss$LSA)

##doing the analysis the same way as cosine
lowlsa.output1 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS * LSA_lOW * FSG,
                       data = nomiss,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)
summary(lowlsa.output1)

highlsa.output1 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS * LSA_high * FSG,
                        data = nomiss,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"),
                        nAGQ = 1)
summary(highlsa.output1)

####FSG####
##setup
nomiss$FSG_lOW = nomiss$FSG + sd(nomiss$FSG)
nomiss$FSG_high = nomiss$FSG - sd(nomiss$FSG)

##FSG moderation analyis
lowfsg.output1 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS * LSA * FSG_lOW,
                       data = nomiss,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)
summary(lowfsg.output1)

highfsg.output1 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS * LSA * FSG_high,
                        data = nomiss,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"),
                        nAGQ = 1)
summary(highfsg.output1)

####checking correlations####
##lsa correlation
cor(nomiss[ ,c(4, 5, 7)])

##everything correlation!
cor(nomiss[ ,c(4:8)])

####subseting by judgement type####
thematic = subset(nomiss, 
                  Judgement == "thematic")

semantic = subset(nomiss,
                  Judgement == "semantic")

associative = subset(nomiss,
                     Judgement == "associative")

##doing correlations on the new subsets
cor(thematic[ ,c(4:8)])
cor(semantic[ ,c(4:8)])
cor(associative[ ,c(4:8)])

####splitting lsa by cosine strength####
##low cosine
lowcos.lowlsa.output = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS_lOW * LSA_lOW * FSG,
                                              data = nomiss,
                                              family = binomial,
                                              control = glmerControl(optimizer = "bobyqa"),
                                              nAGQ = 1)
summary(lowcos.lowlsa.output)

lowcos.highlsa.output = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS_lOW * LSA_high * FSG,
                              data = nomiss,
                              family = binomial,
                              control = glmerControl(optimizer = "bobyqa"),
                              nAGQ = 1)
summary(lowcos.highlsa.output)

##high cosine
highcos.lowlsa.output = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS_high * LSA_lOW * FSG,
                             data = nomiss,
                             family = binomial,
                             control = glmerControl(optimizer = "bobyqa"),
                             nAGQ = 1)
summary(highcos.lowlsa.output)

highcos.highlsa.output = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS_high * LSA_high * FSG,
                              data = nomiss,
                              family = binomial,
                              control = glmerControl(optimizer = "bobyqa"),
                              nAGQ = 1)
summary(highcos.highlsa.output)
