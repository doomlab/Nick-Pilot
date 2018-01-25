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

##descriptives
m1 = mean(nomiss$Judged.Value)
m2 = mean(nomiss$COS)
m3 = mean(nomiss$LSA)
m4 = mean(nomiss$FSG)

sd1 = sd(nomiss$Judged.Value)
sd2 = sd(nomiss$COS)
sd3 = sd(nomiss$LSA)
sd4 = sd(nomiss$FSG)

##zscoring no outlier dataset
noout$Judged.Value = scale(noout$Judged.Value)
noout$COS = scale(noout$COS)
noout$LSA = scale(noout$LSA)
noout$FSG = scale(noout$FSG)

####Analysis Time!####
##original model/ model 5 -- two interaction model
model5 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS * LSA * FSG,
               data = noout,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(model5)

##moderation analysis
##cosine
noout$COS_lOW = noout$COS + sd(noout$COS, na.rm = TRUE)
noout$COS_high = noout$COS - sd(noout$COS, na.rm = TRUE)

##high and low cos in the original models
lowcos.output1 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS_lOW * LSA * FSG,
                       data = noout,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)
summary(lowcos.output1)

highcos.output1 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS_high * LSA * FSG,
                        data = noout,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"),
                        nAGQ = 1)
summary(highcos.output1)

##LSA
noout$LSA_lOW = noout$LSA + sd(noout$LSA, na.rm = TRUE)
noout$LSA_high = noout$LSA - sd(noout$LSA, na.rm = TRUE)

lowlsa.output1 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS * LSA_lOW * FSG,
                       data = noout,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)
summary(lowlsa.output1)

highlsa.output1 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS * LSA_high * FSG,
                        data = noout,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"),
                        nAGQ = 1)
summary(highlsa.output1)

##FSG
noout$FSG_lOW = noout$FSG + sd(noout$FSG)
noout$FSG_high = noout$FSG - sd(noout$FSG)

lowfsg.output1 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS * LSA * FSG_lOW,
                       data = noout,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)
summary(lowfsg.output1)

highfsg.output1 = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS * LSA * FSG_high,
                        data = noout,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"),
                        nAGQ = 1)
summary(highfsg.output1)

####checking correlations####
##lsa correlation
cor(noout[ ,c(4, 5, 7)], use = "pairwise.complete.obs")

##everything correlation!
cor(noout[ ,c(4:8)], use = "pairwise.complete.obs")

####subseting by judgement type####
thematic = subset(noout, 
                  Judgement == "thematic")

semantic = subset(noout,
                  Judgement == "semantic")

associative = subset(noout,
                     Judgement == "associative")

##doing correlations on the new subsets
cor(thematic[ ,c(4:8)], use = "pairwise.complete.obs")
cor(semantic[ ,c(4:8)], use = "pairwise.complete.obs")
cor(associative[ ,c(4:8)], use = "pairwise.complete.obs")

####splitting lsa by cosine strength####
##low cosine
lowcos.lowlsa.output = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS_lOW * LSA_lOW * FSG,
                             data = noout,
                             family = binomial,
                             control = glmerControl(optimizer = "bobyqa"),
                             nAGQ = 1)
summary(lowcos.lowlsa.output)

lowcos.highlsa.output = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS_lOW * LSA_high * FSG,
                              data = noout,
                              family = binomial,
                              control = glmerControl(optimizer = "bobyqa"),
                              nAGQ = 1)
summary(lowcos.highlsa.output)

##high cosine
highcos.lowlsa.output = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS_high * LSA_lOW * FSG,
                              data = noout,
                              family = binomial,
                              control = glmerControl(optimizer = "bobyqa"),
                              nAGQ = 1)
summary(highcos.lowlsa.output)

highcos.highlsa.output = glmer(Recall ~ (1|Partno) + Judgement + Judged.Value + COS_high * LSA_high * FSG,
                               data = noout,
                               family = binomial,
                               control = glmerControl(optimizer = "bobyqa"),
                               nAGQ = 1)
summary(highcos.highlsa.output)