##setup
master = read.csv("Melted Data.csv")
colnames(master)[2] = "Judgement"

options(scipen = 999)

####data screening####
##accuracy
summary(master)

##outliers
##using mahal on judged value and recall
mahal = mahalanobis(master[ , c(4,5)],
                    colMeans(master[ , c(4,5)], na.rm = TRUE),
                    cov(master[ , c(4,5)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(master[ , c(4,5)]))
cutoff 
summary(mahal < cutoff)

noout = master[mahal < cutoff , ]
summary(noout)

##aditivity
nomiss = na.omit(noout) ##correlations aren't all that high
correl = cor(nomiss[ , -c(1:3)])
symnum(correl)

####analysis time####
##take 1
##going off of notes from meeting
tapply(noout$Judged.Value, list(noout$Judgement, function(x){lm(Judged.Value ~ COS + LSA + FSG,
                                                   data = noout)})) ##nope try again

##saving the function for later use
y = function(x){lm(Judged.Value ~ COS + LSA + FSG,
               data = noout)}

##trying it without the list
t1 = unlist(tapply(noout$Judged.Value, list(noout$Judgement, noout$Partno) , y)) ##okay this is giving the overall slopes 
t1                                              ##assuming this is averaged across participants?

##also slopes are the same across all three judgement types
##probably means its not actually doing what i think its doing

##third times the charm?
tapply(noout$Judged.Value, with(noout$Judgement, y)) ##also doesn't like this

##moving on. Attempt number 4
t2 = tapply(noout$Judged.Value, noout$Judgement , y)$coefficients 
t2 ##also nope

##maybe do some subsetting?
sub1 = subset(noout,
              noout$Judgement == "associative")
sub2 = subset(noout,
              noout$Judgement == "semantic")
sub3 = subset(noout,
              noout$Judgement == "thematic")

##running the lm stuff just on the subsets
model1 = lm(Judged.Value ~ COS + LSA + FSG, ##assoc
               data = sub1)
model1

model2 = lm(Judged.Value ~ COS + LSA + FSG, ##semantic
            data = sub2)
model2

model3 = lm(Judged.Value ~ COS + LSA + FSG, ##thematic
            data = sub3)
model3

##assuming all of these are averages
##also noticed that if you average together the coefficients from each category the results
##are pretty close to the coefficients from the base model

##still need to actually break it down by participant but this is a start
##trying to break it down by participant again
t3 = tapply(noout$Judgement, list(noout$Judged.Value, 
                                  noout$Partno), y)  
t3   ##okay this at least did something but it sure as hell aint right

##switch the variables and try again
t4 = tapply(noout$Judged.Value, list(noout$Judgement, 
                                  noout$Partno), y)  
t4
##okay this still isn't right. But!
##its set up now in a way where i would have these scores for each participant
##so progress maybe?

##lets keep at it
t5 = tapply(noout$Judged.Value, list(noout$Judgement, 
                                     noout$Partno), mean)  
t5
##okay so it works for means...
##maybe its an issue with the function i'm using

View(y)

model_1 = lm(Judged.Value ~ COS + LSA + FSG,  ##this still works just fine...
   data = noout) ##all the function things just print out each participant with the overall coefficients
model_1

t6 = by(noout$Judged.Value, list(noout$Judgement, 
                                     noout$Partno), y)  
t6 ##didn't work...

##maybe this?
summary(model1)$coefficients ##literally just the same output from before.

z = function(x){summary(model1)$coefficients}

t6 = tapply(noout$Judged.Value, list(noout$Judgement, 
                                     noout$Partno), z)  
t6 ##nope. Points for creativity maybe.

##trying this thing I found on stackoverflow
sapply(split(noout, noout$Partno), function(x) {
  model4 = lm(Judged.Value ~ COS + LSA + FSG,
              data = noout)
  c(Value = noout$Judge.Value, coef(model4),R2 = summary(model4)[["r.squared"]])
})
##i feel like this is what i need, BUT IT STILL GIVES ME THE SAME FUCKING NUMBERS FOR EACH ONE

##going back to the tapply method
##maybe try combining the two approachs?
t6 = tapply(noout$Judged.Value, list(noout$Judgement, 
                                     noout$Partno), function(x) {
                                       model4 = lm(Judged.Value ~ COS + LSA + FSG,
                                                   data = noout)
                                       c(Value = noout$Judge.Value, coef(model4),R2 = summary(model4)[["r.squared"]])
                                     })
t6
##didn't work

##trying something else i found online
s = split(noout$Judged.Value, noout$Partno)
s

s2 = sapply(s, function(x) {
  model4 = lm(Judged.Value ~ COS + LSA + FSG,
              data = noout)
  c(Value = noout$Judge.Value, coef(model4),R2 = summary(model4)[["r.squared"]])
})
s2
##and this is just doing the same as earlier. Everything is the same number

library(broom)
library(dplyr)

temp = noout %>% group_by(Partno) %>%
  do(model7 = lm(Judged.Value ~ COS +LSA + FSG, data = noout))

# get the coefficients by group in a tidy data_frame
nooutCoef = tidy(temp, model7)
nooutCoef
View(nooutCoef)  ##same fucking numbers. why?


##try a loop
persontable = matrix(NA,
                     nrow=length(names(table(noout$Partno))),
                     ncol=5+4+4)
colnames(persontable) = c("Partno", "AIntercept", "ACOS", "ALSA", "AFSG",
                          "SIntercept", "SCOS", "SLSA", "SFSG",
                          "TIntercept", "TCOS", "TLSA", "TFSG")

simnum = 1

noout$Judged.Value2 = noout$Judged.Value/100

for ( person in names(table(noout$Partno)) ){ ##loop over participants
  
  temp1 = subset(noout, Partno == person & Judgement == "associative")
  temp2 = subset(noout, Partno == person & Judgement == "semantic")
  temp3 = subset(noout, Partno == person & Judgement == "thematic")
  
  persontable[ simnum , 1] = person
  
  if (nrow(temp1) > 9) {
    model9 = lm(Judged.Value2 ~ COS + LSA + FSG, data = temp1)
    persontable[ simnum , 2:5] = model9$coefficients
    }
  
  if(nrow(temp2) > 9) {
    model10 = lm(Judged.Value2 ~ COS + LSA + FSG, data = temp2)
    persontable[ simnum , 6:9] = model10$coefficients
    }
  
  if(nrow(temp3) > 9) {
    model11 = lm(Judged.Value2 ~ COS + LSA + FSG, data = temp3)
    persontable[ simnum , 10:13] = model11$coefficients
    }
  
   simnum = simnum + 1
  
  }
