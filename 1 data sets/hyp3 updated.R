##setup
master = read.csv("Melted Data.csv")
colnames(master)[2] = "Judgement"

options(scipen = 999)

####data screening####
##accuracy
summary(master)

##fixing judged value
master$Judged.Value[ master$Judged.Value > 100 ] = NA
table(master$Judged.Value)

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

####try a loop####
##setup
persontable = matrix(NA,
                     nrow=length(names(table(noout$Partno))),
                     ncol=5+4+4)
colnames(persontable) = c("Partno", "AIntercept", "ACOS", "ALSA", "AFSG",
                          "SIntercept", "SCOS", "SLSA", "SFSG",
                          "TIntercept", "TCOS", "TLSA", "TFSG")

simnum = 1

##loop
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

View(persontable)

####playing with the output####
people = as.data.frame(persontable)
people2 = apply(people, 2, as.numeric)
people2 = as.data.frame(people2) ##getting it back to a dataframe
View(people2)

##group them all by type
##dropping partno column
people_thematic = people2[ , -c(1:9)]
people_semantic = people2[ , -c(1:5, 10:13)]
people_associative = people2[ , -c(1, 6:13)]

##getting absolute values
people_thematic2 = abs(people_thematic)
people_semantic2 = abs(people_semantic)
people_associative2 = abs(people_associative)

##getting rid of the intercepts
people_thematic3 = people_thematic2[ , -1]
people_semantic3 = people_semantic2[ , -1]
people_associative3 = people_associative2[ , -1]

##finding max coefficents for each row
thematic_output = names(people_thematic3)[max.col(people_thematic3, ties.method="first")]
thematic_output = as.data.frame(thematic_output)

semantic_output = names(people_semantic3)[max.col(people_semantic3, ties.method="first")]
semantic_output = as.data.frame(semantic_output)

associative_output = names(people_associative3)[max.col(people_associative3, ties.method="first")]
associative_output = as.data.frame(associative_output)

##sticking it all together
combined = cbind(people$Partno, associative_output, semantic_output, thematic_output)
colnames(combined)[1] = "Partno"
colnames(combined)[2] = "Associative"
colnames(combined)[3] = "Semantic"
colnames(combined)[4] = "Thematic"

##rename the things
combined$Associative = factor(combined$Associative,
                              labels = c("COS", "FSG", "LSA"))
combined$Semantic = factor(combined$Semantic,
                           labels = c("COS", "FSG", "LSA"))
combined$Thematic = factor(combined$Thematic,
                           labels = c("COS", "FSG", "LSA"))
View(combined)
summary(combined)
