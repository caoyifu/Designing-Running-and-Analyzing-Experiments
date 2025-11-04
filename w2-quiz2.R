#week 2 quiz 2
deviceprefAB = read.csv("deviceprefs.csv")
View(deviceprefAB)
deviceprefAB$Subject = factor(deviceprefAB$Subject) # convert to nominal factor
deviceprefAB$Pref = factor(deviceprefAB$Pref) # Rv4
deviceprefAB$Disability = factor(deviceprefAB$Disability) # Rv4
summary(deviceprefAB)
plot(deviceprefAB$Pref) 

#q5.one-sample chi-square test
prfs = xtabs( ~ Pref, data=deviceprefAB)
prfs # show counts
chisq.test(prfs)

#q6. 
nodis = binom.test(sum(deviceprefAB[deviceprefAB$Disability == "0",]$Pref == "touchpad"), nrow(deviceprefAB[deviceprefAB$Disability == "0",]), p=1/2)
nodis

#q7
yesdis = binom.test(sum(deviceprefAB[deviceprefAB$Disability == "1",]$Pref == "touchpad"), nrow(deviceprefAB[deviceprefAB$Disability == "1",]), p=1/2)
yesdis

#q8:two-sample Chi-Square test of proportions 
prfs = xtabs( ~ Pref + Disability, data=deviceprefAB) # the '+' sign indicates two vars
View(prfs)
chisq.test(prfs)

#q9 Perform a two-sample G-test on preferences by disability status
library(RVAideMemoire)
G.test(prfs)

#q10 Perform Fisher's exact test on preferences by disability status.
fisher.test(prfs)
