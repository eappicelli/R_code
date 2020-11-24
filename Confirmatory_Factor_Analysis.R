#--------------CONFIRMATORY FACTOR ANALYSIS----------------------------#
# Dropbox\Active Faculty\Hyde, S_Den_Bus\Workers with ADHD_Fall 2020
# 11-24-2020
# EA
#
#------------------------GETTING STARTED----------------------------#
# Install Lavaan and semPlot

install.packages("lavaan")
install.packages("semPlot")

library(lavaan)
library(semPlot)

#Read data into R and store in data object. 
getwd()

processdata<-read.csv("Hyde_CFA.csv",header=TRUE,sep=",")

#look at the structure of the data.

str(processdata)

#------Model Specification------#

model<-'
  Proactivity =~ V1 + V2 + V3+ V4
  Thriving =~ V5 + V6 + V7 + V8 + V9 + V10 + V11 +V12 + V13+ V14
  Jobdemands =~ V15 + V16 + V17 + V18 + V19 + V20 + V21 +V22 + V23+ V24
  Engagement =~ V25 + V26 + V27 + V28 + V29 + V30 + V31 +V32 + V33
  Psych_safe =~ V34 + V35 + V36 + V37 + V38 + V39 + V40 + V41
  Stig_con =~ V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49
'

fit <- cfa(model, data=processdata)

#------Model Summary ------#
summary(fit, standardized=TRUE, fit.measures=TRUE, rsq=TRUE)
  
semPaths(fit, what="paths", whatLabels = "stand")




#------Model2 Specification------#
# Remove V16 and V39 from model (p>.05)

model2<-'
  Proactivity =~ V1 + V2 + V3+ V4
  Thriving =~ V5 + V6 + V7 + V8 + V9 + V10 + V11 +V12 + V13+ V14
  Jobdemands =~ V15 + V17 + V18 + V19 + V20 + V21 +V22 + V23+ V24
  Engagement =~ V25 + V26 + V27 + V28 + V29 + V30 + V31 +V32 + V33
  Psych_safe =~ V34 + V35 + V36 + V37 + V38 + V40 + V41
  Stig_con =~ V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49'

fit2 <- cfa(model2, data=processdata)

#------Model2 Summary ------#
# Request modification indices by adding argument 'modindices=TRUE'
# to summary() call

summary(fit2, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)

semPaths(fit, what="paths", whatLabels = "stand")

