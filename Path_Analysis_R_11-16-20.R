#------------------------PATH ANALYSIS----------------------------#
#
# In our model, we will treat stigma consciousness and 
# job demands as predictors of engagement. The effects of stigma consciousness
# and job demands on engagement will be both direct and indirect 
# (via psychological safety and proactivity).
#
# Install Lavaan

install.packages("lavaan")
library(lavaan)

#Read data into R and store in data object. 
getwd()

processdata<-read.csv("Path_Analysis_Hyde.csv",header=TRUE,sep=",")

#Look at the structure of the data.

str(processdata)

#------------------------STEP 1----------------------------#
# Use lavaan model syntax to specify path model and have it stored 
# in an R object. 
#
#------Model Specification------#

model<-'
#equation where psychological safety is predicted by stigma consciousness 
#and job demands
psychsafe~stigcon+jobdemands
#equation where engagment is predicted by psychological safety, proactivity
#stigma consciousness, and job demands
engage~psychsafe+proactivity+stigcon+jobdemands
#equation where proactivity is predicted by stigma consciousness
#and job demands
proactivity~stigcon+jobdemands
#estimating the variances of the exogenous variables (stigma consciousness
#and job demands)
stigcon~~stigcon
jobdemands~~jobdemands
#estimating the covariances of the exogenous variables stigma consciousness
#and job demands)
stigcon~~jobdemands
#The auto.var argument when fitting the model can be used so that 
#you do not have to directly request estimation of residual variances
#Estimating the covariance of residuals for psychological safety and 
#proactivity
psychsafe~~proactivity'


#------------------------STEP 1----------------------------#
# Use 'lavaan' function to run analysis.

fit<-lavaan(model,data=processdata,auto.var=TRUE)

# The 'summary' function can be used to obtain various fit measures and the 
# parameter estimates for the model. 

summary(fit,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE)


#------------------------Visualizations----------------------------#
# Two methods for obtaining path diagrams:
#                                                           
# 1) Use the 'semPaths' function from the 'semPlot' package.

install.packages("semPlot")
library("semPlot")
                                                          
semPaths(fit,what="paths",whatLabels="par",style="lisrel",layout="tree", rotation=2)
                                                          
# 2) Use the 'lavaanPlot" function from the 'lavaanPlot'package.

install.packages("lavaanPlot")

library(lavaanPlot)

lavaanPlot(model = fit, node_options = list(shape = "box", fontname = 
"Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,covs=
TRUE,stars = c("regress"))


#----------------------INDIRECT AND TOTAL EFFECTS----------------------#
#
#------Model Specification------#
# Indirect and Total Effects of STIGMA CONSCIOUSNESS on ENGAGEMENT
  
model<-'
#equation where psychological safety is predicted by stigma consciousness 
#and job demands
psychsafe~a*stigcon+jobdemands
#equation where engagment is predicted by psychological safety, proactivity
#stigma consciousness, and job demands
engage~b*psychsafe+e*proactivity+c*stigcon+jobdemands
#equation where proactivity is predicted by stigma consciousness
#and job demands
proactivity~d*stigcon+jobdemands
#estimating the variances of the exogenous variables (stigma consciousness
#and job demands)
stigcon~~stigcon
jobdemands~~jobdemands
#estimating the covariances of the exogenous variables stigma consciousness
#and job demands)
stigcon~~jobdemands
#The auto.var argument when fitting the model can be used so that 
#you do not have to directly request estimation of residual variances
#Estimating the covariance of residuals for psychological safety and 
#proactivity
psychsafe~~proactivity
#calculating specific indirect effect of stigma consciousness on
# engagement via psychological safety
SIE1:=a*b
#calculating specific indirect effect of stigma consciousness on
# engagement via proactivity
SIE2:=d*e
#calculating total indirect effect of stigma consciousness on 
# engagement via mediators
TIE:=SIE1+SIE2
#calculating total effect of mastery on achieve
TE:=TIE+c'

#using naive bootstrap to obtain standard errors
fit<-lavaan(model,data=processdata, auto.var=TRUE, se="bootstrap")

summary(fit,fit.measures=TRUE)

#Using 'parameterEstimates' function will give us confidence intervals based
#on naive bootstrap. A standard approach to testing indirect effects.

parameterEstimates(fit)

#------Model Specification------# 
# Indirect and Total Effects of JOB DEMANDS on ENGAGEMENT

model<-'
#equation where psychological safety is predicted by stigma consciousness 
#and job demands
psychsafe~stigcon+a*jobdemands
#equation where engagment is predicted by psychological safety, proactivity
#stigma consciousness, and job demands
engage~b*psychsafe+e*proactivity+stigcon+c*jobdemands
#equation where proactivity is predicted by stigma consciousness
#and job demands
proactivity~stigcon+d*jobdemands
#estimating the variances of the exogenous variables (stigma consciousness
#and job demands)
stigcon~~stigcon
jobdemands~~jobdemands
#estimating the covariances of the exogenous variables stigma consciousness
#and job demands)
stigcon~~jobdemands
#The auto.var argument when fitting the model can be used so that 
#you do not have to directly request estimation of residual variances
#Estimating the covariance of residuals for psychological safety and 
#proactivity
psychsafe~~proactivity
#calculating specific indirect effect of stigma consciousness on
# engagement via psychological safety
SIE1:=a*b
#calculating specific indirect effect of stigma consciousness on
# engagement via proactivity
SIE2:=d*e
#calculating total indirect effect of stigma consciousness on 
# engagement via mediators
TIE:=SIE1+SIE2
#calculating total effect of mastery on achieve
TE:=TIE+c'


fit<-lavaan(model,data=processdata, auto.var=TRUE, se="bootstrap")

summary(fit,fit.measures=TRUE)

parameterEstimates(fit)