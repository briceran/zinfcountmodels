
################################################
## Code for Zero inflated count models report ##
##     Brice Randolph & Junhyung Park         ##
################################################


##############################################################
################ Loading and cleaning data    ################
##############################################################

#rm(list=ls())
library(foreign)
all.data<-read.dta("http://www.ats.ucla.edu/stat/stata/notes/lahigh.dta", convert.factors=TRUE)

attr(all.data,"label.table")  # aggregate  bilingual factor 

all.data$nateng[all.data$biling=="no bilingual"]<-1 #informed by codebook
all.data$nateng[all.data$biling!="no bilingual"]<-0
all.data$school<-as.factor(all.data$school)

# combine levels for groups with small representation  
# (see report for details) 
table(all.data$ethnic)
levels(all.data$ethnic)<-c("other", "asian", "afr amer", "hispanic", "white", "other", "other")

#########  center and scale variables (numeric)   ##########
all.data$mathnce<-scale(x =all.data$mathnce ,center=TRUE,scale=TRUE)
all.data$langnce<-scale(x =all.data$langnce ,center=TRUE,scale=TRUE)


######## Split training and testing data  ##########
set.seed(5)  

TestIndices<-sample(1:nrow(all.data),size = 100,replace=FALSE)
test<-all.data[TestIndices,] 
train<-all.data[-TestIndices,]


attach(train) 

## Looking at predicting positive count vs. perfect attendance
## Binarize response variable for prediction
BIN_dat<-all.data
BIN_dat[BIN_dat$daysabs>0,]$daysabs<-1
trainBIN<-BIN_dat[-TestIndices,] 
testBIN<-BIN_dat[TestIndices,] 


##### Logistic regression on binarized data.  GOAL: Predict Perfect attendance ###
logistic.out<-glm(daysabs~mathnce+langnce+gender+ethnic+nateng+school, family=binomial(logit), data=trainBIN)

summary(logistic.out)  # Note: variables lose significance in binarized data


library(randomForest)
library(pscl) # for generalized count models
library(rpart) 
library(VGAM) 


#######################################################
###############      Models     #######################
#######################################################
####  Trained on training set of 216 observations (100 held out)

# Poisson regression model ()
poiss.out <- glm(daysabs~mathnce+langnce+gender+ethnic+nateng+school, family="poisson", data=train)

# Zero inflated poisson regression (ZIP) model
# Note syntax difference using or | operator - specifies covariates for both 
zip.out <- zeroinfl(daysabs~mathnce+langnce+gender+ethnic+nateng+school|
                      mathnce+langnce+gender+ethnic+nateng+school, data = train)

# Zero inflated negative binomial model
# NOTE: not using all variables because of lack of convergence 
zinb.out <- zeroinfl(daysabs~langnce+mathnce|
                       langnce+mathnce, data = train, dist = "negbin")

# Hurdle model fit by ML
hurdle.out<-hurdle(daysabs~mathnce+langnce+gender+ethnic+nateng+school|
                     mathnce+langnce+gender+ethnic+nateng+school, data = train, dist = "negbin")

# Negative Binomial model (NB2)
negbin.out <- glm.nb(daysabs ~mathnce+langnce+gender+ethnic+nateng+school, data = train)

# Quassi Poisson model
quasipoiss.out<-glm(daysabs~ mathnce+langnce+gender+ethnic+nateng+school, 
                    family="quasipoisson"(link="log"), data=train)

# Linear model - to show that the assumptions do not hold
linear.out<-lm(daysabs ~mathnce+langnce+gender+ethnic+nateng+school, data = train)

#   Random Forest  # mtry (hyper parameter) chosen by comparison below
library(randomForest)
rf.out<-randomForest(daysabs ~ mathnce + langnce + gender + ethnic +nateng+school,mtry=4,data = train)


######################  hyper parameter for random forest chosen by
######################  plotting mean square by mtry for out of bag estimate and test error
oob.err=double(6)
test.err=double(6)
for (mtry in 2:6){
  fit<-randomForest(daysabs ~ mathnce + langnce + gender + ethnic +nateng+school,mtry=mtry,data = train)
  oob.err[mtry]=fit$mse[400]
  pred = predict(fit,test)
  test.err[mtry]=with(test,mean((daysabs-pred)^2))
  cat(mtry," ")
}

matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topleft",legend = c("OOB","Test"),pch = 19,col=c("red","blue"))
##  Fairly arbitrary choice of mtry due to graph being relatively flat

importance(rf.out,type = 1) # shows variable importance 

# Tobit model   (like a censored regression)
tobit.out<- vglm(daysabs ~mathnce+langnce+gender+ethnic+nateng+school, tobit(Lower=0),data=train)

################# linear diagnostics/ Collinearity check ##############

plot(linear.out) # Note: Need to press enter in console to browse through graphs

#install.packages("car")
library(car)

# Assessing Outliers (Note: no samples were thrown out since we did not want to bias our models)
outlierTest(linear.out) # Bonferonni p-value for most extreme obs
qqPlot(linear.out, main="QQ Plot") #qq plot for studentized resid 

plot(linear.out) # residual plot especially important 

#######################################
# Evaluate homoscedasticity assumption
# Breusch-Pagan test: tests whether the estimated variance of residuals
# depends on the values of the independent variables
ncvTest(linear.out)  # We can see non-constant variance (heteroskedasticity)

###############################################################
#install.packages("flexmix")
#library(flexmix)
#
# Not using in analysis
#
# ### Two-class Finite Mixture Model (both Poisson) 
# class <- FLXPmultinom(~ ~mathnce+langnce+gender+ethnic+nateng+school)
# formula <- "daysabs ~mathnce+langnce+gender+ethnic+nateng+school"
# control <- list(verbose = 10, iter.max = 500, minprior = 0.1, tol = 0.01)
# 
# mix.out <- flexmix(as.formula(formula), data = train, k = 2, model =  FLXMRglm(family = "poisson"), concomitant = class, control = control)
# summary(mix.out)
# refit1 <- refit(mix.out, method = 'optim')
# mix.out
# # model the response
# summary(refit1, which = 'model')
# # model the mixture distribution 
# summary(refit1, which = 'concomitant') 
# mix.out
# plot(mix.out)
# mean((test$daysabs-predict(mix.out, test,type = "resp")$Comp.1)^2)
####################################################################

### Dispersion Test  (based on likelihood ratio)
odTest(negbin.out,alpha=0.05)

mean(train$daysabs)  # Can also see from comparing mean and variance of response
var(train$daysabs)

#### Model summaries
summary(poiss.out)
summary(quasipoiss.out)
summary(zinb.out)
summary(zip.out)
summary(hurdle.out)
summary(rf.out)
summary(negbin.out)
summary(linear.out)
summary(logistic.out)
#summary(mix.out)

####### AIC COMPARISONS #####
# NOTE: quassi Poisson doesn't have a standard likelihood (so no AIC reported)

AIC(poiss.out,negbin.out,zip.out,zinb.out,hurdle.out) 

####################
### Coefficients  ## 
####################
# Note: when interpetting Poisson/negbinom coeffs, usually exponentiate
linear<-linear.out$coefficients
Poiss<-poiss.out$coefficients
QPoiss<-quasipoiss.out$coefficients
NegBin<-negbin.out$coefficients
ZIPsampling<-zip.out$coefficients$count
ZIPstruct<-zip.out$coefficients$zero
Hurdlesampling<-hurdle.out$coefficients$count
Hurdlestruct<-hurdle.out$coefficients$zero
logistic<-logistic.out$coefficients

Coefs<-round(cbind(linear,Poiss,NegBin,ZIPsampling,ZIPstruct),digits = 2)
Coefs2<-round(cbind(Hurdlesampling,Hurdlestruct),digits = 2)


rownames(Coefs)<-c("Intercept","Math","Language","Gender-male","Ethnic-Asian","Ethnic-Afr","Ethnic-Hisp","Ethnic-White","Native-English","School2")
rownames(Coefs2)<-c("Intercept","Math","Language","Gender-male","Ethnic-Asian","Ethnic-Afr","Ethnic-Hisp","Ethnic-White","Native-English","School2")

Coefs       
Coefs2

zinb.out$coefficients  # Displayed seperately since different covariates were used

################################################################
###################  Zero count prediction  ####################
################################################################

###### Poisson #########
summary(poiss.out)  # Note: Many significant variables
beta=coef(poiss.out)
vcov=vcov(poiss.out)

plot(daysabs,(train$daysabs-predict(poiss.out,type = "response"))^2)#poorer prediction with larger daysabs true value

mean((train$daysabs-predict(poiss.out,type ="response"))^2) 

sum(daysabs==0)
sum(dpois(0,fitted(poiss.out))) # comparison of expected # zeros to actual


########   ZIP   ###########
summary(zip.out)
mean(zip.out$residuals^2)
mean((train$daysabs-predict(zip.out,type = "resp"))^2) # should be same as above
plot(daysabs,(train$daysabs-predict(zip.out))^2) #poor prediction with larger counts

sum(predict(zip.out, type="prob")[,1])  # predicted number of zeros

## Vuong tests have been used for nested/non nested models(some controversy)
## See Cameron & Trivedi's Count Models (report bibliography)

vuong(zip.out,poiss.out)    # significant
vuong(zinb.out,negbin.out)  # Not significant

#### Zero-Inflated Negative Binomial (ZINB)  ####
# Removed covariates gender and school otherwise convergence issues
summary(zinb.out)
mean(zinb.out$residuals^2)
mean((train$daysabs-predict(zinb.out,train,type = "resp"))^2) 
plot(daysabs,(train$daysabs-predict(zinb.out))^2)#poor prediction with larger counts


#### Hurdle Model ####
summary(hurdle.out)
mean(hurdle.out$residuals^2)
mean((train$daysabs-predict(hurdle.out,type = "response"))^2) 
plot(daysabs,(train$daysabs-predict(hurdle.out))^2)

sum(predict(hurdle.out, type="prob")[,1])  # predicted number of zeros

#### Negative Binomial  ####
summary(negbin.out) 
sum(dnbinom(0,mu=fitted(negbin.out),size=negbin.out$theta))  

mean(resid(negbin.out,type="resp")^2)
mean((train$daysabs-predict(negbin.out,type="resp"))^2)  
plot(daysabs,(train$daysabs-predict(negbin.out,type="resp"))^2)#poorer prediction with larger daysabs true value

####QUASI-POISSON####
summary(quasipoiss.out)

mean((train$daysabs-predict(quasipoiss.out,train,type = "resp"))^2)  
plot(daysabs,(train$daysabs-predict(quasipoiss.out,train,type = "resp"))^2)#poorer prediction with larger daysabs true value
sum(dpois(0,fitted(quasipoiss.out))) # same as Poisson in terms of # zeros predicted



############################## Train MSE #############
tmse.ZIP<-mean((train$daysabs-predict(zip.out, train,type = "resp"))^2)
tmse.HURDLE<-mean((train$daysabs-predict(hurdle.out, train,type = "resp"))^2)
tmse.POISS<-mean((train$daysabs-predict(poiss.out, train,type = "resp"))^2)
tmse.NB2<-mean((train$daysabs-predict(negbin.out, train,type = "resp"))^2)
tmse.QPOISS<-mean((train$daysabs-predict(quasipoiss.out, train,type = "resp"))^2)
tmse.rf<-mean((train$daysabs-predict(rf.out, train,type = "resp"))^2)
tmse.ZINB<-mean((train$daysabs-predict(zinb.out, train,type = "resp"))^2)
tmse.Linear<-mean((train$daysabs-predict(linear.out, train,type = "resp"))^2)
tmse.TOBIT<-mean((train$daysabs-predict(tobit.out, train,type = "resp"))^2)

tMSE<-c(tmse.ZIP,tmse.HURDLE,tmse.POISS,tmse.NB2,tmse.QPOISS,tmse.rf,tmse.ZINB,tmse.Linear,tmse.TOBIT)
names(tMSE)<-c("ZIP","Hurdle","Poisson","NB2","Quassi-Poisson","Random Forest","ZINB","Linear","Tobit")
sort(tMSE)    # Note how Random Forest Overfits 


############################## TEST MSE #############
mse.ZIP<-mean((test$daysabs-predict(zip.out, test,type = "resp"))^2)
mse.HURDLE<-mean((test$daysabs-predict(hurdle.out, test,type = "resp"))^2)
mse.POISS<-mean((test$daysabs-predict(poiss.out, test,type = "resp"))^2)
mse.NB2<-mean((test$daysabs-predict(negbin.out, test,type = "resp"))^2)
mse.QPOISS<-mean((test$daysabs-predict(quasipoiss.out, test,type = "resp"))^2)
mse.rf<-mean((test$daysabs-predict(rf.out, test,type = "resp"))^2)
mse.ZINB<-mean((test$daysabs-predict(zinb.out, test,type = "resp"))^2)
mse.Linear<-mean((test$daysabs-predict(linear.out, test,type = "resp"))^2)
mse.TOBIT<-mean((test$daysabs-predict(tobit.out, test,type = "resp"))^2)

MSE<-c(mse.ZIP,mse.HURDLE,mse.POISS,mse.NB2,mse.QPOISS,mse.rf,mse.ZINB,mse.Linear,mse.TOBIT)
names(MSE)<-c("ZIP","Hurdle","Poisson","NB2","Quassi-Poisson","Random Forest","ZINB","Linear","Tobit")
sort(MSE)  




############################## TEST Med. Abs. Err #############
med.ZIP<-median(abs(test$daysabs-predict(zip.out, test,type = "resp")))
med.HURDLE<-median(abs(test$daysabs-predict(hurdle.out, test,type = "resp")))
med.POISS<-median(abs(test$daysabs-predict(poiss.out, test,type = "resp")))
med.NB2<-median(abs(test$daysabs-predict(negbin.out, test,type = "resp")))
med.QPOISS<-median(abs(test$daysabs-predict(quasipoiss.out, test,type = "resp")))
med.rf<-median(abs(test$daysabs-predict(rf.out, test,type = "resp")))
med.ZINB<-median(abs(test$daysabs-predict(zinb.out, test,type = "resp")))
med.Linear<-median(abs(test$daysabs-predict(linear.out, test,type = "resp")))
med.TOBIT<-median(abs(test$daysabs-predict(tobit.out, test,type = "resp")))

MED<-c(med.ZIP,med.HURDLE,med.POISS,med.NB2,med.QPOISS,med.rf,med.ZINB,med.Linear,med.TOBIT)
names(MED)<-c("ZIP","Hurdle","Poisson","NB2","Quassi-Poisson","Random Forest","ZINB","Linear","Tobit")
sort(MED)


sort(MED)
sort(MSE)

##### MSE using Fitted values as BAYES estimates #############
bayes.est.zip<-numeric()
bayes.est.hurdle<-numeric()
bayes.est.poiss<-numeric()
bayes.est.nb2<-numeric()
bayes.est.zinb<-numeric()

for (i in 1:length(test$daysabs)){
  bayes.est.zip[i]   <-which.max(predict(zip.out, test, type="prob")[i,])-1#b/c first column is for zero. need to add one
  bayes.est.hurdle[i]<-which.max(predict(hurdle.out,test, type="prob")[i,])-1
  bayes.est.zinb[i]<-which.max(predict(zinb.out,test, type="prob")[i,])-1
  bayes.est.poiss[i]<-which.max(dpois(0:100,predict(poiss.out,test,type="res")[i]))-1
  bayes.est.nb2[i]<-which.max(dnbinom(0:100, mu=predict(negbin.out,test,type="resp")[i], size=negbin.out$theta))-1
  }

#hurdle bayes estimates are 0 or 1 (try  plot(predict(hurdle.out,test, type="prob")[3,]))
#nb2 bayes estimates are all 0 due to the shape of the distribution and dispersion param being 1.
#poiss is non zeros, and zip is zeros mixed with non zeros.

#OPTION 1 MEAN ABS ERR USING BAYES ESTIMATE
bayeserr.ZIP<-mean(abs(test$daysabs-bayes.est.zip))
bayeserr.HURDLE<-mean(abs(test$daysabs-bayes.est.hurdle))
bayeserr.ZINB<-mean(abs(test$daysabs-bayes.est.zinb))
bayeserr.POISS<-mean(abs(test$daysabs- bayes.est.poiss))
bayeserr.NB2<-mean(abs(test$daysabs-bayes.est.nb2))

#OPTION 2 MEAN SQUARED ERROR USING BAYES ESTIMATE
bayeserr.ZIP<-mean((test$daysabs-bayes.est.zip)^2)
bayeserr.HURDLE<-mean((test$daysabs-bayes.est.hurdle)^2)
bayeserr.ZINB<-mean((test$daysabs-bayes.est.zinb)^2)
bayeserr.POISS<-mean((test$daysabs- bayes.est.poiss)^2)
bayeserr.NB2<-mean((test$daysabs-bayes.est.nb2)^2)

#OPTION 3 MEDIAN ABS ERR USING BAYES ESTIMATE
bayeserr.ZIP<-median(abs(test$daysabs-bayes.est.zip))
bayeserr.HURDLE<-median(abs(test$daysabs-bayes.est.hurdle))
bayeserr.ZINB<-median(abs(test$daysabs-bayes.est.zinb))
bayeserr.POISS<-median(abs(test$daysabs- bayes.est.poiss))
bayeserr.NB2<-median(abs(test$daysabs-bayes.est.nb2))

#RUN EITHER OPTION 1, 2 or 3 AND THEN RUN THE FOLLOWING
BAYESERR<-c(bayeserr.ZIP,bayeserr.HURDLE,bayeserr.ZINB, bayeserr.POISS,bayeserr.NB2)
names(BAYESERR)<-c("ZIP","Hurdle","ZINB","Poisson","NB2")
sort(BAYESERR)


########################################
##########      Plots     ##############
########################################

#Absolute errors vs. true values (Poisson, ZIP Hurdle)
plot(test$daysabs,abs(test$daysabs-predict(poiss.out,test,type = "resp")),ylab="Abs Error", main="Pred Errs by True Outcome")#poorer prediction with larger daysabs true value
points(test$daysabs,abs(test$daysabs-predict(zip.out,test,type = "resp")),col="Red",pch=3)
points(test$daysabs,abs(test$daysabs-predict(hurdle.out,test,type = "resp")),col="Blue",pch=4) 
legend(x=5,y=30, pch=c(1,3,4),c("Poisson","ZIP","Hurdle"),cex = .7,lwd=c(1,1),col=c("black","red","Blue"))


#Absolute errors vs. true values (Neg Bin, ZINB)
plot(test$daysabs,abs(test$daysabs-predict(negbin.out,test,type = "resp")),col=" green",pch=5, ylab="Abs Error", main="Pred Errs by True Outcome")#poorer prediction with larger daysabs true value
points(test$daysabs,abs(test$daysabs-predict(zinb.out,test,type = "resp")),col="purple",pch=6)
points(test$daysabs,abs(test$daysabs-predict(zip.out,test,type = "resp")),col="Red",pch=3) #just for reference
legend(x=5,y=30, pch=c(5,6,3),c("Neg Binomial","ZINB", "ZIP"),cex = .7,lwd=c(1,1),col=c(" green","purple","red"))

#predicted vs true value. (Poisson, ZIP Hurdle) Note: symbols to be close to 45 degree line are better predictions
plot(test$daysabs,predict(poiss.out,test,type = "resp"),ylim=c(0,20),ylab="pred value", main="Pred value by True value ")#poorer prediction with larger daysabs true value
points(test$daysabs,predict(zip.out,test,type = "resp"),col="Red",pch=3) 
points(test$daysabs,predict(hurdle.out,test,type = "resp"),col="blue",pch=4) 
abline(0, 1)#45 degree line. Bad prediction for high counts
legend(x=30,y=15, pch=c(1,3),c("Poisson","ZIP","Hurdle"),cex = .7,lwd=c(1,1),col=c("black","red","blue"))

#predicted vs true value. (Neg Bin, ZINB)
plot(test$daysabs,predict(negbin.out,test,type = "resp"),col=" green",pch=5, ylim=c(0,20),ylab="pred value", main="Pred value by True value ")#poorer prediction with larger daysabs true value
points(test$daysabs,predict(zinb.out,test,type = "resp"),col="purple",pch=6) 
abline(0, 1)#45 degree line. Bad prediction for high counts
legend(x=30,y=15, pch=c(5,6),c("Neg Bin","ZINB"),cex = .7,lwd=c(1,1),col=c("green","purple"))



#The next 13 lines is to plot predicted probability of true value, against true value
probtrue.zip<-rep(0,length(test$daysabs))
probtrue.poiss<-rep(0,length(test$daysabs))
probtrue.hurdle<-rep(0,length(test$daysabs))
probtrue.negbin<-rep(0,length(test$daysabs))
probtrue.zinb<-rep(0,length(test$daysabs))

for (i in 1:length(test$daysabs)){
  probtrue.zip[i]<-predict(zip.out, test, type="prob")[i,(test$daysabs[i]+1)]#b/c first column is for zero. need to add one
  probtrue.poiss[i]<-dpois(test$daysabs[i],predict(poiss.out,test,type="resp")[i])
  probtrue.hurdle[i]<-predict(hurdle.out, test, type="prob")[i,(test$daysabs[i]+1)]#b/c first column is for zero. need to add one
  probtrue.zinb[i]<-predict(zinb.out, test, type="prob")[i,(test$daysabs[i]+1)]#b/c first column is for zero. need to add one
  probtrue.negbin[i]<-dnbinom(test$daysabs[i], mu=predict(negbin.out,test,type="resp")[i], size=negbin.out$theta)
  }

plot(test$daysabs,probtrue.poiss,ylim=c(0,0.5),ylab="predicted prob", main="Pred prob by True Outcome")
points(test$daysabs,probtrue.zip,col="Red",pch=1) #poorer prediction with larger daysabs true value
legend(x=20,y=0.3, pch=c(1,1),c("Poisson","ZIP"),cex = .7,lwd=c(1,1),col=c("black","red"))

#poiss vs zip, average predicted probability of true outcome vs true outcome
plot(sort(unique(test$daysabs)),y=tapply(probtrue.zip, test$daysabs, mean),cex=1.3,type="b",col="red",ylim=c(0,0.27),ylab="Predicted Prob.",xlab="True Outcome", main="Avg. Predicted Prob against True Outcome")
lines(sort(unique(test$daysabs)),y=tapply(probtrue.poiss, test$daysabs, mean),cex=1.3,col="blue",type="b") 
legend(x=30,y=0.25, pch=c(1,1),c("ZIP","Poisson"),cex = .8,lwd=c(1,1),col=c("red","blue"))

#poiss vs others
plot(sort(unique(test$daysabs)),y=tapply(probtrue.poiss, test$daysabs, mean),cex=1.3,type="b",col="blue",ylim=c(0,0.27),ylab="Predicted Prob.",xlab="True Outcome", main="Avg. Predicted Prob against True Outcome")
lines(sort(unique(test$daysabs)),y=tapply(probtrue.hurdle, test$daysabs, mean),cex=1.3,col="green",type="b")
lines(sort(unique(test$daysabs)),y=tapply(probtrue.zinb, test$daysabs, mean),cex=1.3,col="purple",type="b") 
lines(sort(unique(test$daysabs)),y=tapply(probtrue.negbin, test$daysabs, mean),cex=1.3,col="black",type="b")
legend(x=30,y=0.25, pch=c(1,1),c("Poisson","Hurdle","ZINB","Neg. Bin."),cex = .8,lwd=c(1,1),col=c("blue","green","purple","black"))



#########################
##### Misc. Plotting ####
#########################

library(ggplot2)
ggplot(train,aes(daysabs, fill = gender))+
geom_histogram(binwidth = 0.5,position = "dodge")+ # by gender
ggtitle("Absence Count by Gender")

ggplot(train,aes(daysabs, fill = school))+
  geom_histogram(binwidth = 0.5,position = "dodge")+ # by gender
  ggtitle("Absence Count by School")

ggplot(train,aes(daysabs, fill = ethnic))+ geom_histogram(bins=50) # by school

############### Plot of Poisson Predicted vs True Absences
dfa<-data.frame(cond = factor(rep(c("Poisson (5.375)","True"), each=216)), 
           Absences = c(rpois(lambda=5.375,n=216),train$daysabs))
ggplot(dfa, aes(x=Absences, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity") + ggtitle("Poisson Predicted Count vs True Count")


####  Other observations  ####
sum(daysabs[gender=="female"]) # girls/boys absences 
sum(daysabs[gender=="male"])

sum(daysabs[school=="1"]==0) # zeros of school 1/2
sum(daysabs[school=="2"]==0) 

numStudents1<-dim(train[school=="1",])[1] 
numStudents2<-dim(train[school=="2",])[1] 

table(ethnic)



############################
## plot for presentation  ##
############################

absVSpoiss<-data.frame(Absences = train$daysabs, Poisson = rpois(n = length(train$daysabs),lambda = mean(train$daysabs)))
mean(absVSpoiss$Absences)


hist(train$daysabs,breaks = 70,col=rgb(0,0,1,0.8),xlab="Absences",main = "Poisson vs Actual Count")
hist(absVSpoiss$Poisson,add=TRUE,col=rgb(1,0,0,0.7),breaks=30)
legend("topright", fill=c("blue", "red"),legend = c("Actual", "Poisson(5.38)"))
       