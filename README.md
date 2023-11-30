# R.-code for joint model
#LOESS SMOTHING PLOT#
library(sjPlot)
library(gplots)
#longitudinal analysis#
library(nlme)
library("lme4")
library(tcltk)
library(tkrplot)
library(lattice)
require(graphics)
library(ConvergenceConcepts)
#kaplan meier curve#
library(survival)
require(survival)
#joint model analysis code
# load packages 'JM' and 'lattice'
library("JM")
library("lattice")
library("MASS")
library("splines")
library("joineRML")
library(lattice)
library(ggfortify)
library(survminer)
library(jskm)
library(ggpubr)
library(foreign)
library(caret)
install.packages("caret")
library(caret)
library(haven)
chacha <-read_sav()
# function to produce scatteplots with superimposed smooth line
plotResid <- function (, survival, col.loess = "black", ...) {
 plot(chacha, survival, ...)
 lines(lowess(chacha, survival), col = col.loess, lwd = 2)
 abline(h = 0, lty = 3, col = "grey", lwd = 2)}
visittime=chacha$visittime
plot(lmm)
qqnorm()
qqline()
hist(MD, prob = TRUE,xlab ="", main = "")
x <- seq(, , length = 40)
f <- dnorm(x, mean = mean(MD), sd = sd(MD))
lines(x,f, col = "red", lwd = 2)

chacha<- within(chacha, {
  visittime <- factor(visittime)
  ID <- factor(ID)
})
intervals(lmm)
residuals <- resid(lmm)
summary(residuals)
hist(residuals)
plot(lmm)
#to plot the individual profile plot.
library(ggplot2)
library(methods)
require(ggplot2)
visittime=chacha$visittime
p <- ggplot(data = chacha, aes(x =visittime, y =MD,  group = ID,ylab = "MD",xlab = "Visit time in month",
                            scale=list(x=list(at=c(0,6,12,18,24,30,36,42)))))
p + geom_line()
boxplot(~visittime,data = ,xlab = "follow-up time in month", ylab ="",horizontal=F)
ggqqplot(chacha,'MD',facet.by='visittime',main="Normal Q-Q plot",xlab="Theoretical Quantiles",ylab="MD")
a<-var(MDe~visittime)
plot(a)
#LOESS SMOTHING PLOT#
library(sjPlot)
library(gplots)
loess(formula =MD~visittime,data=chacha)
scatter.smooth(MD~visittime,ylab = "Mean of MD",xlab = "Visit time in month",data=)
# FOR mean profile plot of each categorical var#

 ms=as.factor(chacha$ms)
ms=chacha$ms
interaction.plot(x.factor = visittime,trace.factor = ms,response =MD,xlab = "Follow-up time in months",col = 2:4,
                 lwd=3,fun = mean,type="b",
                 pch=c(19, 17), fixed=TRUE,leg.bty = "n",legend = TRUE)
residence=as.factor(chacha$residence)
residence=chacha$residence
interaction.plot(x.factor = visittime,trace.factor = residence,response =MD,xlab = "Follow-up time in month",ylab = "Mean of tumor size cm2",col = 2:4,
                 lwd=3,fun = mean,type="b",main="Mean of MD by residence",
                 pch=c(19, 17), fixed=TRUE,leg.bty = "n",legend = TRUE)
comorbid=as.factor(chacha$comorbid)
comorbid=chacha$comorbid
interaction.plot(x.factor = visittime,trace.factor = comorbid,response =MD,xlab = "Follow-up time in months",col = 2:4,
                 lwd=3,fun = mean,type="b",
                 pch=c(19, 17), fixed=TRUE,leg.bty = "n",legend = TRUE)
historyofabortion=as.factor(chacha$historyofabortion)
historyofabortion=chacha$historyofabortion
interaction.plot(x.factor = visittime,trace.factor = historyofabortion,response =MD,xlab = "Follow-up time in months",col = 2:4,
                 lwd=3,fun = mean,type="b",
                 pch=c(19, 17), fixed=TRUE,leg.bty = "n",legend = TRUE)
histologytype=as.factor(chacha$histologytype)
histologytype=chacha$histologytype
interaction.plot(x.factor = visittime,trace.factor = histologytype,response =MD,xlab = "Follow-up time in months",col = 2:4,
                 lwd=3,fun = mean,type="b",
                 pch=c(19, 17), fixed=TRUE,leg.bty = "n",legend = TRUE)

HIV=as.factor(chacha$HIV)
HIV=chacha$HIV
interaction.plot(x.factor = visittime,trace.factor = HIV,response =MD,xlab = "Follow-up time in months",col = 2:4,
                 lwd=3,fun = mean,type="b",
                 pch=c(19, 17), fixed=TRUE,leg.bty = "n",legend = TRUE)
smoking=as.factor(chacha$smoking)
smoking=chacha$smoking
interaction.plot(x.factor = visittime,trace.factor = smoking,response =MD,xlab = "Follow-up time in months",col = 2:4,
                 lwd=3,fun = mean,type="b",
                 pch=c(19, 17), fixed=TRUE,leg.bty = "n",legend = TRUE)
ocontraceptives=as.factor(chacha$ocontraceptives)
ocontraceptives=chacha$ocontraceptives
interaction.plot(x.factor = visittime,trace.factor = ocontraceptives,response =MD,xlab = "Follow-up time in month",ylab = "Mean of tumor size cm2",col = 2:4,
                 lwd=3,fun = mean,type="b",main="Mean of MD by oral contraceptives",
                 pch=c(19, 17), fixed=TRUE,leg.bty = "n",legend = TRUE)
stage=as.factor(chacha$stage)
stage=chacha$stage
interaction.plot(x.factor = visittime,trace.factor = stage,response =MD,xlab = "Follow-up time in month",ylab = "Mean of tumor size cm2",col = 2:4,
                 lwd=3,fun = mean,type="b",main="Mean of tumor size by stage of cancer",

                 pch=c(19, 17), fixed=TRUE,leg.bty = "n",legend = TRUE)
Treatment=as.factor(chacha$Treatment)
Treatment=chacha$Treatment
interaction.plot(x.factor = visittime,trace.factor = Treatment,response =MD,xlab = "Follow-up time in months",col = 2:4,
                 lwd=3,fun = mean,type="b",
                 pch=c(19, 17), fixed=TRUE,leg.bty = "n",legend = TRUE)
plot(lmm)
#longitudinal analysis#
library(nlme)
library(tcltk)
library(tkrplot)
library(lattice)
require(graphics)
library(ConvergenceConcepts)

#SELECTION OF COVARIANCE STRUCTURE#
UN= lme(MD~residence+age+educ+HIV+comorbid+historyofSTI+stage+smoking+ocontraceptives+histologytype+Treatment+weight+visittime,
            random = ~1 | ID,method = "ML",data=chacha, na.action=na.exclude)
summary(UN)
copsym=lme(MD~residence+comorbid+age+historyofSTI+HIV+ocontraceptives+stage+histologytype+educ+Treatment+weight+visittime,
            random = ~1|ID,correlation = corCompSymm(form =~ 1 | ID ),
            method = "ML",data=chacha,na.action=na.exclude)
summary(copsym)
ar= lme(MD~residence+comorbid+age+historyofSTI+HIV+ocontraceptives+stage+educ+histologytype+Treatment+weight+visittime,
            random = ~ 1 | ID,correlation = corAR1(form =~ 1 | ID ),
            method = "ML",data=chacha,na.action=na.exclude)        
summary(ar)
###toepliz
gt= lme(MD~residence+comorbid+age+historyofSTI+HIV+smoking+ocontraceptives+stage+educ+histologytype+Treatment+weight+visittime,
            random = ~ 1 | ID,correlation = corCAR1(form =~ 1 | ID ),
            method = "ML",data=chacha,na.action=na.exclude)        
summary(gt)

#selection of random effects#
#random intercept model
ri= lme(MD~1,
        random = ~ 1 | ID,correlation = corAR1(form =~ 1 | ID ),
        method = "ML",data=chacha)
summary(ri)
#random slope model
ris= lme(MD~visittime,
           random = ~visittime|ID,data=chacha, correlation =corAR1(form =~1|ID ),
           method = "ML",control=lmeControl(maxIter=1000,msMaxIter=1000,niterEM=1000,opt='optim'))
summary(ris)
#random intercept and random slope model
rs = lme(MD~residence+comorbid+age+historyofSTI+HIV+ocontraceptives+VIA+educ+pathologytype+Treatment+bweight+visittime,
             random = ~visittime|ID,correlation = corAR1(form =~1 |ID ),
             method = "ML", data=chacha,control=lmeControl(maxIter=1000,msMaxIter=1000,niterEM=1000,opt='optim'))
summary(rs)
#for model selection#
full= lme(MD~residence+comorbid+age+historyofSTI+HIV+smoking+ocontraceptives+stage+educ+histologytype+Treatment+weight+visittime,
         random = ~visittime | ID,
         correlation = corAR1(form =~ 1 | ID ),
     method = "ML",data=chacha,control=lmeControl(maxIter=1000,msMaxIter=1000,niterEM=1000,opt='optim'))
summary(full)
null= lme(MD~1,random = ~ 1 | ID,
          correlation = corAR1(form =~ visittime | ID ),
 method = "ML",data=chacha)
summary(null)
null= lme(tumorlength~1+(1|ID),random = ~ 1 | ID,
 method = "ML",data=chacha)
summary(null)

#fitting univariable lmm#
m=lme(MD~age+visittime ,data=chacha,random= ~ visittime| ID ,correlation = corAR1(form =~ 1 | ID ), method = "ML")
summary(m)
intervals(m)
y= lme(MD~as.factor(educ)+visittime , data=chacha,random= ~ visittime| ID ,correlation = corAR1(form =~ 1 | ID ), method = "ML",control=lmeControl(maxIter=1000,msMaxIter=1000,niterEM=1000,opt='optim'))     
summary(y)
intervals(y)
k= lme(MD~as.factor(residence)+visittime , data=chacha,random= ~ visittime| ID ,correlation = corAR1(form =~ 1 | ID ), method = "ML")     
summary(k)
intervals(k)
x=lme(MD~as.factor(comorbid)+visittime  , data=chacha,random= ~ visittime| ID , method = "ML")     
summary(x)
intervals(x)
z= lme(MD~as.factor(historyofSTI)+visittime , data=chacha,random= ~ visittime| ID , method = "ML")     
summary(z)
intervals(z)
v= lme(MD~(as.factor(HIV))*sinvisittime , data=chacha,random= ~ visittime| ID , method = "ML",na.action=na.exclude)     
summary(v)
intervals(v)
r= lme(MD~as.factor(smoking)+visittime , data=chacha,random= ~ visittime| ID , method = "ML")     
summary(r)
intervals(r)
t= lme(MD~as.factor(ocontraceptives)+visittime , data=chacha,random= ~ visittime| ID , method = "ML")     
summary(t)
intervals(t)
d= lme(MD~as.factor(VIA)+visittime , data=chacha,random= ~ visittime| ID , method = "ML")     
summary(d)
intervals(d)
f= lme(MD~as.factor(Treatment)+visittime , data=chacha,random= ~ visittime| ID , method = "ML")     
summary(f)
intervals(f)
aa= lme(MD~as.factor(pathologytype)+visittime , data=chacha,random= ~ visittime| ID , method = "ML")     
summary(aa)
intervals(aa)
g= lme(MD~bweight+visittime , data=chacha,random= ~ visittime| ID ,correlation = corAR1(form =~ 1 | ID ), method = "ML")     
summary(g)
intervals(g)
#multivariableanalysys#
lmm= lme(MD~(as.factor(Treatment)+weight+as.factor(histologytype)+as.factor(stage)+as.factor(HIV)+as.factor(ocontraceptives)+as.factor(comorbid)+as.factor(historyofSTI)+as.factor(smoking))*visittime,
random = ~visittime | ID,
          correlation = corAR1(form =~ visittime|ID ),method = "ML",data=chacha,na.action=na.omit,control=lmeControl(maxIter=1000,msMaxIter=1000,niterEM=1000,opt='optim'))
summary(lmm)
#survival analysis by survival data#
#kaplan meier curve#
library(survival)
require(survival)
library(ggplot2)
require(ggplot2)
library(dplyr)
#median survival time#
attach(survival)
library(ggplot2)
library(survival)
library(survminer)
ebolaSurv <- Surv(time = survival$time, event = survival$status)
ebolaKM <- survfit(ebolaSurv ~ 1, data = survival, type="kaplan-meier")
  ggsurvplot(ebolaKM, conf.int = TRUE,main="\n\n  KM curve for education of patient ", pval = FALSE, risk.table = FALSE, 
  legend = "none", censor.shape = "|", censor.size = 4, palette = c("firebrick"), 
  ylab = "Proportion surviving", xlab = "Time in month")
summary(ebolaKM, censored = FALSE)
####3median survival time
ebolaKM
survival$educ <- factor(survival$educ,
                     levels = c(0,1),
                     labels = c("litrate", "illitrate"))
survival$residence <- factor(survival$residence,
                        levels = c(0,1),
                        labels = c("rural","urban"))
survival$ms <- factor(survival$ms,
                 levels = c(0,1,2,3),
                 labels = c("maried","single","divorced","widowed"))
survival$comorbid <- factor(survival$comorbid,
                    levels = c(0,1),
                    labels = c("No", "Yes"))
survival$historyofabortion <- factor(survival$historyofabortion,
                    levels = c(0,1),
                    labels = c("No", "Yes"))
survival$HIV <- factor(survival$HIV,
                    levels = c(0,1),
                    labels = c("No", "Yes"))
survival$smoking <- factor(survival$smoking,
                    levels = c(0,1),
                    labels = c("No", "Yes"))
survival$histologytype <- factor(survival$histologytype ,
                    levels = c(0,1),
                    labels = c("Adenocarcinoma", "Squamous cell carcinoma"))
survival$ocontraceptives<- factor(survival$ocontraceptives,
                    levels = c(0,1),
                    labels = c("No", "Yes"))
survival$stage <- factor(survival$stage,
                    levels = c(1,0),
                    labels = c("early", "late"))
survival$stage<- factor(survival$stage,
                    levels = c(0,1,2),
                    labels = c("I", "II","III"))
survival$religion<- factor(survival$religion,
                    levels = c(0,1,2),
                    labels = c("orthodox", "muslim","other"))
km<-survfit(Surv(time,status,)~1)
km
#km estimate curve#
par(mfrow=c(1,1))
plot(km)
#educ
fit=survfit( Surv(time,status)~survival$educ)
summary(fit)
plot(fit,main="\n\n  KM curve for education of patient ",conf.int=FALSE,mark.time=TRUE,col=c("red","blue"),
     lty=c(2,1),xlab="Length of follow up in month",ylab="Survival probability")
legend("bottomleft",c("ilitrate","litrate"),lty=c(2,1),col=c("red","blue"),bty="n")
#comorbid
kmsurvival<-survfit(Surv(time,status)~comorbid,data = survival)
 summary(kmsurvival)
 plot(kmsurvival,main="\n\n  KM curve for comorbid disease ",conf.int=FALSE,mark.time=TRUE, col=c("black","red"),lty=1:2
,xlab = "Time(month)"
,ylab = "survival probability")
legend("topright",c("NO","Yes"),lty = 1:2,col = c("black","red"), bty= "n")

#historyofabortion
kmsurvival<-survfit(Surv(time,status)~historyofabortion,data = survival)
 summary(kmsurvival)
 plot(kmsurvival,main="\n\n  KM curve for history of abortion ",conf.int=FALSE,mark.time=TRUE, col=c("black","red"),lty=1:2
,xlab = "Time(month)"
,ylab = "survival probability")
legend("topright",c("NO","Yes"),lty = 1:2,col = c("black","red"), bty
= "n")
#residence
kmsurvival<-survfit(Surv(time,status)~residence,data = survival)
 summary(kmsurvival)
 plot(kmsurvival,main="\n\n  KM curve residence",conf.int=FALSE,mark.time=TRUE, col=c("black","red"),lty=1:2
,xlab = "Time(month)"
,ylab = "survival probability")
legend("topright",c("rural","urban"),lty = 1:2,col = c("black","red"), bty
= "n")

#HIV
kmsurvival<-survfit(Surv(time,status)~HIV,data = survival)
 summary(kmsurvival)
 plot(kmsurvival,main="\n\n  KM curve HIV",conf.int=FALSE,mark.time=TRUE, col=c("black","red"),lty=1:2
,xlab = "Time(month)"
,ylab = "survival probability")
legend("topright",c("No","Yes"),lty = 1:2,col = c("black","red"), bty
= "n")
#histologytype
kmsurvival<-survfit(Surv(time,status)~histologytype,data = survival)
 summary(kmsurvival)
 plot(kmsurvival,main="\n\n  KM curve histologytype",conf.int=FALSE,mark.time=TRUE, col=c("black","red"),lty=1:2
,xlab = "Time in month"
,ylab = "survival probability")
legend("topright",c("Adenocarcinoma","Squamous cell carcinoma"),lty = 1:2,col = c("black","red"), bty
= "n")

#Treatment
kmsurvival<-survfit(Surv(time,status)~Treatment,data = survival)
 summary(kmsurvival)
 plot(kmsurvival,main="\n\n  KM curve Treatment",conf.int=FALSE,mark.time=TRUE, col=c("black","red"),lty=1:2
,xlab = "Time(month)"
,ylab = "survival probability")
legend("topright",c("radiotherapy","Chemotherapy","Surgery"),lty = 1:2,col = c("black","red"), bty
= "n")

#nodestatus
kmsurvival<-survfit(Surv(time,status)~nodestatus,data = survival)
 summary(kmsurvival)
 plot(kmsurvival,main="\n\n  KM curve node status",conf.int=FALSE,mark.time=TRUE, col=c("black","red"),lty=1:2
,xlab = "Time(month)"
,ylab = "survival probability")
legend("topright",c("No","Yes"),lty = 1:2,col = c("black","red"), bty
= "n")
#stage
fit=survfit( Surv(time,status)~survival$stage)
fit
plot(fit,main="\n\n  KM curve for stage ",conf.int=FALSE,mark.time=TRUE,col=c("red","blue"),
     lty=c(2,1),xlab="Length of follow up in month",ylab="Survival probability")
legend("bottomleft",c("early","late"),lty=c(2,1),col=c("red","blue"),bty="n")

#ocontraceptives
kmsurvival<-survfit(Surv(time,status)~ocontraceptives,data = survival)
 summary(kmsurvival)
 plot(kmsurvival,main="\n\n  KM curve for oral contraceptives uses",conf.int=FALSE,mark.time=TRUE, col=c("black","red"),lty=1:2
,xlab = "Time(month)"
,ylab = "survival probability")
legend("topright",c("No","Yes"),lty = 1:2,col = c("black","red"), bty
= "n")

#Perform the log rank test
mysurv<-Surv(time,status)
mysurv
fit1<-survdiff(mysurv~Treatment,data=survival)
fit1
fit2<-survdiff(mysurv~religion)
fit2
fit3<-survdiff(mysurv~residence,data=survival)
fit3
fit4<-survdiff(mysurv~HIV,data=survival)
fit4
fit5<-survdiff(mysurv~stage,data=survival)
fit5
fit6<-survdiff(mysurv~ocontraceptives,data=survival)
fit6
fit7<-survdiff(mysurv~histologytype,data=survival)
fit7
fit8<-survdiff(mysurv~smoking,data=survival)
fit8
fit9<-survdiff(mysurv~comorbid,data=survival)
fit9
fit10<-survdiff(mysurv~historyofSTI,data=survival)
fit10
fit10<-survdiff(mysurv~educ,data=survival)
fit10
fit11<-survdiff(mysurv~ms,data=survival)
fit11
fit12<-survdiff(mysurv~age,data=survival)
fit12
fit13<-survdiff(mysurv~bweight,data=survival)
fit13
#univariable analysis of survival data#
#by chacha data#
b<- coxph(Surv(time, status) ~ as.factor(ms), data =survival, x= TRUE)
summary(b)
c<- coxph(Surv(time, status) ~ as.factor(residence), data = survival, x= TRUE)
summary(c)
d<- coxph(Surv(time, status) ~ as.factor(HIV), data =survival, x= TRUE)
summary(d)
e<- coxph(Surv(time, status) ~ as.factor(VIA), data =survival, x= TRUE)
summary(e)
g<- coxph(Surv(time, status) ~ as.factor(Treatment), data =survival, x= TRUE)
summary(g)
i<- coxph(Surv(time, status) ~(ocontraceptives), data =survival, x= TRUE)
summary(i)
j<- coxph(Surv(time, status) ~ as.factor(smoking), data =survival, x= TRUE)
summary(j)
k<- coxph(Surv(time, status) ~ as.factor(religion), data =survival, x= TRUE)
summary(k)
l<- coxph(Surv(time, status) ~ as.factor(historyofSTI), data =survival, x= TRUE)
summary(l)
m<- coxph(Surv(time, status) ~ as.factor(comorbid), data =survival, x= TRUE)
summary(m)
xx<- coxph(Surv(time, status) ~ as.factor(educ), data =survival, x= TRUE)
summary(xx)
uu<- coxph(Surv(time, status) ~ as.factor(pathologytype), data =survival, x= TRUE)
summary(uu)
n<- coxph(Surv(time, status) ~ age, data =survival, x= TRUE)
summary(n)
p<- coxph(Surv(time, status) ~ bweight, data =survival, x= TRUE)
summary(p)
#multivariate analysis of survival data#

survival<-chacha.ID<-chacha[!duplicated(chacha[c('ID')]), ]
survfit <-coxph(Surv(time, status) ~as.factor(educ)+as.factor(Treatment)+weight+as.factor(histologytype)+as.factor(stage)+as.factor(HIV)+as.factor(ocontraceptives)+as.factor(comorbid)+as.factor(historyofSTI)+as.factor(smoking),
                 data =survival, x=TRUE, model=TRUE)
summary(survfit)
w<-cox.zph(survfit)
par(mfrow=c(2,2))
plot(w)
#diagonosis for survival data#
#cox snell code#
cox.snell <- (as.numeric(survival$status) - 0) - resid(survfit,type = "martingale")
cox <- survfit(Surv(cox.snell, status) ~ 0, data =survival)
plot(cox,mark.time = FALSE,conf.int = TRUE,xlab = "Cox-Snell Residuals",ylab = "Survival Probability",
     main="Survival function of Cox-Snell Residuals")
curve(exp(-x),from = 0,to= max(time),add=TRUE, col="red",lwd=2)

ggcoxdiagnostics(survfit, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(survfit, type = "dfbetas",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(survfit, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
plot(survfit)
#joint model analysis code
library(MASS)
library(nlme)
library(splines)
library(survival)
library(JM)
library(caret)
library(lattice)
library(ggplot2)
library(joineRML)
jointFit <- jointModel(lmm,survfit, timeVar="visittime" , method = 'weibull-AFT-aGH')
summary(jointFit)
confint(jointFit,level=0.95)
plot(jointFit)
residuals <- resid(jointFit)
summary(residuals)
var(residuals)
hist(residuals)

#by joinRML#
set.seed(1)
fit=mjoint(formLongFixed=tumor~as.factor(comorbid)+as.factor(historyofSTI)+as.factor(HIV)+as.factor(Treatment)+as.factor(smoking)+as.factor(ocontraceptives)+as.factor(histologytype)+as.factor(stage)+weight+visittime,
     formLongRandom=~visittime|ID,
     formSurv=Surv(time,status)~as.factor(comorbid)+as.factor(historyofSTI)+as.factor(HIV)+as.factor(smoking)+as.factor(ocontraceptives)+as.factor(histologytype)+as.factor(stage)+weight,
      data=chacha,
       timeVar="visittime",
       control = list(nMCscale = 2, burnin = 5))
summary(fit) 
intervals(jointFit)
######### joint model diagnostics
par(mfrow = c(2, 2))
plot(jointFit)
#Residuals for the Longitudinal Part
# marginal residuals
resMargY.renal <- residuals(jointFit, process = "Longitudinal",type= "Marginal")
# marginal fitted values
fitMargY.renal <- fitted(jointFit, process = "Longitudinal",type = "Marginal")
# function to produce scatteplots with superimposed smooth line
plotResid <- function (chacha, survival, col.loess = "black", ...) {
 plot(chacha, survival, ...)
 lines(lowess(chacha, survival), col = col.loess, lwd = 2)
 abline(h = 0, lty = 3, col = "grey", lwd = 2)}
# scatteplot of marginal residuals vs marginal fitted values
plotResid(fitMargY.renal, resMargY.renal, xlab = "Fitted Values",
ylab = "Marginal Residuals", main="Marginal Residuals vs Fitted value")
vy <- residuals(jointFit, process = "Longitudinal",type= "stand-Subject")
xy <- fitted(jointFit, process = "Longitudinal",type = "Subject")
plot(xy,vy,xlab="Fitted value",ylab="Residuals",main="Subject-Specific Residual vs Fitted Values")


#Residuals for the Survival Part
# martingale residuals
martRes <- residuals(survfit,process = "Event")
# subject-specific fitted values for the longitudinal outcome
mi.t <- fitted(survfit, process = "Event",type = "EventTime")
# scatterplot of martingale residuals vs subject-specific fitted values
plotResid(mi.t, martRes, col.loess = "black",
 xlab = "Fitted Values",ylab = "Residuals",main="martingale Residuals vs FIteed value")
###jiont model diagnosics plot


#######Residuals for the Survival Part
# martingale residuals
martRes <- residuals(survfit,process = "Event")
# subject-specific fitted values for the longitudinal outcome
mi.t <- fitted(jointFit,process= "Longitudinal",type = "EventTime")
# scatterplot of martingale residuals vs subject-specific fitted values
plotResid(mi.t, martRes, col.loess = "black",
 xlab = "Fitted Values",ylab = "Residuals",main="martingale Residuals vs FIteed value")
