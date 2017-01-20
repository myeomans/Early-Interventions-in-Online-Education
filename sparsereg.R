#######################################################
#
#  Preregistration for "Joint Interventions at Scale"
#  Discovery of Heterogeneous Treatment Effects
#######################################################

#setwd("~/git/Joint-Interventions-At-Scale/")
require(sparsereg)

load("simdat.rda")

################################################
covariates<-c("course","verify.before","pre.enrol",
              "start.day","age","sex","USA","HDI",
              "bachelors","advanced",
              "work_fulltime","work_parttime",
              "in_school","school_course",
              "MOOC_enrol", "MOOC_finish")
treatments<-c("control","affirm","plans_short","plans_long")
HTEdata<-data[(data$prereg==1),c(covariates,treatments,"certified")]

################################################
HTEdata<-HTEdata[(rowMeans(is.na(HTEdata))==0),]
SPR<-sparsereg(y=HTEdata$certified, 
               X=as.matrix(HTEdata[,covariates]), 
               treat=HTEdata[,treatments],
               baseline.vec="control",
               type="probit",
               id="course", # is this right?
               EM=F,
               scale.type="TX")
summary(SPR) 

#round(SPR$beta.mean[1,],3)
#round(SPR$beta.mode[1,],3)
violinplot(SPR)
#coef(cv.glmnet(x=SPR$X,y=HTEdata$certified), s="lambda.1se")s

