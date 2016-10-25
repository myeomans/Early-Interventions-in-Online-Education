require(lme4)

test.data<-data[(data$treated==1)&(data$start_day<32),]
fluent<-(test.data$fluency==5)
low.dev<-(test.data$HDI<0.7)

models<-list()

# Full Sample
models[["affirm"]]<-lmer(certified~affirm+(1|course),family="binomial",data=test.data)
models[["affirm"]]<-lmer(certified~affirm+(1|course),family="binomial",data=test.data[low.dev,])
# All Students
models[["plans"]]<-lmer(certified~plans+(1|course),family="binomial",data=test.data[fluent,])
models[["short.plans"]]<-lmer(certified~short.plans+(1|course),family="binomial",data=test.data[fluent,])
models[["long.plans"]]<-lmer(certified~long.plans+(1|course),family="binomial",data=test.data[fluent,])
models[["shortlong"]]<-lmer(certified~short.plans+(1|course),family="binomial",data=test.data[fluent&(test.data$plans==1),])

models[["all"]]<-lmer(certified~affirm*(short.plans+long.plans)+(1|course),family="binomial",data=test.data)