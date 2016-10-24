require(lme4)

test.data<-data[(data$treated==1)&(data$start_day<32),]

models<-list()

models[["affirm"]]<-lmer(certified~affirm,data=test.data)
models[["plans"]]<-lmer(certified~plans,data=test.data)
models[["shortlong"]]<-lmer(certified~affirm*short.plans,data=test.data[test.data$plans==1,])

models[["all"]]<-lmer(certified~affirm*plans,data=test.data)

models[["all"]]<-lmer(certified~affirm*plans,data=test.data)