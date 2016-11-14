require(lme4)

load("simdat.rda")

#######################################################
# Model Specification
#######################################################
# Binary Outcomes: cert_verified, cert_basic, upgrade_verified, subsequent_enroll
# Percentage Outcomes: course_progress
# Condition Indicators: affirm, plans_long, plans_short
# Stratification: strata {strata_intent_assess, strata_hours, strata_crs_finish, strata_educ}
# Additional Nesting: school, course


models<-list()

# Using random effects
models[["glmer.verified.simple"]] = cert_verified ~ affirm * highHDI + (plans_long + plans_short) * is_fluent + 
                             (1 | school/course) + (1 | strata)

fit.glmer = glmer(models[["glmer.verified.simple"]], 
              data = data, 
              family = "binomial", 
              nAGQ = 0, 
              control = glmerControl(optimizer = "nloptwrap"))

summary(fit.glmer)

# Using fixed effects
models[["glm.verified.simple"]] = cert_verified ~ affirm * highHDI + (plans_long + plans_short) * is_fluent + 
  factor(school) + factor(course) + strata

fit.glm = glm(models[["glm.verified.simple"]], 
            data = data, 
            family = "binomial")
            
summary(fit.glm)

# # Full Sample
# models[["affirm"]]<-lmer(certified~affirm+(1|course),family="binomial",data=test.data)
# models[["affirm"]]<-lmer(certified~affirm+(1|course),family="binomial",data=test.data[low.dev,])
# # All Students
# models[["plans"]]<-lmer(certified~plans+(1|course),family="binomial",data=test.data[fluent,])
# models[["short.plans"]]<-lmer(certified~short.plans+(1|course),family="binomial",data=test.data[fluent,])
# models[["long.plans"]]<-lmer(certified~long.plans+(1|course),family="binomial",data=test.data[fluent,])
# models[["shortlong"]]<-lmer(certified~short.plans+(1|course),family="binomial",data=test.data[fluent&(test.data$plans==1),])
# 
# models[["all"]]<-lmer(certified~affirm*(short.plans+long.plans)+(1|course),family="binomial",data=test.data)