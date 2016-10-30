require(lme4)

load("simdat.rda")

#######################################################
# Define subgroups and strata
#######################################################
data$is_fluent = as.numeric(data$fluent == 5)
data$highHDI = as.numeric(data$HDI > 0.7)

data$strata_intent_assess = ifelse(data$intent_assess>2, 1, 0)
data$strata_hours = ifelse(data$hours>5, 1, 0)
data$strata_crs_finish = ifelse(data$crs_finish>3, 2, ifelse(data$crs_finish>0, 1, 0))
data$strata_educ = ifelse(data$educ<4, 2, ifelse(data$educ==4, 1, 0))
data$strata = with(data, paste(strata_intent_assess, strata_hours, strata_crs_finish, strata_educ))

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