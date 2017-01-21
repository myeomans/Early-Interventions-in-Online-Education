setwd("~/git/Joint-Interventions-At-Scale/")
require(lme4)

load("simdat.rda")

#######################################################
# Model Specification
#######################################################
# Binary Outcomes: cert_verified, cert_basic, upgrade_verified, subsequent_enroll
# Percentage Outcomes: course_progress, likely_complete_1
# Condition Indicators: affirm, plans_long, plans_short
# Stratification: strata {strata_intent_assess, strata_hours, strata_crs_finish, strata_educ}
# Additional Nesting: school, course

models<-list()



#### Using mixed-effects model ####

# Overall main and interaction effects
models[["simple"]] = 
  Y ~ affirm * (plans_long + plans_short) + 
  intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)

# Affirmation effect by HDI region
models[["affirmHDI"]] = 
  Y ~ affirm * highHDI + (plans_long + plans_short) + 
  intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)

# Plans effect by school
models[["plans"]] = 
  Y ~ affirm + (plans_long + plans_short) * school +
  intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)

# Affirmation and plans effects by HDI region
models[["interaction"]] = 
  Y ~ affirm * highHDI * (plans_long + plans_short) + 
  intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)


# For binary Y
fit.glmer = glmer(models[["simple"]], 
              data = data, 
              family = "binomial",
              nAGQ = 0, 
              control = glmerControl(optimizer = "nloptwrap"))
summary(fit.glmer)

# For continous Y
fit.lmer = lmer(models[["simple"]], data = data)
summary(fit.lmer)


# For example, for certification
fit.glmer = glmer(models[["simple"]], 
                  data = data %>% mutate(Y=cert_verified), # specifying Y
                  family = "binomial",
                  nAGQ = 0, 
                  control = glmerControl(optimizer = "nloptwrap"))
summary(fit.glmer)


### Planned analyses for Affirmation ###
# Affirmation and HDI: effect for low/high HDI, and by 5 bins
models[["affirm.hdi2"]] = 
  Y ~ affirm * highHDI + (plans_long + plans_short) + 
  intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)

models[["affirm.hdi5"]] = 
  Y ~ affirm * cut_number(HDI, 5) + (plans_long + plans_short) + 
  intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)

# Affirmation and HDI based on country social identity threat and language
models[["affirm.csit"]] = 
  Y ~ affirm * (HDI + scale(threat_country)) + (plans_long + plans_short) + 
  intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)

models[["affirm.lang"]] = 
  Y ~ affirm * (HDI + scale(fluent)) + (plans_long + plans_short) + 
  intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)

# Affirmation: effect for women/men
models[["affirm.sex"]] = 
  Y ~ affirm * sex + (plans_long + plans_short) + 
  intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)

# Affirmation: effect for low SES (based on parental education)
models[["affirm.sex"]] = 
  Y ~ affirm * scale(10-educ_parents) + (plans_long + plans_short) + 
  intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)

# Affirmation: effect for US racial-ethnic minorities (majority = white/asian/non-hispanic)
models[["affirm.minority"]] = 
  Y ~ affirm * I(us_race%in%c(1,4) & us_ethnicity==1) + (plans_long + plans_short) + 
  intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)



# # Using fixed effects
# models[["glm.verified.simple"]] = cert_verified ~ affirm * highHDI + (plans_long + plans_short) * is_fluent + 
#   intent_assess + hours + crs_finish + educ + factor(school) + factor(course) + strata
# 
# fit.glm = glm(models[["glm.verified.simple"]], 
#             data = data, 
#             family = "binomial")
#             
# summary(fit.glm)

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