#######################################################
#
#  Preregistration for "Joint Interventions at Scale"
#
#######################################################

setwd("~/git/Joint-Interventions-At-Scale/")
require(lme4)
require(dplyr)

load("simdat.rda")

#######################################################
#### Analytical Strategy ####
#######################################################
#
# The data are collected from a series of treatments embedded in a single procedure that was 
# replicated in many courses, across several schools. Additionally, assignment to treatment
# was stratified across four covariates collected early in the survey. 
# 
# In every model we control for strata covariates; and nesting within strata, course and school
course.strata<-" + intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)"
#
#### Variable Names ####
# Stratification: strata {strata_intent_assess, strata_hours, strata_crs_finish, strata_educ}
# Additional Nesting: school, course
### Regression Models ##
# only estimates the background covariates/nesting
models[["baseline"]] = "1"     
#
#######################################################
#### Randomized Treatments ####
#######################################################
#
# Students were randomized into cells in a 2x3 between-subjects design.
#
# Factor 1: Affirmation    (affirmed / control)
# Factor 2: Plan-Making    (long plans / short plans / control)
#
# Randomization was done within strata (defined by covariates above)
#
#### Variable Names ####
# Condition Indicators: affirm, plans_long, plans_short
### Regression Models ##
# simple treatment effects
models[["simple"]] = "affirm * (plans_long + plans_short) " 
#######################################################
#### Other Covariates ####
#######################################################
#### Affirmation effect by HDI region
models[["affirmHDI"]] = "affirm * highHDI + (plans_long + plans_short)" 
#### Plans effect by language (original analysis plan)
models[["plans"]] = "affirm + (plans_long + plans_short) * fluent"  
#### Plans effect by school (based on bottom-up search for moderators)
models[["plans"]] = "affirm + (plans_long + plans_short) * school"  

#######################################################
#### Outcomes ####
#######################################################
# Binary Outcomes: cert_verified, cert_basic, upgrade_verified, subsequent_enroll
# Percentage Outcomes: course_progress, likely_complete_1

outcome<-"cert_verified"


data$Y<-data[,outcome]

models<-list()

fit.glmer = glmer(formula(paste("Y ~ ", models[["simple"]], course.strata)),
                  data = data, # specifying Y
                  family = "binomial",
                  nAGQ = 0, 
                  control = glmerControl(optimizer = "nloptwrap"))
summary(fit.glmer)
# For continous Y
fit.lmer = lmer(models[["simple"]], data = data)
summary(fit.lmer)

#######################################################
### Additional planned analyses for Affirmation ###
#######################################################
# Affirmation and HDI: effect for low/high HDI, and by 5 bins
models[["affirm.hdi2"]] = "Y ~ affirm * highHDI + (plans_long + plans_short)"
models[["affirm.hdi5"]] = "Y ~ affirm * cut_number(HDI, 5) + (plans_long + plans_short)"

# Affirmation and HDI based on country social identity threat and language
models[["affirm.csit"]] = "Y ~ affirm * (HDI + scale(threat_country)) + (plans_long + plans_short)"
models[["affirm.lang"]] = "Y ~ affirm * (HDI + scale(fluent)) + (plans_long + plans_short)"

# Affirmation: effect for women/men
models[["affirm.sex"]] = "Y ~ affirm * sex + (plans_long + plans_short) "

# Affirmation: effect for low SES (based on parental education)
models[["affirm.sex"]]<-"Y ~ affirm * scale(10-educ_parents) + (plans_long + plans_short)"

# Affirmation: effect for US racial-ethnic minorities (majority = white/asian/non-hispanic)
models[["affirm.minority"]] = "Y ~ affirm * I(us_race%in%c(1,4) & us_ethnicity==1) + (plans_long + plans_short)"

#######################################################
### Planned analyses for Plan-Making ###
#######################################################