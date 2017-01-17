#######################################################
#
#  Preregistration for "Joint Interventions at Scale"
#  Analysis Plan
#######################################################

#setwd("~/git/Joint-Interventions-At-Scale/")
require(lme4)
require(dplyr)

load("simdat.rda")

#######################################################
# Sample Selection
#######################################################
#
#  Survey software records every student who started a pre-course survey. 
#  We impose a set of exclusion criteria to define our sample of interest.
# 
#  1. Student must have completed enough of the survey to be exposed to randomized condition
data$exposed.to.treat<-1*(data$webservice_call_complete == 1)&(!is.na(data$affirm))&(!is.na(data$plans))
#  2. Student must have started the course in the first 14 days
data$start.day<-(data$first_activity_timestamp-data$course_start_timestamp)/(60*60*24)
#  3. Student must have started the survey within the first hour of entering the course
data$survey.delay<-(data$survey_timestamp-data$first_activity_timestamp)/(60*60) 

data$itt.sample<-1*((data$exposed.to.treat==1)&(data$start.day<15)&(data$survey.delay<1))

samples<-list()
samples[["baseline"]] = (data$itt.sample==1)
#######################################################
#### Statistical Model ####
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
models<-list()
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
# Randomization was done within strata
# {strata_intent_assess, strata_hours, strata_crs_finish, strata_educ}
#
#### Variable Names ####
# Condition Indicators: affirm, plans_long, plans_short
### Regression Models ##
# simple treatment effects
models[["simple"]] = "affirm * (plans_long + plans_short) " 

#######################################################
### Planned analyses for Affirmation ###
#######################################################

# Affirmation and HDI: effect for low/high HDI, and by 5 bins
models[["affirm.hdi2"]] = " affirm * highHDI + (plans_long + plans_short)"
models[["affirm.hdi5"]] = " affirm * cut_number(HDI, 5) + (plans_long + plans_short)"

# Affirmation and HDI based on country social identity threat and language
models[["affirm.csit"]] = " affirm * (HDI + scale(threat_country)) + (plans_long + plans_short)"
models[["affirm.lang"]] = " affirm * (HDI + scale(fluent)) + (plans_long + plans_short)"

# Affirmation: effect for women/men
models[["affirm.sex"]] = " affirm * sex + (plans_long + plans_short) "

# Affirmation: effect for low SES (based on parental education)
models[["affirm.sex"]]<-" affirm * scale(10-educ_parents) + (plans_long + plans_short)"

# Affirmation: effect for US racial-ethnic minorities (majority = white/asian/non-hispanic)
models[["affirm.minority"]] = " affirm * I(us_race%in%c(1,4) & us_ethnicity==1) + (plans_long + plans_short)"


#######################################################
### Planned analyses for Plan-Making ###
#######################################################
data$plans_any<-1*(data$plans>0)
models[["plan.original"]]<-"affirm + (plans_long + plans_short)"  
models[["plan.vs.plan"]]<-"affirm + (plans_any + plans_long)"  
samples[["plan.original"]]<- samples[["baseline"]]&(data$is_fluent==1)&(data$intent_assess==4)
samples[["plan.vs.plan"]]<-samples[["plan.original"]]


#######################################################
#### Outcomes ####
#######################################################
# Binary Outcomes: cert_verified, cert_basic, upgrade_verified, subsequent_enroll
# Percentage Outcomes: course_progress, likely_complete_1




#######################################################
# Fitting the model
#######################################################
model.outcome<-"cert_basic"
model.name<-"simple"

##############################
if(model.name%in%names(samples)){
  model.data<-data[samples[[model.name]],]
}else{model.data<-data[samples[["baseline"]],]}

model.data$Y<-model.data[,model.outcome]
if(mean(data$Y%in%(0:1))==1){
  fit.glmer = glmer(formula(paste("Y ~ ", models[[model.name]], course.strata)),
                    data = model.data,
                    family = "binomial",
                    nAGQ = 0, 
                    control = glmerControl(optimizer = "nloptwrap"))
  summary(fit.glmer)
}else{
  fit.lmer = lmer(formula(paste("Y ~ ", models[[model.name]], course.strata)), 
                  data = model.data)
  summary(fit.lmer)
}
