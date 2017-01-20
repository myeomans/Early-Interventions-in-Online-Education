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
#  Survey software records all learners who started the course survey.
#  We impose a set of exclusion criteria to define our sample of interest:
#  a. The learner must have completed enough of the survey to be randomized and exposed to condition
#  b. The learner must have started the course in the first 14 days (not applicable for self-paced courses)
#  c. The learner must have started the survey within the first hour of entering the course

data = mutate(data,
  exposed.to.treat = (webservice_call_complete == 1) & (!is.na(affirm)) & (!is.na(plans)), # a.
  start.day = (first_activity_timestamp - course_start_timestamp) / (60*60*24), # b.
  survey.delay = (survey_timestamp - first_activity_timestamp) / (60*60), # c.
  itt.sample = exposed.to.treat & (is_selfpaced | start.day < 15) & (survey.delay < 1)
)

samples = list()
samples[["baseline"]] = data$itt.sample # baseline (a., b., & c.)
samples[["exposed"]] = data$exposed.to.treat # all exposed (only a.)
samples[["HarvardMIT"]] = data$school %in% (1:2) # all Harvard/MIT courses

#######################################################
#### Stratified and Nested Design ####
#######################################################
#
# The data are collected from a series of treatments embedded in a single procedure that was 
# replicated in many courses, across several schools. Additionally, assignment to treatment
# was stratified across four covariates collected early in the survey. 
#
# Stratification variables: intent_assess, hours, crs_finish, educ
# Stratified: strata_intent_assess (2 levels), strata_hours (2), strata_crs_finish (3), strata_educ (3)
# Strata encoding variables: strata {strata_intent_assess, strata_hours, strata_crs_finish, strata_educ}
# Additional nesting variables: school, course
#
# In every model we control for strata covariates; and nesting within strata, course and school
strata = " + intent_assess + hours + crs_finish + educ + (1 | school/course) + (1 | strata)"

#######################################################
#### Randomized Treatments ####
#######################################################
#
# Learners were randomized into cells in a 2x3 between-subjects design.
#
# Factor 1: Affirmation    (affirm / control)
# Factor 2: Plan-Making    (long plans / short plans / control)
#
# Randomization was done within strata
# {strata_intent_assess, strata_hours, strata_crs_finish, strata_educ}
#
#### Variable Names ####
# Binary condition indicators: affirm, plans_long, plans_short

### Regression Models ###
models = list()
# only estimates the background covariates/nesting
models[["baseline"]] = "1"
# simple treatment effects
models[["simple"]] = "affirm * (plans_long + plans_short) " 

#######################################################
### Planned analyses for Affirmation ###
#######################################################

### Primary Analysis ###
# Replication: Affirmation effect for low vs. high HDI countries
# Expect: affirmation supports low-HDI learners
models[["affirm.hdi2"]] = " affirm * highHDI + (plans_long + plans_short)"

# Zooming in: Affirmation effect in four HDI bins
# Expect: affirmation supports low-HDI learners the most, followed by medium-HDI learners
models[["affirm.hdi4"]] = " affirm * HDI4 + (plans_long + plans_short)"

### Secondary Analysis - Theory-driven Extensions ###

# Affirmation effect by socioeconomic status (based on parental education)
# Expect: affirmation supports low-SES learners
models[["affirm.ses"]] = " affirm * scale(10-educ_parents) + (plans_long + plans_short)"

# Affirmation effect for US racial-ethnic minorities (majority = white/asian/non-hispanic)
# Expect: affirmation supports US minority learners
data$us_majority = !(data$us_race %in% c(1, 4) & data$us_ethnicity == 1)
models[["affirm.minority"]] = " affirm * us_majority + (plans_long + plans_short)"
samples[["affirm.minority"]] = !is.na(data$us_race) & !is.na(data$us_ethnicity) # US participants

# Affirmation effect moderated by HDI and individual-level country social identity threat
# Expect: affirmation supports learners in low HDI regions, especially those with high threat
models[["affirm.csit"]] = " affirm * scale(HDI) * scale(threat_country) + (plans_long + plans_short)"

# Affirmation effect moderated by HDI region and individual-level lanuage skill
# Expect: affirmation supports learners in low HDI regions, especially those whose English is not fluent 
models[["affirm.lang"]] = " affirm * highHDI * is_fluent + (plans_long + plans_short)"

# Affirmation effect by gender and course gender ratio
# Expect: affirmation supports learners of underrepresented (<33%) gender group
data = data %>% group_by(course) %>% mutate(course_prop_female = mean(sex == 2))
models[["affirm.sex"]] = " affirm * I(sex==2) * scale(course_prop_female) + (plans_long + plans_short) "


#######################################################
### Planned analyses for Plan-Making ###
#######################################################
data$plans_any = as.numeric(data$plans > 0)

models[["plan.original"]] = "affirm + (plans_long + plans_short)"  
models[["plan.vs.plan"]] = "affirm + (plans_any + plans_long)"
samples[["plan.original"]] = samples[["baseline"]] & (data$is_fluent==1) & (data$intent_assess==4)
samples[["plan.vs.plan"]] = samples[["plan.original"]]


#######################################################
#### Outcome Measures ####
#######################################################
# Primary outcome: cert_basic (binary)
# Primary outcomes (Harvard and MIT courses only): cert_verified (binary), upgrade_verified (binary)
# Secondary outcome: course_progress (percentage)
# Tertiary outcome: likely_complete_1 (percentage)


#######################################################
####  Generalized Model Fitting Function #### 
#######################################################
fit_model = function(model.name, model.outcome) {
  # Select relevant sample for model
  if (model.name %in% names(samples)){
    model.data = data[samples[[model.name]],]
  } else {
    model.data = data[samples[["baseline"]],]
  }
  
  # Set Y to be the outcome of interest
  model.data$Y = unlist(model.data[,model.outcome])
  
  # Estimate model parameters
  if (mean(model.data$Y %in% (0:1)) == 1){
    # for binary Y
    model.fit = glmer(formula(paste("Y ~ ", models[[model.name]], strata)),
                      data = model.data,
                      family = "binomial",
                      nAGQ = 0, 
                      control = glmerControl(optimizer = "nloptwrap"))
    
  } else {
    # for non-binary Y
    model.fit = lmer(formula(paste("Y ~ ", models[[model.name]], strata)), 
                    data = model.data)
  }
  
  summary(model.fit)
}

#######################################################
#### Fitting the model #### 
#######################################################


### Primary outcome cert_basic (binary) ###

fit_model(model.name = "simple", model.outcome = "cert_basic")

# Affirm Primary
fit_model(model.name = "affirm.hdi2", model.outcome = "cert_basic")
fit_model(model.name = "affirm.hdi4", model.outcome = "cert_basic")

# Affirm Secondary
fit_model(model.name = "affirm.ses", model.outcome = "cert_basic")
fit_model(model.name = "affirm.minority", model.outcome = "cert_basic")
fit_model(model.name = "affirm.csit", model.outcome = "cert_basic")
fit_model(model.name = "affirm.lang", model.outcome = "cert_basic")
fit_model(model.name = "affirm.sex", model.outcome = "cert_basic")

# Plans Primary

# Plans Secondary


### Secondary outcome course_progress (percentage) ###


### Tertiary outcome likely_complete_1 (percentage) ###
 

### MIT/Harvard only outcome cert_verified (binary) ###


# MIT/Harvard only outcome upgrade_verified (binary)

##############################