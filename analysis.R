#######################################################
###                                                 ###
#     "Early Interventions in Online Education"       #
#          Preregistration Analysis Plan              #
#                   02/01/2017                        #
###                                                 ###
#######################################################

#setwd("~/git/Joint-Interventions-At-Scale/")
require(lme4)
require(dplyr)

# Loading simulated dataset
load("simdat.rda")

#######################################################
###   Generalized Model Fitting Function            ### 
#######################################################
fit_model = function(model.name, model.outcome, sample = "baseline") {
  
  # Select relevant sample for model
  model.data = data[samples[[sample]],]
  
  # Set Y to be the outcome of interest
  model.data$Y = unlist(model.data[,model.outcome])
  
  # Estimate model parameters using maximum likelihood
  if (mean(model.data$Y %in% (0:1)) == 1){
    # generalized linear mixed-effects model (logistic) for for binary Y
    model.fit = glmer(formula(paste("Y ~", models[[model.name]], strata)),
                      data = model.data,
                      family = "binomial",
                      nAGQ = 0, 
                      control = glmerControl(optimizer = "nloptwrap"))
    
  } else {
    # linear mixed-effects model for non-binary Y
    model.fit = lmer(formula(paste("Y ~", models[[model.name]], strata)), 
                     data = model.data)
  }
  
  summary(model.fit)
}

#######################################################
###  Sample Selection                               ###
#######################################################
#
#  As we record all learners who started the course survey, we impose a set of 
#  exclusion criteria to define our sample of interest. The learner must have:
#  a. progressed far enough in the survey to be randomized and exposed to condition,
#  b. been exposed just once in the first 30 days following initial exposure,
#  c. started the survey within the first hour of their first timestamp in the course,
#  d1. started a cohort-based course in its first 14 days, OR
#  d2. started a self-paced course before the last 30 days
#
.analysis_timestamp = 1487145600 # e.g. 2/15/2017

data = ungroup(data %>%
  filter(webservice_call_complete == 1 & !is.na(affirm) & !is.na(plans)) %>% # a.
  group_by(id) %>% 
  arrange(survey_timestamp) %>% 
  mutate(
    first.exposure = row_number() == 1,
    num.exposures = n(),
    days.to.next.exposure = ifelse(num.exposures > 1, diff(survey_timestamp)[1] / (60*60*24), 0),
    clean.exposure = first.exposure & (num.exposures == 1 | days.to.next.exposure > 29), # for b.
    survey.delay = (survey_timestamp - first_activity_timestamp) / (60*60), # for c.
    days.from.start = (first_activity_timestamp - course_start_timestamp) / (60*60*24), # for d1.
    days.to.analysis = (.analysis_timestamp - first_activity_timestamp) / (60*60*24), # for d2.
    itt.sample = 
      clean.exposure & # b.
      (survey.delay < 1) & # c.
      ((course_selfpaced & (days.to.analysis > 29)) | # d1.
         ((!course_selfpaced) & (days.from.start < 15))) # d2.
))

samples = list()
samples[["baseline"]] = data$itt.sample # baseline

#######################################################
###  Stratified and Nested Design                   ###
#######################################################
#
# The data are collected from a series of treatments embedded in a single procedure that was 
# replicated in many courses, across several institutions. Additionally, assignment to treatment
# was stratified across four covariates collected early in the survey. 
#
# Stratification variables: intent_assess, hours, crs_finish, educ
# Stratified: strata_intent_assess (2 levels), strata_hours (2), strata_crs_finish (3), strata_educ (3)
# Strata encoding variables: strata {strata_intent_assess, strata_hours, strata_crs_finish, strata_educ}
# Additional nesting variables: institution, course
#
# In every model we control for strata covariates; and nesting within strata, course and institution
strata = "+ intent_assess + hours + crs_finish + educ + (1 | institution/course) + (1 | strata)"

#######################################################
### Randomized Treatments                           ###
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
#
### Regression Models ###
models = list()

# only estimates the background covariates/nesting
models[["baseline"]] = "1"

# simple treatment effects with interaction
models[["main.interaction"]] = "affirm * (plans_long + plans_short)" 

#######################################################
### Planned analyses for Affirmation                ###
#######################################################
#
### Primary Analysis - Replicating Prior Findings ###
#
# Affirmation effect in low vs. high HDI countries
# Hypothesis 1a. affirmation supports low-HDI learners (positive coef on affirm)
# Hypothesis 1b. gap between high vs. low HDI regions (positive coef on highHDI)
# Hypothesis 1c. affirmation doesn't support high-HDI learners (negative coef on affirm:highHDI)
models[["simple"]] = "affirm * highHDI + (plans_long + plans_short)"

### Secondary Analysis - Multiple Theory-driven Extensions ###
#
# Affirmation effect by socioeconomic status (based on parental education)
# Expect: affirmation supports low-SES learners
models[["affirm.ses"]] = "affirm * scale(educ_parents) + (scale(HDI) + plans_long + plans_short)"

# Affirmation effect for US racial-ethnic minorities (majority = white/asian/non-hispanic)
# Expect: affirmation supports US minority learners
models[["affirm.minority"]] = "affirm * us_majority + (plans_long + plans_short)"
samples[["US.respondent"]] = samples[["baseline"]] & !is.na(data$us_majority) # respondent in US 

# Affirmation effect moderated by HDI and individual-level national social identity threat (NSIT)
# Expect: affirmation especially supports low-HDI learners with high NSIT
models[["affirm.csit"]] = "affirm * highHDI * scale(threat_country) + (plans_long + plans_short)"

# Affirmation effect moderated by HDI region and individual-level lanuage skill
# Expect: affirmation especially supports low-HDI learners whose English is not fluent
models[["affirm.lang"]] = "affirm * highHDI * is_fluent + (plans_long + plans_short)"

# Affirmation effect by gender and course gender ratio
# Expect: affirmation supports female learners in male-dominated (<20% women) courses
data = ungroup(data %>% group_by(course) %>% mutate(male_dom_course = mean(gender_female) < .2))
models[["affirm.sex"]] = "affirm * gender_female * male_dom_course + (scale(HDI) + plans_long + plans_short)"

#######################################################
### Planned analyses for Plan-Making                ###
#######################################################
#
### Primary Analysis ###
#
# Plan-making effect for short-term and long-term plans
# H2a: Short-term plan-making increase course completion over baseline.
# H2b: Long-term plan-making increase course completion over baseline.
models[["plan.original"]] = "plans_long + plans_short + affirm"

# H2c: Long-term plan-making increase course completion more than short-term plan-making.
data$plans_any = as.numeric(data$plans > 0)
models[["plan.vs.plan"]] = "plans_any + plans_long + affirm"

# Previous plan-making interventions had pre-registered a subgroup for analysis:
# - Fluent English speakers, so that the task would not be effortful
# - Students who intend to complete most/all course assessments, as a masure of follow-through
samples[["fluent_intent"]] = samples[["baseline"]] & (data$is_fluent == 1) & (data$intent_assess == 4)

#######################################################
### Outcomes Measures and Special Samples           ### 
#######################################################
#
# The following analyses are structured based on the priorities specified above.
# The priorities for outcome measures are as follows:
# Primary outcomes: cert_basic (binary), course_progress (percentage)
# Secondary outcomes: cert_verified, upgrade_verified (binary; Harvard/MIT only)
# Tertiary outcome: likely_complete_1 (percentage)
#
### Primary outcome: cert_basic (binary) ###
.y = "cert_basic"
.s = "baseline"
# Note: EdX at Harvard/MIT does not provide "cert_basic" automatically. We will construct it from learner records.

### Primary outcome: course_progress ###
.y = "course_progress"
.s = "baseline"
# Note: We will count the percentage of unique videos watched directly from the tracking data.
# The number of videos per course varies across courses. We will document additional calculations, where necessary.

### Secondary outcome: cert_verified (binary) ###
.y = "cert_verified"
.s = "baseline_HarvardMIT"
# Note: MIT/Harvard only - defining specific sample that excludes learners in Stanford courses
samples[["baseline_HarvardMIT"]] = samples[["baseline"]] & data$institution %in% c("Harvard","MIT")
samples[["US.respondent_HarvardMIT"]] = samples[["US.respondent"]] & samples[["baseline_HarvardMIT"]]
samples[["fluent_intent_HarvardMIT"]] = samples[["fluent_intent"]] & samples[["baseline_HarvardMIT"]]

### Secondary outcome: upgrade_verified (binary) ###
.y = "upgrade_verified"
.s = "baseline_upgrades"
# Note: MIT/Harvard only - sample also excludes those who upgraded to verified track pre-exposure
samples[["baseline_upgrades"]] = samples[["baseline_HarvardMIT"]] & data$enroll_verified == 0
samples[["US.respondent_upgrades"]] = samples[["US.respondent"]] & samples[["baseline_upgrades"]]
samples[["fluent_intent_upgrades"]] = samples[["fluent_intent"]] & samples[["baseline_upgrades"]]

### Tertiary outcome likely_complete_1 (percentage) ###
.y = "likely_complete_1"
.s = "baseline"
# Note: This immediate self-report question is to examine the mechanism of the interventions

### Tertiary outcome future sign-ups (count) ###
# .y = "future courses"
# Note: We cannot measure this in our current data but we will calculate it in a year (January 1, 2018)

#######################################################
###  Model Estimation                               ###
#######################################################
#
### Overall Effect of Interventions ###
fit_model(model.name = "main.interaction", model.outcome = .y, sample = .s)

### Affirmation - Primary Analysis ###
fit_model(model.name = "simple", model.outcome = .y, sample = .s)

### Affirmation - Secondary Analyses ###
fit_model(model.name = "affirm.ses", model.outcome = .y, sample = .s)
fit_model(model.name = "affirm.csit", model.outcome = .y, sample = .s)
fit_model(model.name = "affirm.lang", model.outcome = .y, sample = .s)
fit_model(model.name = "affirm.sex", model.outcome = .y, sample = .s)
fit_model(model.name = "affirm.minority", model.outcome = .y, sample = "US.respondent") #_HarvardMIT

### Plan-making - Primary Analyses ###
# Note: Automatically dropping 'intent_assess' covariate from model for this sample
fit_model(model.name = "plan.original", model.outcome = .y, sample = "fluent_intent") #_HarvardMIT
fit_model(model.name = "plan.vs.plan", model.outcome = .y, sample = "fluent_intent") #_HarvardMIT

#######################################################
### Descriptive Statistics                          ### 
#######################################################
#
# Some descriptive statistics by condition
data %>% 
  filter(itt.sample) %>% # same as "baseline" sample
  group_by(affirm, plans) %>%
  summarise(
    N = n(),
    N_harvardMIT = sum(institution %in% c("Harvard","MIT")),
    cert_basic = mean(cert_basic),
    cert_verified = mean(cert_verified, na.rm=T),
    enroll_verified = mean(enroll_verified, na.rm=T),
    upgrade_verified = mean(upgrade_verified, na.rm=T),
    avg_course_progress = mean(course_progress),
    avg_age = mean(2017 - yob),
    prop_women = mean(sex == 2),
    prop_fluent = mean(is_fluent),
    prop_intent_assess_all = mean(intent_assess == 4),
    prop_highHDI = mean(highHDI)
  )

# Check balance of assignment conditional on stratification
# Need to correct for multiple comparisons, 36 for each condition
data %>% 
  filter(itt.sample) %>% # same as "baseline" sample
  group_by(strata_intent_assess, strata_educ, strata_hours, strata_crs_finish) %>%
  summarise(
    a.m = mean(affirm),
    a.p = prop.test(x = sum(affirm), n = n(), p=.5)$p.value,
    pl.m = mean(plans_long), 
    pl.p = prop.test(x = sum(plans_long), n = n(), p=1/3)$p.value,
    ps.m = mean(plans_short),
    ps.p = prop.test(x = sum(plans_short), n = n(), p=1/3)$p.value
  )

#######################################################
