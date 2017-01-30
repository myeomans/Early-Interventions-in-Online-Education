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
# Identify first exposure to treatment, number of exposures, and days to second exposure
#
data = ungroup(data %>% 
  group_by(id) %>% 
  arrange(survey_timestamp) %>% 
  mutate(
    first.exposure = row_number() == 1,
    num.exposures = n(),
    days.to.next.exposure = ifelse(n() > 1, diff(survey_timestamp)[1] / (60*60*24), 0),
    clean.exposure = first.exposure & (num.exposures == 1 | days.to.next.exposure > 29)
  ))

#  Survey software records all learners who started the course survey.
#  We impose a set of exclusion criteria to define our sample of interest:
#  a. The learner must have completed enough of the survey to be randomized and exposed to condition
#  b. The learner must have been exposed only once within 30 days of first exposure
#  c. The learner must have started the survey within the first hour of their first timestamp in the course
#  d1. The learner must have started a cohort-based course in its first 14 days, OR
#  d2. The learner must have started a self-paced course before the last 30 days
#
analysis_timestamp = 1487145600 # e.g. 2/15/2017
data = mutate(data,
  exposed.to.treat = (webservice_call_complete == 1) & (!is.na(affirm)) & (!is.na(plans)), # a.
  survey.delay = (survey_timestamp - first_activity_timestamp) / (60*60), # c.
  days.from.start = (first_activity_timestamp - course_start_timestamp) / (60*60*24), # d1.
  days.to.analysis = (analysis_timestamp - first_activity_timestamp) / (60*60*24), # d2.
  itt.sample = exposed.to.treat & # a.
    clean.exposure & # b.
    (survey.delay < 1) & # c.
    ((course_selfpaced & (days.to.analysis > 29)) | ((!course_selfpaced) & (days.from.start < 15))) # d1|d2
)

samples = list()
samples[["baseline"]] = data$itt.sample # baseline
samples[["baseline_HarvardMIT"]] = samples[["baseline"]] & data$school %in% c(1:2)

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
# models[["baseline"]] = "1"
# simple treatment effects
models[["main.interaction"]] = "affirm * (plans_long + plans_short) " 

#######################################################
### Planned analyses for Affirmation ###
#######################################################

### Primary Analysis ###
# Replication: Affirmation effect in low vs. high HDI countries
# Expect: affirmation supports low-HDI learners
models[["simple"]] = " affirm + (plans_long + plans_short)"

samples[["high.hdi"]] = samples[["baseline"]] & data$highHDI == 1
samples[["low.hdi"]] = samples[["baseline"]] & data$highHDI == 0
samples[["high.hdi_HarvardMIT"]] = samples[["high.hdi"]] & samples[["baseline_HarvardMIT"]]
samples[["low.hdi_HarvardMIT"]] = samples[["low.hdi"]] & samples[["baseline_HarvardMIT"]]

### Secondary Analysis - Theory-driven Extensions ###

# Affirmation effect by socioeconomic status (based on parental education)
# Expect: affirmation supports low-SES learners
models[["affirm.ses"]] = " affirm * scale(10-educ_parents) + (plans_long + plans_short)"

# Affirmation effect for US racial-ethnic minorities (majority = white/asian/non-hispanic)
# Expect: affirmation supports US minority learners
models[["affirm.minority"]] = " affirm * us_majority + (plans_long + plans_short)"
samples[["US.respondent"]] = samples[["baseline"]] & !is.na(data$us_majority) # respondent in US 
samples[["US.respondent_HarvardMIT"]] = samples[["US.respondent"]] & samples[["baseline_HarvardMIT"]]

# Affirmation effect moderated by HDI and individual-level country social identity threat
# Expect: affirmation supports learners in low HDI regions, especially those with high threat
models[["affirm.csit"]] = " affirm * scale(HDI) * scale(threat_country) + (plans_long + plans_short)"

# Affirmation effect moderated by HDI region and individual-level lanuage skill
# Expect: affirmation supports learners in low HDI regions, especially those whose English is not fluent 
models[["affirm.lang"]] = " affirm * highHDI * is_fluent + (plans_long + plans_short)"

# Affirmation effect by gender and course gender ratio
# Expect: affirmation supports learners of underrepresented (<33%) gender group
data = data %>% group_by(course) %>% mutate(course_prop_female = mean(gender_female))
models[["affirm.sex"]] = " affirm * gender_female * scale(course_prop_female) + (plans_long + plans_short) "

#######################################################
### Planned analyses for Plan-Making ###
#######################################################

### Primary Analysis ###
data$plans_any = as.numeric(data$plans > 0)
models[["plan.original"]] = "affirm + (plans_long + plans_short)"  
models[["plan.vs.plan"]] = "affirm + (plans_any + plans_long)"

# Define sample based on from previous plan-making intervention: 
# Fluent English speakers who intend to complete all course assessments
samples[["fluent_intent"]] = samples[["baseline"]] & (data$is_fluent == 1) & (data$intent_assess == 4)
samples[["fluent_intent_HarvardMIT"]] = samples[["fluent_intent"]] & samples[["baseline_HarvardMIT"]]


#######################################################
####  Generalized Model Fitting Function #### 
#######################################################
fit_model = function(model.name, model.outcome, sample = "baseline") {
  
  # Select relevant sample for model
  model.data = data[samples[[sample]],]
  
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
#### Fitting Models #### 
#######################################################
#
# The following analyses are structured based on the priorities specified above.
# The priorities for outcome measures are as follows:
# Primary outcomes: cert_basic (binary), cert_verified (binary; Harvard/MIT only)
# Secondary outcomes: course_progress (percentage), upgrade_verified (binary; Harvard/MIT only)
# Tertiary outcome: likely_complete_1 (percentage)
#
# The 'baseline' ITT sample is used unless specified otherwise.
#
### Primary outcome: cert_basic (binary) ###
.y = "cert_basic"

# Overall Effect of Interventions
fit_model(model.name = "main.interaction", model.outcome = .y)

# Affirmation - Primary Analysis
fit_model(model.name = "simple", model.outcome = .y, sample = "low.hdi")
fit_model(model.name = "simple", model.outcome = .y, sample = "high.hdi")

# Affirmation - Secondary Analysis
fit_model(model.name = "affirm.ses", model.outcome = .y)
fit_model(model.name = "affirm.minority", model.outcome = .y, sample = "US.respondent")
fit_model(model.name = "affirm.csit", model.outcome = .y)
fit_model(model.name = "affirm.lang", model.outcome = .y)
fit_model(model.name = "affirm.sex", model.outcome = .y)

# Affirmation - Tertiary Analysis With All Exposed Sample
fit_model(model.name = "affirm.ses", model.outcome = .y, sample = "exposed")
fit_model(model.name = "affirm.minority", model.outcome = .y, sample = "exposed", sample = "US.respondent")
fit_model(model.name = "affirm.csit", model.outcome = .y, sample = "exposed")
fit_model(model.name = "affirm.lang", model.outcome = .y, sample = "exposed")
fit_model(model.name = "affirm.sex", model.outcome = .y, sample = "exposed")

# Plans - Primary Analysis (note: dropping intent_assess cov. from model for this subsample)
fit_model(model.name = "plan.original", model.outcome = .y, sample = "fluent_intent")
fit_model(model.name = "plan.vs.plan", model.outcome = .y, sample = "fluent_intent")


### Secondary outcome course_progress (percentage) ###
.y = "course_progress"
# Run same models as for the primary outcome.


### Tertiary outcome likely_complete_1 (percentage) ###
.y = "likely_complete_1"
# Run same models as for the primary outcome.


### Primary outcome (MIT/Harvard only) cert_verified (binary) ###
.y = "cert_verified"
.s = "baseline_HarvardMIT"

# Overall Effect of Interventions
fit_model(model.name = "main.interaction", model.outcome = .y, sample = .s)

# Affirmation - Primary Analysis
fit_model(model.name = "simple", model.outcome = .y, sample = "low.hdi_HarvardMIT")
fit_model(model.name = "simple", model.outcome = .y, sample = "high.hdi_HarvardMIT")

# Affirmation - Secondary Analysis
fit_model(model.name = "affirm.ses", model.outcome = .y, sample = .s)
fit_model(model.name = "affirm.minority", model.outcome = .y, sample = "US.respondent_HarvardMIT")
fit_model(model.name = "affirm.csit", model.outcome = .y, sample = .s)
fit_model(model.name = "affirm.lang", model.outcome = .y, sample = .s)
fit_model(model.name = "affirm.sex", model.outcome = .y, sample = .s)

# Plans - Primary Analysis (note: dropping intent_assess cov. from model for this subsample)
fit_model(model.name = "plan.original", model.outcome = .y, sample = "HarvardMIT_fluent_intent")
fit_model(model.name = "plan.vs.plan", model.outcome = .y, sample = "HarvardMIT_fluent_intent")


### Secondary outcome (MIT/Harvard only) upgrade_verified (binary) ###
.y = "upgrade_verified"
.s = "baseline_HarvardMIT"
# Run same models as for MIT/Harvard only outcome cert_verified.


#######################################################
#### Descriptive Statistics #### 
#######################################################

# Some descriptive statistics by condition
data %>% 
  filter(itt.sample) %>% # same as "baseline" sample
  group_by(affirm, plans) %>%
  summarise(
    N = n(),
    N_harvardMIT = sum(school %in% (1:2)),
    cert_basic = mean(cert_basic),
    cert_verified = mean(cert_verified, na.rm=T),
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
  # filter(itt.sample) %>% # same as "baseline" sample
  group_by(strata_intent_assess, strata_educ, strata_hours, strata_crs_finish) %>%
  summarise(
    a.m = mean(affirm),
    a.p = prop.test(x = sum(affirm), n = n(), p=.5)$p.value,
    pl.m = mean(plans_long), 
    pl.p = prop.test(x = sum(plans_long), n = n(), p=1/3)$p.value,
    ps.m = mean(plans_short),
    ps.p = prop.test(x = sum(plans_short), n = n(), p=1/3)$p.value
  )

##############################
