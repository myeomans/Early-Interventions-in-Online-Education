#######################################################
#
#  Preregistration for "Joint Interventions at Scale"
#  Discovery of Heterogeneous Treatment Effects
#######################################################

#setwd("~/git/Joint-Interventions-At-Scale/")
require(sparsereg)
require(dplyr)

load("simdat.rda")

#######################################################
# Sample Selection and Data Transformation
#######################################################

# Selecting ITT sample as in prereg.R analyses
data = ungroup(data %>% 
  group_by(id) %>% 
  arrange(survey_timestamp) %>% 
  mutate(
    first.exposure = row_number() == 1,
    num.exposures = n(),
    days.to.next.exposure = ifelse(n() > 1, diff(survey_timestamp)[1] / (60*60*24), 0),
    clean.exposure = first.exposure & (num.exposures == 1 | days.to.next.exposure > 29)
  ))

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

data %>% filter(itt.sample)

# Transforming variables for sparse model
data = data %>% mutate(
  age = 2017 - yob,
  educ_phd = educ == 1,
  educ_ma_prof = educ %in% (2:3),
  educ_ba = educ == 4,
  educ_some_he = educ %in% (5:6),
  more_educ_than_parents = educ < educ_parents,
  is_teacher = teach == 1,
  is_employed = empstatus == 1,
  is_unemployed = empstatus == 2,
  is_ft_student = empstatus == 3,
  is_pt_student = (school == 1) & (school_ftpt == 0),
  born_in_US = pob == 187
)

covariates = c("intent_lecture", "intent_assess", "hours", "crs_finish",
               "goal_setting", "fam", "educ_parents", "age", "gender_female", "gender_other",
               "educ_phd", "educ_ma_prof", "educ_ba", "educ_some_he", "more_educ_than_parents",
               "is_teacher", "is_employed", "is_unemployed", "is_ft_student", "is_pt_student",
               "fluent", "threat_country", "olei_interest", "olei_job", "olei_degree",
               "olei_research", "olei_growth", "olei_career", "olei_fun", "olei_social",
               "olei_experience", "olei_certificate", "olei_uniprof", "olei_peer", 
               "olei_language", "HDI4", "born_in_US")

treatments = c("affirm", "plans_short", "plans_long")


#######################################################
# Fitting Sparsereg Model and Evaluating Results
#######################################################

SPR = sparsereg(
    y = data$cert_basic, 
    X = data.matrix(data[, covariates]), 
    treat = data[, treatments],
    id = data$course,
    id2 = data$strata,
    # id3 = "id",
    type = "probit",
    EM = F,
    scale.type = "TTX"
  )

summary(SPR)
plot(SPR)
violinplot(SPR)

round(SPR$beta.mean[1,], 3)
round(SPR$beta.mode[1,], 3)

# Could use glmnet cross-validated LASSO for comparison
# require(glmnet)
# coef(cv.glmnet(x=SPR$X, y=SPR$y), s="lambda.1se")

#######################################################