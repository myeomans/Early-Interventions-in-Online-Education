#######################################################
#
#  Preregistration for "Joint Interventions at Scale"
#  Data Generation
#######################################################

#setwd("~/git/Joint-Interventions-At-Scale/")
library(dplyr)
set.seed(02138)

# Number of observations to simulate
N = 1e5

# Sampling function
samp = function(x, n = N, repl = T){
  sample(x, n, replace = repl)
}

#######################################################
# Simulate Initial Dataset
#######################################################
data = data.frame(id=samp(N, N, F),
                  school=samp(1:3),
                  course=samp(1:35),
                  webservice_call_complete = samp(rep(0:1, c(1,99))),
                  intent_lecture=samp(1:4),
                  intent_assess=samp(rep(1:4, c(30,10,10,50))),
                  hours=rpois(N, 6),
                  crs_finish=rpois(N, 1.5),
                  goal_setting=samp(1:5),
                  fam=samp(1:5),
                  sex=samp(1:3),
                  yob=samp(1950:2000),
                  empstatus=samp(1:5),
                  teach=samp(c(0,1,13)),
                  school=samp(0:1),
                  educ=sample(1:10, N, replace=T, prob=c(.1, .1, .1, .4, rep(.05, 6))),
                  educ_parents=samp(1:10),
                  fluent=samp(1:5),
                  pob=samp(c(0:193, 580, 1357)),
                  threat_country=samp(1:5),
                  us_ethnicity=samp(1:3),
                  us_race=samp(1:6),
                  industry=samp(0:22),
                  occupation=samp(0:14),
                  school_ftpt=samp(0:1),
                  school_online=samp(1:4),
                  school_lev=samp(1:7),
                  olei_interest=samp(0:1),
                  olei_job=samp(0:1),
                  olei_degree=samp(0:1),
                  olei_research=samp(0:1),
                  olei_growth=samp(0:1),
                  olei_career=samp(0:1),
                  olei_fun=samp(0:1),
                  olei_social=samp(0:1),
                  olei_experience=samp(0:1),
                  olei_certificate=samp(0:1),
                  olei_uniprof=samp(0:1),
                  olei_peer=samp(0:1),
                  olei_language=samp(0:1),
                  affirm_values_1=samp(0:1),
                  affirm_values_2=samp(0:1),
                  affirm_values_3=samp(0:1),
                  affirm_values_4=samp(0:1),
                  affirm_values_5=samp(0:1),
                  affirm_values_6=samp(0:1),
                  affirm_values_7=samp(0:1),
                  affirm_values_8=samp(0:1),
                  affirm_values_9=samp(0:1),
                  affirm_values_10=samp(0:1),
                  affirm_values_11=samp(0:1),
                  affirm_values_12=samp(0:1),
                  affirm_values_13=samp(0:1),
                  affirm_choice_time_3=samp(0:600),
                  affirm_write_time_3=samp(0:600),
                  shortplans_time_3=samp(0:600),
                  shortplanswrite_time_3=samp(0:600),
                  longplans_time_3=samp(0:600),
                  longplanswrite_time_3=samp(0:600),
                  survey_timestamp=samp(1470009600:1475280000),
                  first_activity_timestamp=samp(1470009600:1475280000),
                  course_start_timestamp=samp(1470009600:1475280000)
)

#######################################################
# Simulate Stratified Assignment
#######################################################

# Define strata
data$strata_intent_assess = ifelse(data$intent_assess>2, 1, 0)
data$strata_hours = ifelse(data$hours>5, 1, 0)
data$strata_crs_finish = ifelse(data$crs_finish>3, 2, ifelse(data$crs_finish>0, 1, 0))
data$strata_educ = ifelse(data$educ<4, 2, ifelse(data$educ==4, 1, 0))

# Encode strata in single variable
data$strata = with(data, paste(strata_intent_assess, strata_hours, strata_crs_finish, strata_educ))

# Simulate random assignment within each strata
data = data %>% 
  group_by(strata) %>%
  mutate(c = c(rep(1:(n() %/% 6), each=6), rep(1 + n() %/% 6, n() %% 6))) %>%
  group_by(strata, c) %>%
  mutate(
    cond = sample(1:6, size = n()),
    affirm = as.numeric(cond>3),
    plans = cond %% 3
  )

#######################################################
# Remove Counterfactual Data
#######################################################

data[data$affirm == 0, grepl("affirm_", names(data))] = NA
data[data$plans != 2, grepl("longplans", names(data))] = NA
data[data$plans != 1, grepl("shortplans", names(data))] = NA

#######################################################
# Define Treatment Variables & Subgroups
#######################################################

data$plans_long = ifelse(data$plans == 2, 1, 0)
data$plans_short = ifelse(data$plans == 1, 1, 0)

# Simulate Human Development Index (HDI) and Discretize (high/low)
data$HDI = sample(30:98, nrow(data), replace = T)/100
data$highHDI = as.numeric(data$HDI > 0.7)
data$HDI4 = cut(data$HDI, 
  breaks = c(0, .55, .7, .8, 1), 
  labels = c("low", "medium", "high", "v.high"))

# Binary indicator for fluent English speakers
data$is_fluent = as.numeric(data$fluent == 5)

# Binary indicator for self-paced (vs. cohort-based) courses
data$is_selfpaced = data$course %in% (1:5)

#######################################################
# Simulate Outcome Measures with Treatment Effects
#######################################################

baseline = .05
eff_affirm = .05
eff_plan_st = .025
eff_plan_lg = .05
eff_inter = .025
error_sd = .1

# Encode strata covariates and school/course effects in sigmoid
sig = 1/(1+exp(-rowMeans(scale(
  ungroup(data) %>% select(intent_assess, hours, crs_finish, educ, school, course) %>% mutate(educ=10-educ) ))))

# Simulate probability of certification
data$y_prob = baseline + 
  sig / 5 + # baseline plus covariate contribution
  eff_affirm * data$affirm * (1 - data$highHDI) + # affirm effect in Low HDI
  eff_plan_st * data$plans_short + # srt plans effect
  eff_plan_lg * data$plans_long + # lng plans effect
  eff_inter * data$affirm * data$plans_long + # treatment interaction effect
  rnorm(nrow(data), 0, error_sd) # gaussian error term

# Ensure probability is in [0,1]
data$y_prob[data$y_prob<0] = 0
data$y_prob[data$y_prob>1] = 1

# Generate All Outcome Measures Based on Pr(certification)
data$cert_verified = rbinom(nrow(data), 1, data$y_prob)
data$cert_basic = rbinom(nrow(data), 1, data$y_prob)
data$upgrade_verified = rbinom(nrow(data), 1, data$y_prob)
data$course_progress = 100 * data$y_prob
data$likely_complete_1 = 100 * data$y_prob

#######################################################
# Save Simulated Dataset
#######################################################

save(data, file="simdat.rda")
#######################################################
