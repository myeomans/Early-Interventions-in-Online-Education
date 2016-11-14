library(dplyr)
set.seed(123456)
N = 5e5

#################################################################
# Assigner
samp = function(x, n=N){
  sample(x, n, replace=T)
}

# Variable Sets
# times<-(0:10)
# texts<-paste0("sampletext",1:50)
# YNU<-c("Yes","No","Unsure")
# intend<-c("All","Most","Few","No")
# education<-c("doctor","master","profess","bachelor","somecollege",
#              "associate","highschool","middleschool","elementary","none")
# intend.verify=samp(1:3),
# intend.forums=samp(c("Post-Hi","Post-Lo","Read","None")),
# employment=samp(c("employed","unemployed","student","retired")),
# teacher=samp(YNU),
# student=samp(YNU),
# education.own=samp(education),
# education.parent=samp(education),
# english=samp(1:5),
# country=samp(c("Canada","Seychelles","Singapore")),
# worried=samp(1:5),
# hispanic=samp(c("yes","mexican","no")),
# race=samp(c("black","white","amind","asian","nhopi")),
# teacher.held=samp(c("teach","admin","coach","data")),
# teacher.related=samp(YNU),
# teacher.current=samp(c("Yes","No")),
# teacher.level=samp(c("primary","secondary","college","outside")),
# industry.current=samp(1:12),
# industry.past=samp(1:12),
# industry.future=samp(1:12),
# industry.if=samp(1:12),
# industry.job=samp(1:12),
# student.fulltime=samp(c("Yes","No")),
# student.similar=samp(1:4),
# student.level=samp(c("primary","middle","secondary","2year","4year","grad")),
# interview=samp(c("Yes","No")),
# harvard=samp(1:4),
# webservice_call_complete = samp(0:1),
# treated=samp(c(0,rep(1,10))),
# affirm.text1=samp(texts),
# affirm.time1=samp(times),
# affirm.text2=samp(texts),
# affirm.time2=samp(times),
# affirm.text3=samp(texts),
# affirm.time3=samp(times),
# plans.text=samp(texts),
# plans.time1=samp(times),
# plans.time2=samp(times),
# plans.short=samp(c(0,1)),
# prob.complete=samp(0:100))

#################################################################
data = data.frame(school=samp(1:3),
                 course=samp(1:25),
                 webservice_call_complete = samp(rep(0:1, c(1,99))),
                 # affirm=samp(0:1),
                 # plans=samp(0:2),
                 intent_lecture=samp(1:4),
                 intent_assess=samp(rep(1:4, c(30,10,10,50))),
                 hours=rpois(N, 6), #samp(0:50),
                 crs_finish=rpois(N, 1.5), #samp(0:30),
                 goal_setting=samp(1:5),
                 fam=samp(1:5),
                 sex=samp(1:3),
                 yob=samp(1950:2000),
                 empstatus=samp(1:5),
                 teach=samp(c(0,1,13)),
                 school=samp(0:1),
                 educ=sample(1:10, N, replace=T, prob=c(.1, .1, .1, .4, rep(.05, 6))), #samp(1:10),
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
                 likely_complete_1=samp(0:100),
                 survey_timestamp=samp(1470009600:1475280000),
                 first_activity_timestamp=samp(1470009600:1475280000),
                 course_start_timestamp=samp(1470009600:1475280000)
                 # cert_verified=samp(0:1),
                 # cert_basic=samp(0:1),
                 # upgrade_verified=samp(0:1),
                 # subsequent_enroll=samp(0:1),
                 # course_progress=samp(0:100)
)

#######################################################
# Simulate Stratified Assignment
#######################################################

data$strata_intent_assess = ifelse(data$intent_assess>2, 1, 0)
data$strata_hours = ifelse(data$hours>5, 1, 0)
data$strata_crs_finish = ifelse(data$crs_finish>3, 2, ifelse(data$crs_finish>0, 1, 0))
data$strata_educ = ifelse(data$educ<4, 2, ifelse(data$educ==4, 1, 0))
data$strata = with(data, paste(strata_intent_assess, strata_hours, strata_crs_finish, strata_educ))

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
# Exclusions: Exposure & Timing
#######################################################
# Learner was assigned correctly and exposed
data = subset(data, webservice_call_complete == 1 & (!is.na(affirm) | !is.na(plans)))

# Learner entered course within the first 14 days of course launch
data = subset(data, (first_activity_timestamp-course_start_timestamp)/(60*60*24) < 14)

# Learner started the survey within the first hour of entering the course
data = subset(data, (survey_timestamp-first_activity_timestamp)/(60*60) < 1)

#######################################################
# Treatment Variables & Subgroups
#######################################################
data$plans_long = ifelse(data$plans == 2, 1, 0)
data$plans_short = ifelse(data$plans == 1, 1, 0)

data$HDI = sample(30:98, nrow(data), replace = T)/100
data$highHDI = as.numeric(data$HDI > 0.7)

data$is_fluent = as.numeric(data$fluent == 5)

#######################################################
# Simulate Treatment Effects
#######################################################

baseline = .1
eff_affirm = .1
eff_plan_st = .05
eff_plan_lg = .1
error_sd = .025

data$y_prob = rnorm(nrow(data), baseline, error_sd) +
  eff_affirm * data$affirm * (1 - data$highHDI) + 
  eff_plan_st * data$plans_short * data$is_fluent + 
  eff_plan_lg * data$plans_long * data$is_fluent

data$y_prob[data$y_prob<0] = 0

data$cert_verified = rbinom(nrow(data), 1, data$y_prob)
data$cert_basic = rbinom(nrow(data), 1, data$y_prob)
data$subsequent_enroll = rbinom(nrow(data), 1, data$y_prob)
data$upgrade_verified = rbinom(nrow(data), 1, data$y_prob)
data$course_progress = sample(0:100, nrow(data), replace=T) # suppose no treatment effect

#######################################################

save(data, file="simdat.rda")
