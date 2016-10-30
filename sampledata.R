#################################################################
# Assigner
set.seed(123456)
samp<-function(x, n=1e6){
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
data<-data.frame(school=samp(1:3),
                 course=samp(1:20),
                 webservice_call_complete = samp(0:1),
                 affirm=samp(0:1),
                 plans=samp(0:2),
                 intend_lecture=samp(1:4),
                 intend_assess=samp(1:4),
                 hours=samp(0:50),
                 crs_finish=samp(0:30),
                 goal_setting=samp(1:5),
                 fam=samp(1:5),
                 sex=samp(1:3),
                 yob=samp(1950:2000),
                 empstatus=samp(1:5),
                 teach=samp(c(0,1,13)),
                 school=samp(0:1),
                 educ=samp(1:10),
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
                 course_start_timestamp=samp(1470009600:1475280000),
                 cert_verified=samp(0:1),
                 cert_basic=samp(0:1),
                 upgrade_verified=samp(0:1),
                 subsequent_enroll=samp(0:1),
                 course_progress=samp(0:100))

data[data$affirm == 0, grepl("affirm_", names(data))] = NA
data[data$plans != 2, grepl("longplans", names(data))] = NA
data[data$plans != 1, grepl("shortplans", names(data))] = NA

#######################################################
# Exclusions: Exposure & Timing
#######################################################
# Learner was assigned correctly and exposed
data = subset(data, webservice_call_complete == 1 & 
                (!is.na(affirm) | !is.na(plans)) &
                rowSums(cbind(affirm_choice_time_3, shortplans_time_3, 
                              longplans_time_3), na.rm = T) > 0)

# Learner entered course within the first 14 days of course launch
data = subset(data, (first_activity_timestamp-course_start_timestamp)/(60*60*24) < 14)

# Learner started the survey within the first hour of entering the course
data = subset(data, (survey_timestamp-first_activity_timestamp)/(60*60) < 1)

#######################################################
# Treatment Variables
#######################################################
data$plans_long = ifelse(data$plans == 2, 1, 0)
data$plans_short = ifelse(data$plans == 1, 1, 0)

#######################################################
# Merge with HDI data
#######################################################
data$HDI = sample(30:98, nrow(data), replace = T)/100

#######################################################

save(data, file="simdat.rda")
