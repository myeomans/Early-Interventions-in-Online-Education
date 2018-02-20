# TITLE: Descriptive Analysis of Survey Data
# AUTHOR: Joshua Littenberg-Tobias, TSL (MIT)
# DATE: 1/24/2018
# BRIEF DESCRIPTION: Code to analyze survey data to compare blended students 

# Clear workspace
rm(list = ls())

# Load and install packages
library(pacman)
p_load(tidyr, dplyr, ggplot2, stringr, janitor, httpuv, gridExtra, grid, magrittr)

# Set up directory
setwd("~/Micromasters/Data")


# IMPORT DATA--------------------------------------------------------------------------------

# Import survey data
combined_data <- read.csv("analysis_comparison_data.csv")

# Remove residential students from data
combined_data <- combined_data %>%
                 filter(group_name != "Residential")

# Import course actions data - only import data frm Students (not Staff)
course_actions <- read.csv("course_actions_data.csv") %>%
    filter(roles == "Student")


# DATA SET-UP---------------------------------------------------------------------------------------

# Merge course actions with survey data on likelyhood pf completing coourse
course_actions <- left_join(course_actions,
                            combined_data %>%
                                select(id_map_hash_id, enrollment_course_id,
                                       likely_complete_1))


# Change blended so its 0 unless they did blended program
course_actions <- course_actions %>%
                  mutate(blended = ifelse(!is.na(blended) & blended == 1,
                                          1, 0))

# Create variable that specificies specific courses (does not matter the run)
combined_data %<>% 
    mutate(course_id = word(enrollment_course_id, 2, 2, sep = "/"),
           course_id = ifelse(str_detect(course_id, "_"),
                              word(course_id, 1, 1, "_"), course_id)
            )

tabyl(combined_data, course_id)

# Repeat with course actons
course_actions %<>%
    mutate(course_id = word(enrollment_course_id, 2, 2, sep = "/"),
           course_id = ifelse(str_detect(course_id, "_"),
                              word(course_id, 1, 1, "_"), course_id)
    )

tabyl(course_actions, course_id)


# FUNCTIONS-----------------------------------------------------------------------------------------

# Grid Arrange with shared legend
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + 
                        theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x +
                     theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(position,
                       "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                              legend,ncol = 1,
                                              heights = unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                             legend, ncol = 2,
                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
}




# SURVEY DESCRIPTIVE STATISTICS---------------------------------------------------------------------

# Reasons for taking the course - compare Blended students (N = 66) to Comparison Group (N = 6483)
# on reasons for taking hte course
course_reasons <- as.list(paste0("mrmi_mrmi_", seq(1, 10, by = 1)))
course_reasons_labels <- list("Lifelong learning", "Curiosity about online learning",
                              "Advancing my career", "Advancing my formal education",
                              "Best professors and universities", "Serve my community",
                              "Access learning opportunities not otherwise available",
                              "Earn a certificate", "Participate in online community",
                              "Learn about course content")

mapply(function(x, y) {
    combined_data %>%
        gather(variable, value, starts_with("mrmi")) %>%
        filter(variable == x) %>%
        filter(stringr::str_detect(as.character(value), "Important")) %>%
        mutate(value = case_when(
            .$value == "Extremely Important" ~ 5,
            .$value == "Very Important" ~ 4,
            .$value == "Somewhat Important" ~ 3,
            .$value == "Slightly Important" ~ 2,
            .$value == "Not Important" ~ 1
        ),
        value = factor(value,
                       levels = c(1:5),
                       labels = c("Not Important",
                                  "Slightly Important",
                                  "Somewhat Important",
                                  "Very Important",
                                  "Extremely Important"))) %>%
        crosstab(value, group, percent = "col") %>%
        knitr::kable (
            format =  "pandoc",
            digits = 3,
            col.names = c("Response", "Blended Students", "Other Verified Students"),
            align = "lrr",
            caption = y
        )
}, x = course_reasons, y = course_reasons_labels, SIMPLIFY = F)


# Create paired bar graphs for reasons for taking the course- make it so the y axis is at
# vry important and we combined the top two categories. Need to order categories slightly
# different so they show up correctly on the graph 
course_reasons_labels_graph <- list("Lifelong learning", "Curiosity about\nonline learning",
                                    "Advancing my\ncareer", "Advancing formal\neducation",
                                    "Best professors\nand universities", "Serve my\ncommunity",
                                    "Access learning\nopportunities",
                                    "Earn certificate", "Participate in\nonline community",
                                    "Learn about\ncourse content")


data_plot <- mapply(function(x, y) {
    combined_data %>%
        gather(variable, value, starts_with("mrmi")) %>%
        filter(variable == x) %>%
        filter(stringr::str_detect(as.character(value), "Important")) %>%
        mutate(value = case_when(
            .$value == "Extremely Important" ~ 4,
            .$value == "Very Important" ~ 5,
            .$value == "Somewhat Important" ~ 3,
            .$value == "Slightly Important" ~ 2,
            .$value == "Not Important" ~ 1
        ),
        value = factor(value,
                       levels = c(1:5),
                       labels = c("Not Important",
                                  "Slightly Important",
                                  "Somewhat Important",
                                  "Extremely Important",
                                  "Very Important")),
        group = factor(group,
                       labels = c("blended",
                                  "comparison"))) %>%
        crosstab(value, group, percent = "col")   %>%
        mutate(variable = y) %>%
        gather(group_label, pct, blended, comparison) %>%
        mutate(pct = ifelse(as.numeric(value) <= 3, pct * -1, pct),
               top2 = pct > 0)  %>%
        group_by(variable, group_label, top2) %>%
        mutate(pct_top2 = sum(pct))
}, x = course_reasons, y = course_reasons_labels_graph, SIMPLIFY = F) %>% bind_rows()


# Plot data
ggplot(data = data_plot, aes(x = group_label , y = pct, fill = value, alpha = group_label)) +
    facet_grid(. ~ variable) +
    geom_bar(stat = "identity", position = position_stack(), width = 0.5) +
    geom_text(aes(x = group_label, y = pct_top2 + 0.05, 
                  label = ifelse(value == "Extremely Important",
                                 paste0(round(pct_top2, 2) * 100, "%"), NA)),
              size = 3,
              alpha = 1) +
    scale_alpha_manual(values = c(1, 0.5),
                       labels = c("Blended students", "Other verified student")) +
    scale_fill_manual(values = c("#000000", "#8A8B8C", "#C2C0BF", "#40BDBF", "#A31F34"),
                      limits = c("Not Important",
                                 "Slightly Important",
                                 "Somewhat Important",
                                 "Very Important",
                                 "Extremely Important")) +
    geom_hline(yintercept = 0, size = 1) +
    scale_y_continuous(limits = c(-0.8, 1.1)) +
    theme_minimal() +
    guides(fill = guide_legend(nrow = 2, byrow = T)) +
    theme(
        text = element_text(size = 14),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank()
    ) +
    ggtitle("Reasons for taking the course")

# Remove data plot
rm(data_plot)



# Intentions to participate in the course-----------------------------------------------------------

# Compare the the percentage of Blended students versus other students by how much they intend 
# to participate in the course
data_plot <- combined_data %>%
             gather(indicator, value, intent_lecture:forums) %>%
             filter(str_detect(value, ".")) %>%
             mutate(value_code = ifelse(str_detect(value, "All") |
                                        str_detect(value, "frequently"),
                                        T, F)) %>%
             group_by(group, indicator) %>%
             summarise(pct_high = mean(value_code),
                       n = n()) %>%
             ungroup() %>%
             mutate(indicator = factor(indicator,
                                       levels = c("intent_lecture", 
                                                  "intent_assess",
                                                  "forums"),
                                       labels = c("Participate in all lectures",
                                                  "Participate in all assessments",
                                                  "Participate frequently in the forums")),
                    group = factor(group,
                              labels = c("Blended\nstudents",
                                         "Other verified\nstudents")))


# Plot data
ggplot(data = data_plot, aes(x = group, y = pct_high, alpha = group)) +
       facet_grid(. ~ indicator) +
       geom_bar(stat = "identity", fill = "#A31F34") +
       geom_text(aes(y = pct_high + 0.05,
                     label = paste0(round(pct_high, 2) * 100, "%")),
                 size = 4,
                 alpha = 1) +
       scale_y_continuous(limits = c(0, 1.1)) +
       scale_alpha_manual(values = c(1, 0.5), guide = F) +
       theme_minimal() +
       theme(
            text = element_text(size = 14),
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.text.y = element_blank(),
            axis.title = element_blank(),
            panel.spacing = unit(3, "lines")
       ) +
    ggtitle("Intentions to participate in course")

# Remove data plot
rm(data_plot)


# Hours per week intending to spend on the course
combined_data %>%
    filter(str_detect(hours, "[0-9]")) %>%
    group_by(group) %>%
        summarise(num_hours = mean(hours),
                  sd_hours = sd(hours),
                  n = n()) %>%
    knitr::kable (
        format =  "pandoc",
        digits = 2,
        col.names = c("Group", "Mean Number of Hours", "SD Number of Hours", "N"),
        align = "lrrr",
        caption = "Number of hours expect to work on course per week"
    )

# Number of online courses completed in the past
combined_data %>%
    filter(str_detect(oc_comp, "[0-9]")) %>%
    group_by(group) %>%
    summarise(num_hours = mean(oc_comp),
              sd_hours = sd(oc_comp),
              n = n()) %>%
    knitr::kable (
        format =  "pandoc",
        digits = 2,
        col.names = c("Group", "Mean number of courses completed", "SD number of courses completed", "N"),
        align = "lrrr",
        caption = "Number of previous online courses completed"
    )

# How often do you set goals before learning something new?
combined_data %>%
    mutate(goal_setting = factor(goal_setting,
                                 levels = c("Never",
                                            "Rarely",
                                            "Sometimes",
                                            "Most of the time",
                                            "Always"))) %>%
    filter(!is.na(goal_setting)) %>%
    crosstab(goal_setting, group, percent = "col") %>%
    knitr::kable (
        format =  "pandoc",
        digits = 4,
        col.names = c("Frequency", "Blended students", "Other verified student"),
        align = "lrr",
        caption = "How often do you set goals before learning something new?"
    )


# Plot as two different pie chart
blended_students_chart <- combined_data %>%
    mutate(goal_setting = factor(goal_setting,
                                 levels = c("Never",
                                            "Rarely",
                                            "Sometimes",
                                            "Most of the time",
                                            "Always"))) %>%
    filter(!is.na(goal_setting) & group == 1) %>%
    tabyl(goal_setting, percent = "col", sort = T)



# Plot as pie_chart
blended_plot <- ggplot(data = blended_students_chart, aes(x = "", y = percent, fill = goal_setting)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0,  direction = 1) +
        geom_text(aes(x = 1,
                      y = cumsum(percent) - percent/2,
                  label = ifelse(percent > 0.005,
                                 paste0(round(percent, 2) * 100, "%"),
                                 NA)),
                  size = 5) +
      scale_fill_manual(values = c("#000000", "#8A8B8C", "#C2C0BF", "#40BDBF", "#A31F34")) +
      theme_void() +
      theme(
            text = element_text(size = 14),
            legend.position = "left",
            legend.title = element_blank(),
            plot.title = element_text(hjust= 0.5)
            ) +
    ggtitle("Blended students")

# Non-blended students chart
non_blended_students_chart <- combined_data %>%
    mutate(goal_setting = factor(goal_setting,
                                 levels = c("Never",
                                            "Rarely",
                                            "Sometimes",
                                            "Most of the time",
                                            "Always"))) %>%
    filter(!is.na(goal_setting) & group == 2) %>%
    tabyl(goal_setting, percent = "col") %>%
    arrange(desc(goal_setting))


# Plot as pie_chart
non_blended_plot <- ggplot(data = non_blended_students_chart, aes(x = "", y = percent, fill = goal_setting)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y", start = 0,  direction = 1) +
    geom_text(aes(x = 1,
                  y = cumsum(percent) - percent/2,
                  label = ifelse(percent > 0.01,
                                 paste0(round(percent, 2) * 100, "%"),
                                 NA)),
              size = 5) +
    scale_fill_manual(values = c("#000000", "#8A8B8C", "#C2C0BF", "#40BDBF", "#A31F34"),
                      limits = c("Never", "Rarely", "Sometimes", "Most of the time",
                                 "Always")) +
    theme_void() +
    theme(
        text = element_text(size = 14),
        legend.position = "none",
        legend.title = element_blank(),
        plot.title = element_text(hjust= 0.5)
    ) +
    ggtitle("Other verified students")

library(gridExtra)

# Combine plots
grid_arrange_shared_legend(blended_plot, non_blended_plot, ncol = 2, nrow = 1)


# How familiar are you with the topics in the course?
combined_data %>%
    mutate(fam= factor(fam,
                       levels = c("Not at all familiar",
                                  "Slightly familiar",
                                  "Somewhat familiar",
                                  "Very familiar",
                                  "Extremely familiar"))) %>%
    filter(!is.na(fam)) %>%
    crosstab(fam, group, percent = "col") %>%
    knitr::kable (
        format =  "pandoc",
        digits = 4,
        col.names = c("Familiarity with subject", "Blended students", "Other verified student"),
        align = "lrr",
        caption = "How familiar are you with the topics in this course? "
    )


# Plot as stacked bar charts centered at very familiar or higher
data_plot <- combined_data %>%
    mutate(fam= factor(fam,
                       levels = c("Not at all familiar",
                                  "Slightly familiar",
                                  "Somewhat familiar",
                                  "Extremely familiar",
                                  "Very familiar"))) %>%
    filter(!is.na(fam)) %>%
    crosstab(fam, group, percent = "col") %>%
    gather(group, pct, -fam) %>%
        mutate(pct = ifelse(fam != "Very familiar" & fam != "Extremely familiar",
                            pct * - 1, pct),
               top_2 = pct > 0) %>%
    group_by(group, top_2) %>%
        mutate(top_2_pct = sum(pct)) %>%
    ungroup()

ggplot(data = data_plot, aes(x = group, y = pct, fill = fam)) +
    geom_bar(stat = "identity", position = position_stack(), width = 0.4) +
    scale_fill_manual(values = c("#000000", "#8A8B8C", "#C2C0BF", "#40BDBF", "#A31F34"),
                      limits = c("Not at all familiar",
                                 "Slightly familiar",
                                 "Somewhat familiar",
                                 "Very familiar",
                                 "Extremely familiar")) +
    scale_x_discrete(labels = c("Blended students", "Other verified students")) +
    geom_text(aes(y = top_2_pct + 0.05,
                  label = ifelse(top_2, 
                                 paste0(round(top_2_pct, 2) * 100, "%"), NA)),
              size = 5) +
    geom_hline(yintercept = 0, size = 1) +
    theme_minimal() +
    theme(
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust= 0.5),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
        ) +
    ggtitle("How familiar are you with the content of this course?")
    


# COURSE ACTIONS------------------------------------------------------------------------------------

# Types of course actions
actions <- list("nevents", "ndays_act", "nplay_video", "npause_video",
                "nvideos_total_watched", "nchapters", "nforum_posts", "grade") 

actions_labels <- list("Number of events", "Number of days with activities",
                       "Number of video play events", "Number of times video paused",
                       "Percentage of videos watched a course",
                       "Number chapters visited",
                       "Number of forum posts", "Course Grade (%)")

# Among people who completed the course how unusual are blended students in their 
# course participation (excluse current residential students)
mapply(function(x, y) {
course_actions %>%
    gather(variable, value, nevents, ndays_act, nplay_video, npause_video, nvideos_total_watched,
           nchapters, nforum_posts, grade) %>%
    filter(variable == x) %>%
    filter(completed == 1 & !is.na(value)) %>%
    mutate(blended_binary = ifelse(!is.na(blended) & blended != F,
                                   "Blended students", "Other students")) %>%
    group_by(blended_binary) %>%
        summarise(mean_evens = mean(value),
                  sd_events = sd(value),
                  min_events = min(value),
                  max_events = max(value),
                  n = n()) %>%
        mutate(se_events = sd_events/sqrt(n)) %>%
        knitr::kable (
            format =  "pandoc",
            digits = 2,
            col.names = c("Student Category", "Mean", "SD", "Min", "Max", "N", "SE"),
            align = "lrrrr",
            caption = y
        )
    
}, x = actions, y = actions_labels, SIMPLIFY = F)

# Rerun looking at only students with a certificate as opposed to all completed students
mapply(function(x, y) {
    course_actions %>%
        gather(variable, value, nevents, ndays_act, nplay_video, npause_video, nvideos_total_watched,
               nchapters, nforum_posts, grade) %>%
        filter(variable == x) %>%
        filter(certified == 1 & !is.na(value)) %>%
        mutate(blended_binary = ifelse(!is.na(blended) & blended != F,
                                       "Blended students", "Other students")) %>%
        group_by(blended_binary) %>%
        summarise(mean_evens = mean(value),
                  sd_events = sd(value),
                  min_events = min(value),
                  max_events = max(value),
                  n = n()) %>%
        mutate(se_events = sd_events/sqrt(n)) %>%
        knitr::kable (
            format =  "pandoc",
            digits = 2,
            col.names = c("Student Category", "Mean", "SD", "Min", "Max", "N", "SE"),
            align = "lrrrr",
            caption = y
        )
    
}, x = actions, y = actions_labels, SIMPLIFY = F)



# Compare to all students who viewed the course (should be big difference)
mapply(function(x, y) {
    course_actions %>%
        gather(variable, value, nevents, ndays_act, nplay_video, npause_video, nvideos_total_watched,
               nchapters, nforum_posts, grade) %>%
        filter(variable == x) %>%
        filter(viewed == 1 & !is.na(value)) %>%
        mutate(blended_binary = ifelse(!is.na(blended) & blended != F,
                                       "Blended students", "Other students")) %>%
        group_by(blended_binary) %>%
        summarise(mean_evens = mean(value),
                  sd_events = sd(value),
                  min_events = min(value),
                  max_events = max(value),
                  n = n()) %>%
        mutate(se_events = sd_events/sqrt(n)) %>%
        knitr::kable (
            format =  "pandoc",
            digits = 2,
            col.names = c("Student Category", "Mean", "SD", "Min", "Max", "N", "SE"),
            align = "lrrrr",
            caption = y
        )
    
}, x = actions, y = actions_labels, SIMPLIFY = F)


# Compare to all students who explored the course (did half of the units)
mapply(function(x, y) {
    course_actions %>%
        gather(variable, value, nevents, ndays_act, nplay_video, npause_video, nvideos_total_watched,
               nchapters, nforum_posts, grade) %>%
        filter(variable == x) %>%
        filter(explored == 1 & !is.na(value)) %>%
        mutate(blended_binary = ifelse(!is.na(blended) & blended != F,
                                       "Blended students", "Other students")) %>%
        group_by(blended_binary) %>%
        summarise(mean_evens = mean(value),
                  sd_events = sd(value),
                  min_events = min(value),
                  max_events = max(value),
                  n = n()) %>%
        mutate(se_events = sd_events/sqrt(n)) %>%
        knitr::kable (
            format =  "pandoc",
            digits = 2,
            col.names = c("Student Category", "Mean", "SD", "Min", "Max", "N", "SE"),
            align = "lrrrr",
            caption = y
        )
    
}, x = actions, y = actions_labels, SIMPLIFY = F)


# PLOT DIFFERENCES IN COURSE ACTIONS BY PARCTIPANT TYPE---------------------------------------------

# Calculate percentile rank for each action to standardize them on the same scale
course_actions <- course_actions %>%
    mutate(
           percentile_ndays_act = ntile(ndays_act, 100),
           percentile_nvideo = ntile(nvideo, 100),
           percentile_nevents = ntile(nevents, 100),
           percentile_forums = ntile(nforum_posts, 100),
           percentile_grade = ntile(grade, 100)
    )

# Calculate percentile for each group
# Viewed - but did fewer than half the units
# Explored - did half or more of the units but did not complete the course with a passing grade
# Completed - completed all the units in the course but did not earn a certficate
# Certified - completed all units and earned certificate but did not do micromasters or do blended program
# Blended - certified and accepted into blended program 

# Do not include people who did next step without having done previous (e.g. people who didn't complete
# course but are in the blended program)

group_types <- list("viewed", "explored", "completed", "certified", "blended")

course_actions <- course_actions %>%
    mutate(group_cat = case_when(
                        .$viewed == 1 & .$explored == 0  & .$completed == 0 & .$certified == 0 & .$blended == 0 ~ "viewed",
                        .$viewed == 1 & .$explored == 1 & .$completed == 0 & .$certified == 0 & .$blended == 0 ~ "explored",
                        .$viewed == 1 & .$explored == 1 & .$completed == 1  & .$certified == 0 & .$blended == 0 ~ "completed",
                        .$viewed == 1 & .$explored == 1 & .$completed == 1  & .$certified == 1 & .$blended == 0 ~ "certified",
                        .$viewed == 1 & .$explored == 1 & .$completed == 1  & .$certified == 1 & .$blended == 1 ~ "blended"
                        )
    )


# Regress coure actions using lognormal distribution
lm(log_nevents ~ factor(group_cat), data = course_actions) %>% summary()
lm(log_nvideo ~ factor(group_cat), data = course_actions) %>% summary()
lm(log_ndays_act ~ factor(group_cat), data = course_actions) %>% summary()


# Set up data plot                        
data_plot <- lapply(group_types, function(x) {
course_actions %>%
    filter(group_cat == x) %>%
    gather(variable, value, starts_with("percentile_")) %>%
    group_by(variable) %>%
        summarise(mean = mean(value, na.rm = T),
                  sd = sd(value, na.rm = T),
                  n = n(),
                  median = median(value, na.rm = T)) %>%
    mutate(se = sd/sqrt(n),
           hi_int =  mean + (2.58 * se),
           low_int = mean - (2.58 * se)) %>%
    ungroup() %>%
    mutate(group_label = x,
           variable_label = factor(variable,
                                levels = c("percentile_nevents",
                                           "percentile_nvideo",
                                           "percentile_ndays_act",
                                           "percentile_forums",
                                           "percentile_grade"),
                                labels = c("Number of Events",
                                           "Number of Video Actions",
                                           "Number of Days Participated in Course",
                                           "Number of Forum Posts",
                                           "Course Grade")))
    
}) %>% bind_rows()


# Create plot in ggplot
ggplot(data = filter(data_plot, variable != "percentile_forums" & variable != "percentile_grade"), aes(x = group_label, y = mean, fill = group_label, colour = group_label)) +
    geom_bar(stat = "identity", width = 0.4) +
    facet_grid(. ~ variable_label) +
    geom_text(aes(y = mean + 0.05, label = paste0(round(mean, 0), "%"),
                  size = 3),
                   vjust = -0.25) +
    scale_x_discrete(labels = c("Blended", "Certified", "Completed", "Explored",  "Viewed")) +
    scale_fill_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000")) +
    scale_colour_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000"), guide = F) +
    theme_minimal() +
    theme(
        text = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(hjust= 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.x = unit(2, "lines")
    ) +
    ggtitle("Average Percentile Rank for Course Actions by Participant Type")

# Forum Posts as seperate chart
ggplot(data = filter(data_plot, variable == "percentile_forums"), aes(x = group_label, y = mean, fill = group_label, colour = group_label)) +
    geom_bar(stat = "identity", width = 0.4) +
    geom_text(aes(y = mean + 0.05, label = paste0(round(mean, 0), "%"),
                  size = 3),
              vjust = -0.25) +
    scale_x_discrete(labels = c("Blended", "Certified", "Completed", "Explored",  "Viewed")) +
    scale_fill_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000")) +
    scale_colour_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000"), guide = F) +
    theme_minimal() +
    theme(
        text = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(hjust= 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.x = unit(2, "lines")
    ) +
    ggtitle("Average Percentile Rank for Number of Forum Posts by Participant Type")


# Forum Posts as seperate chart
ggplot(data = filter(data_plot, variable == "percentile_forums"), aes(x = group_label, y = mean, fill = group_label, colour = group_label)) +
    geom_bar(stat = "identity", width = 0.4) +
    geom_text(aes(y = mean + 0.05, label = paste0(round(mean, 0), "%"),
                  size = 3),
              vjust = -0.25) +
    scale_x_discrete(labels = c("Blended", "Certified", "Completed", "Explored",  "Viewed")) +
    scale_fill_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000")) +
    scale_colour_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000"), guide = F) +
    theme_minimal() +
    theme(
        text = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(hjust= 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.x = unit(2, "lines")
    ) +
    ggtitle("Average Percentile Rank for Number of Forum Posts by Participant Type")


# Grades as seperate chart
ggplot(data = filter(data_plot, variable == "percentile_grade"), aes(x = group_label, y = mean, fill = group_label, colour = group_label)) +
    geom_bar(stat = "identity", width = 0.4) +
    geom_text(aes(y = mean + 0.05, label = paste0(round(mean, 0), "%"),
                  size = 3),
              vjust = -0.25) +
    scale_x_discrete(labels = c("Blended", "Certified", "Completed", "Explored",  "Viewed")) +
    scale_fill_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000")) +
    scale_colour_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000"), guide = F) +
    theme_minimal() +
    theme(
        text = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(hjust= 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.x = unit(2, "lines")
    ) +
    ggtitle("Average Percentile Rank for Course Grade by Participant Type")



# Plot these faceted by course - recalculate percentile within course
data_plot_course <- lapply(group_types, function(x) {
    course_actions %>%
        group_by(course_id) %>%
        mutate(
            percentile_ndays_act = ntile(ndays_act, 100),
            percentile_nvideo = ntile(nvideo, 100),
            percentile_nevents = ntile(nevents, 100),
            percentile_forums = ntile(nforum_posts, 100),
            percentile_grade = ntile(grade, 100)
        ) %>%
        ungroup() %>%
        filter(group_cat == x) %>%
        gather(variable, value, starts_with("percentile_")) %>%
        group_by(course_id, variable) %>%
        summarise(mean = mean(value, na.rm = T),
                  sd = sd(value, na.rm = T),
                  n = n(),
                  median = median(value, na.rm = T)) %>%
        mutate(se = sd/sqrt(n),
               hi_int =  mean + (2.58 * se),
               low_int = mean - (2.58 * se)) %>%
        ungroup() %>%
        mutate(group_label = x,
               variable_label = factor(variable,
                                       levels = c("percentile_nevents",
                                                  "percentile_nvideo",
                                                  "percentile_ndays_act",
                                                  "percentile_forums",
                                                  "percentile_grade"),
                                       labels = c("Number of Events",
                                                  "Number of Video Actions",
                                                  "Number of Days Participated in Course",
                                                  "Number of Forum Posts",
                                                  "Course Grade")))
    
}) %>% bind_rows()


# Create plot of nevnets, nvideo, an ndays_act in ggplot by course_id
ggplot(data = filter(data_plot_course, variable != "percentile_forums" & variable != "percentile_grade"), aes(x = group_label, y = mean, fill = group_label, colour = group_label)) +
    geom_bar(stat = "identity", width = 0.4) +
    facet_grid(course_id ~ variable_label, switch = "y") +
    geom_text(aes(y = mean + 0.05, label = paste0(round(mean, 0), "%")),
                  size = 3,
              vjust = -0.25) +
    scale_y_continuous(limits = c(0, 110)) +
    scale_x_discrete(labels = c("Blended", "Certified", "Completed", "Explored",  "Viewed")) +
    scale_fill_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000")) +
    scale_colour_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000"), guide = F) +
    theme_minimal() +
    theme(
        text = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(hjust= 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.text.y = element_text(angle = 180),
        panel.spacing.x = unit(2, "lines")
    ) +
    ggtitle("Average Percentile Rank for Course Actions by Participant Type and Course")

# Participation in community forums by course id
ggplot(data = filter(data_plot_course, variable == "percentile_forums"), aes(x = group_label, y = mean, fill = group_label, colour = group_label)) +
    geom_bar(stat = "identity", width = 0.4) +
    facet_grid(. ~ course_id) +
    coord_flip() +
    geom_text(aes(y = mean + 0.05, label = paste0(round(mean, 0), "%"),
                  size = 3),
              hjust = -0.10) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_discrete(labels = c("Blended", "Certified", "Completed", "Explored",  "Viewed")) +
    scale_fill_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000")) +
    scale_colour_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000"), guide = F) +
    theme_minimal() +
    theme(
        text = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(hjust= 0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.x = unit(2, "lines")
    ) +
    ggtitle("Average Percentile Rank for Number of Forum Posts by Participant Type and Course")


# Course grade by course id
ggplot(data = filter(data_plot_course, variable == "percentile_grade"), aes(x = group_label, y = mean, fill = group_label, colour = group_label)) +
    geom_bar(stat = "identity", width = 0.4) +
    facet_grid(. ~ course_id) +
    coord_flip() +
    geom_text(aes(y = mean + 0.05, label = paste0(round(mean, 0), "%"),
                  size = 3),
              hjust = -0.10) +
    scale_y_continuous(limits = c(0, 115)) +
    scale_x_discrete(labels = c("Blended", "Certified", "Completed", "Explored",  "Viewed")) +
    scale_fill_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000")) +
    scale_colour_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000"), guide = F) +
    theme_minimal() +
    theme(
        text = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(hjust= 0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.x = unit(2, "lines")
    ) +
    ggtitle("Average Percentile Rank for Course Grade by Participant Type and Course")




# Remove unnecessary datasets
rm(data_plot, data_plot_couse)



# PARTICIPANT BACKGROUND INFO-----------------------------------------------------------------------

# How do participant charachteristics vary by type of participant?

# Percent of participants from US/Canada
data_plot <- course_actions %>%
    mutate(us_canada = cc_by_ip == "US" | cc_by_ip == "CN") %>%
    group_by(group_cat) %>%
        summarise(pct = mean(us_canada, na.rm = T)) %>%
    mutate(group_cat = factor(group_cat,
                              levels = c("blended", "certified", "completed",
                                         "explored", "viewed"),
                              labels = c("Blended", "Certified", "Completed",
                                         "Explored", "Viewed"))) %>%
    filter(!is.na(group_cat))


ggplot(data = data_plot, aes(x = group_cat, y = pct, fill = group_cat, colour = group_cat)) +
    geom_bar(stat = "identity", width = 0.4) +
    geom_text(aes(y = pct + 0.01, label = paste0(round(pct, 2) * 100, "%"),
                  size = 3)) +
    scale_fill_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000")) +
    scale_colour_manual(values = c( "#A31F34",  "#40BDBF", "#C2C0BF", "#8A8B8C", "#000000"), guide = F) +
    theme_minimal() +
    theme(
        text = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(hjust= 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.x = unit(2, "lines")
    ) +
    ggtitle("Percent of Participants From USA/Canada")
