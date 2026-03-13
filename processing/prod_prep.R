# 0. Identification ---------------------------------------------------

# Title: LCA for Merit-scale code of EDUMERCO data
# Institution: JUSMER
# Responsible: Andreas Laffert

# Executive Summary: This script contains the code for run an LCA for merit scale in EDUMERCO data
# Date: March 5, 2026

# 1. Packages  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjmisc, 
               here,
               sjlabelled,
               naniar)


options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(url("https://github.com/educacion-meritocracia/encuesta-edumerco/raw/refs/heads/main/input/data/original/completas-270125.RData"))

glimpse(data)
names(data)

# 3. Processing -----------------------------------------------------------

# select ----

db <- data %>% 
  dplyr::select(perc_effort = merit_01, 
                perc_talent = merit_02, 
                perc_rich_parents = merit_03,
                perc_contact = merit_04, 
                pref_effort = merit_05, 
                pref_talent = merit_06, 
                pref_rich_parents = merit_07, 
                pref_contact = merit_08,
                just_pension = des_11,
                income = carac_a10, 
                age = screen_age,
                sex = screen_sex,
                educ = screen_education) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as_tibble()

# recode and transform ----

# set nas values
db <- db %>% 
  mutate(
    across(
      .cols = -c(id, age),
      .fns = ~ set_na(., na = c(5,6))
    )
  )

# Age
frq(db$age) #ok

# Educ
frq(db$educ)#ok

db$educ_f <- as.factor(db$educ)

# Sex
frq(db$sex)

db <- db %>% 
  mutate(sex = if_else(sex >= 3, NA, sex),
         sex = as.factor(sex)
  )

# Income
frq(db$income)

db$income <- factor(db$income) #recategorizar

# meritocracy

frq(db$perc_effort)
frq(db$perc_talent)
frq(db$perc_rich_parents)
frq(db$perc_contact)

frq(db$pref_effort)
frq(db$pref_talent)
frq(db$pref_rich_parents)
frq(db$pref_contact)

labels1 <- c("Strongly desagree" = 1, 
             "Desagree" = 2, 
             "Agree" = 3, 
             "Strongly agree" = 4)
db <- db %>% 
  mutate_at(.vars = (1:8),.funs = ~ sjlabelled::set_labels(., labels = labels1))

db <- db %>% 
  mutate(across(
    1:8,
    ~ if_else(. >= 3, 1, 0),
    .names = "{.col}_d"   # perc_effort -> perc_effort_d, etc.
  ))


db <- db %>% 
  mutate(
    across(.cols = ends_with("_d"),
           .fns = ~ as.factor(.))
  )

db <- db %>% 
  mutate_at(.vars = 1:8,
            .funs = ~ factor(., 
                             levels = 1:4, 
                             labels = c("Strongly desagree",
                                        "Desagree",                  
                                        "Agree",                     
                                        "Strongly agree")))


db <- db %>% 
  mutate_at(.vars = 16:23,
            .funs = ~ factor(., 
                             levels = 0:1, 
                             labels = c("Low", "High")))

# market justice pensions

frq(db$just_pension)

db$just_pension <- sjlabelled::set_labels(db$just_pension, labels = labels1)

db$just_pension <- factor(db$just_pension, levels = 1:4, 
                          labels = c("Strongly desagree",
                                     "Desagree",                  
                                     "Agree",                     
                                     "Strongly agree"))

# missings ----

colSums(is.na(db))

prop_miss(db)*100

miss_var_summary(db)

miss_var_table(db)

miss_case_table(db)

vis_miss(db) + theme(axis.text.x = element_text(angle=80))


# label ----
db$perc_effort <- sjlabelled::set_label(db$perc_effort, 
                                                label = "In Chile people are rewarded for their efforts")


db$perc_talent <- sjlabelled::set_label(db$perc_talent, 
                                        label = "In Chile people are rewarded for their intelligence and ability")

db$perc_rich_parents <- sjlabelled::set_label(db$perc_rich_parents, 
                                        label = "In Chile those with wealthy parents do much better in life")

db$perc_contact <- sjlabelled::set_label(db$perc_contact, 
                                        label = "In Chile those with good contacts do much better in life")


db$perc_effort_d <- sjlabelled::set_label(db$perc_effort_d, 
                                          label = "In Chile people are rewarded for their efforts")


db$perc_talent_d <- sjlabelled::set_label(db$perc_talent_d, 
                                          label = "In Chile people are rewarded for their intelligence and ability")

db$perc_rich_parents_d <- sjlabelled::set_label(db$perc_rich_parents_d, 
                                                label = "In Chile those with wealthy parents do much better in life")

db$perc_contact_d <- sjlabelled::set_label(db$perc_contact_d, 
                                           label = "In Chile those with good contacts do much better in life")

db$pref_effort <- sjlabelled::set_label(db$pref_effort, 
                                        label = "Those who work harder should reap greater rewards than those who work less hard")


db$pref_talent <- sjlabelled::set_label(db$pref_talent, 
                                        label = "Those with more talent should reap greater rewards than those with less talent")

db$pref_rich_parents <- sjlabelled::set_label(db$pref_rich_parents, 
                                              label = "It is good that those who have rich parents do better in life")

db$pref_contact <- sjlabelled::set_label(db$pref_contact, 
                                         label = "It is good that those who have good contacts do better in life")


db$pref_effort_d <- sjlabelled::set_label(db$pref_effort_d, 
                                          label = "Those who work harder should reap greater rewards than those who work less hard")


db$pref_talent_d <- sjlabelled::set_label(db$pref_talent_d, 
                                          label = "Those with more talent should reap greater rewards than those with less talent")

db$pref_rich_parents_d <- sjlabelled::set_label(db$pref_rich_parents_d, 
                                                label = "It is good that those who have rich parents do better in life")

db$pref_contact_d <- sjlabelled::set_label(db$pref_contact_d, 
                                           label = "It is good that those who have good contacts do better in life")


# 4. Save and export ------------------------------------------------------

db <- db %>% 
  dplyr::select(id, just_pension, starts_with(c("perc", "pref")), age, sex, educ, income)

save(db, file = here("input/data/proc/db_proc.RData"))
