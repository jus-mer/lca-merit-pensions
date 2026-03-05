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
                just_pension = des_11) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as_tibble()

# recode and transform ----

# set nas values
db <- db %>% 
  mutate(
    across(
      .cols = -c(id),
      .fns = ~ set_na(., na = c(5,6))
    )
  )


# meritocracy

frq(db$perc_effort)
frq(db$perc_talent)
frq(db$perc_rich_parents)
frq(db$perc_contact)

frq(db$pref_effort)
frq(db$pref_talent)
frq(db$pref_rich_parents)
frq(db$pref_contact)

labels1 <- c("Muy en desacuerdo" = 1, 
             "En desacuerdo" = 2, 
             "De acuerdo" = 3, 
             "Muy de acuerdo" = 4)

db <- db %>% 
  mutate_at(.vars = (1:8),.funs = ~ sjlabelled::set_labels(., labels = labels1))

db <- db %>% 
  mutate(across(
    1:8,
    ~ if_else(. >= 3, 1L, 0L),
    .names = "{.col}_d"   # perc_effort -> perc_effort_d, etc.
  ))

# market justice pensions

frq(db$just_pension)

db$just_pension <- sjlabelled::set_labels(db$just_pension, labels = labels1)

# missings ----

colSums(is.na(db))

prop_miss(db)*100

miss_var_summary(db)

miss_var_table(db)

miss_case_table(db)

vis_miss(db) + theme(axis.text.x = element_text(angle=80))

# 4. Save and export ------------------------------------------------------

save(db, file = here("input/data/proc/db_proc.RData"))
