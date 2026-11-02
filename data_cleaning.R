# ---- 1. Setup ----
install.packages(c("tidyverse", "janitor","scales","forcats","gt"))
library(tidyverse)
library(janitor)
library(scales)
library(forcats)
library(gt)

# ---- 2. Load & clean ----
raw <- read_csv("data/children-below-18-by-parents-survival-status.csv")

glimpse(raw)

df <- raw %>%
  clean_names() %>%
  select(
    code,
    level,
    district = district_3,
    both_alive,
    mother_alive_father_dead,
    father_alive_mother_dead,
    both_dead
  ) %>%
  mutate(
    across(
      c(
        both_alive,
        mother_alive_father_dead,
        father_alive_mother_dead,
        both_dead
      ),
      ~ as.numeric(str_remove_all(.x, ","))
    )
  ) %>%
  filter(level == "district") %>%
  mutate(
    total_children = both_alive +
      mother_alive_father_dead +
      father_alive_mother_dead +
      both_dead,
    
    orphan_total = mother_alive_father_dead +
      father_alive_mother_dead +
      both_dead,
    
    orphan_pct = orphan_total / total_children,
    
    double_orphan_pct = both_dead / total_children,
    
    maternal_orphan = father_alive_mother_dead,
    
    paternal_orphan = mother_alive_father_dead,
    
    maternal_share = maternal_orphan / orphan_total,
    
    paternal_share = paternal_orphan / orphan_total
  )

glimpse(df)

# ---- 3. Sanity checks ----
summary(df$total_children)
sum(duplicated(df$district))