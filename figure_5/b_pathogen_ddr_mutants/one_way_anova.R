# Pathogen mutants

# Load required libraries
# Cleaning and manipulating data
library(dplyr)

# Statistic tests
library(rstatix)
library(multcompView)

# Read data
data_wt <- read.csv("10_pathogen_mutants/pathogen_mutants.csv") %>% 
  select("treatment", "genotype", "cfu") %>% 
  filter(genotype == "wt")

data_atm <- read.csv("10_pathogen_mutants/pathogen_mutants.csv") %>% 
  select("treatment", "genotype", "cfu") %>% 
  filter(genotype == "atm")

data_atr <- read.csv("10_pathogen_mutants/pathogen_mutants.csv") %>% 
  select("treatment", "genotype", "cfu") %>% 
  filter(genotype == "atr")

# Create levels and labels for factors
treatment_levels <- c("ctl", "col", "cvi", "br")

# Create factors
data_wt <- data_wt %>% 
  mutate(
    genotype = factor(genotype),
    treatment = factor(treatment, levels = treatment_levels)
    )

data_atm <- data_atm %>% 
  mutate(
    genotype = factor(genotype),
    treatment = factor(treatment, levels = treatment_levels)
  )

data_atr <- data_atr %>% 
  mutate(
    genotype = factor(genotype),
    treatment = factor(treatment, levels = treatment_levels)
  )

# One way anova
anova_wt <- aov(cfu ~ treatment, data = data_wt)
summary(anova_wt)

anova_atm <- aov(cfu ~ treatment, data = data_atm)
summary(anova_atm)

anova_atr <- aov(cfu ~ treatment, data = data_atr)
summary(anova_atr)


export::table2csv(anova_wt, "10_pathogen_mutants/anova_wt.csv")
export::table2csv(anova_atm, "10_pathogen_mutants/anova_atm.csv")
export::table2csv(anova_atr, "10_pathogen_mutants/anova_atr.csv")

capture_anova_wt <- summary(anova_wt)
capture.output(capture_anova_wt, file = "10_pathogen_mutants/anova_wt.doc")

capture_anova_atm <- summary(anova_atm)
capture.output(capture_anova_atm, file = "10_pathogen_mutants/anova_atm.doc")

capture_anova_atr <- summary(anova_atr)
capture.output(capture_anova_atr, file = "10_pathogen_mutants/anova_atr.doc")

save.image(file = "10_pathogen_mutants/one_way_anova.RData")
