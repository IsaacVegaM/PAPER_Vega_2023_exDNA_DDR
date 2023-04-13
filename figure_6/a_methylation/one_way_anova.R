# Pathogen mutants

# Load required libraries
# Cleaning and manipulating data
library(dplyr)

# Statistic tests
library(rstatix)
library(multcompView)

# Read data
data_natural <- read.csv("8_methylation/ros_met.csv") %>% 
  select("methylation", "treatment", "ros") %>% 
  filter(methylation == "natural")

data_methylated <- read.csv("8_methylation/ros_met.csv") %>% 
  select("methylation", "treatment", "ros") %>% 
  filter(methylation == "methylated")

data_pcr <- read.csv("8_methylation/ros_met.csv") %>% 
  select("methylation", "treatment", "ros") %>% 
  filter(methylation == "pcr")

# Create levels and labels for factors
treatment_levels <- c("ctl", "col", "cvi", "br")

# Create factors
data_natural <- data_natural %>% 
  mutate(
    genotype = factor(methylation),
    methylation = factor(treatment, levels = treatment_levels)
    )

data_methylated <- data_methylated %>% 
  mutate(
    genotype = factor(methylation),
    methylation = factor(methylation, levels = treatment_levels)
  )

data_pcr <- data_pcr %>% 
  mutate(
    genotype = factor(methylation),
    methylation = factor(methylation, levels = treatment_levels)
  )

# One way anova
anova_natural <- aov(ros ~ treatment, data = data_natural)
summary(anova_natural)

anova_methylated <- aov(ros ~ treatment, data = data_methylated)
summary(anova_methylated)

anova_pcr <- aov(ros ~ treatment, data = data_pcr)
summary(anova_pcr)


export::table2csv(anova_natural, "8_methylation/anova_natural.csv")
export::table2csv(anova_methylated, "8_methylation/anova_methylated.csv")
export::table2csv(anova_pcr, "8_methylation/anova_pcr.csv")

capture_anova_natural <- summary(anova_natural)
capture.output(capture_anova_natural, file = "8_methylation/anova_natural.doc")

capture_anova_methylated <- summary(anova_methylated)
capture.output(capture_anova_methylated, file = "8_methylation/anova_methylated.doc")

capture_anova_pcr <- summary(anova_pcr)
capture.output(capture_anova_pcr, file = "8_methylation/anova_pcr.doc")

# Tukey
tukey_natural <- TukeyHSD(anova_natural)
tukey_natural

tukey_methylated <- TukeyHSD(anova_methylated)
tukey_methylated

tukey_pcr <- TukeyHSD(anova_pcr)
tukey_pcr

save.image(file = "8_methylation/one_way_anova.RData")