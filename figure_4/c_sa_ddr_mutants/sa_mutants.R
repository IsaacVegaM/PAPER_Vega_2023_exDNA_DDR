# SA mutants

# Load required libraries
# Cleaning and manipulating data
library(dplyr)

# Statistic tests
library(rstatix)
library(multcompView)

# Plotting
library(ggplot2)
library(ggprism)
library(ggbeeswarm)
library(ggtext)



# Read data
data <- read.csv("13_sa_mutants/sa_mutants.csv")

# Create levels and labels for factors
genotype_levels <- c("wt", "atm", "atr")
genotype_labels <- c("wt", "*atm*", "*atr*")

treatment_levels <- c("ctl", "col", "cvi", "br")
treatment_labels <- c("Control", "Col-0", "Cvi-0", "Br")

# Create factors
data <- data %>% 
  mutate(
    genotype = factor(genotype,
                      levels = genotype_levels,
                      labels = genotype_labels),
    treatment = factor(treatment,
                       levels = treatment_levels))

# Summary statistics
data_ss <- data %>% 
  group_by(treatment, genotype) %>% 
  get_summary_stats() %>% 
  arrange(desc(mean))

# Two way anova
anova <- aov(sa ~ treatment*genotype, data = data)
summary(anova)
export::table2csv(anova, "13_sa_mutants/two_way_anova_result.csv")



capture_anova <- summary(anova)
capture.output(capture_anova, file = "13_sa_mutants/anova_results.doc")

# Tukey
tukey <- TukeyHSD(anova)
tukey

tukey_capture <- as.data.frame(tukey[3])
write.csv(tukey_capture, "13_sa_mutants/tukey.csv")

# Compact letter display
tukey.cld <- multcompLetters4(anova, tukey)

cld <- as.data.frame.list(tukey.cld$`treatment:genotype`)
data_ss$tukey <- cld$Letters

# Colors
plot_colors <- c("#000000", # white
                 "#D10101", # red
                 "#404040", # dark grey
                 "#A6A6A6") # light 

# Plot 
ggplot(data, aes(x = treatment, y = sa)) +
  # Geoms
  stat_summary(
    aes(color = treatment),
    geom = "crossbar",
    fun = mean,
    width = 0.8
  ) +
  ggbeeswarm::geom_beeswarm(
    aes(fill = treatment),
    shape = 21,
    cex = 3.5,
    size = 2,
    alpha = 0.8
  ) +
  geom_text(
    data = data_ss,
    aes(x = treatment, y = max, label = tukey),
    nudge_y = 2,
    size = 2
  ) +
  # Facet
  facet_wrap(~ genotype) +
  # Scales
  scale_fill_manual(values = plot_colors) +
  scale_color_manual(values = plot_colors) +
  scale_x_discrete(
    guide = guide_prism_bracket(angle = 45),
    labels = treatment_labels
  ) +
  scale_y_continuous(
    guide = "prism_offset_minor",
    limits = c(0, 15),
    breaks = seq(0, 15, 2.5)
  ) +
  # Labels
  labs(
    #title = "SA induction of DDR mutants",
    x = "DNA treatment",
    y = bquote("SA [µg · g"^-1*" FW]"),
    #caption = "Tukey test, p < 0.05, n = 5"
  ) +
  # Theme
  theme_prism(
    base_fontface = "plain",
    base_line_size = 0.35
  ) +
  theme(
    text = element_text(size = 8),
    legend.position = "none",
    strip.text.x = element_blank(),
    # axis.text = element_blank(),
    # axis.title = element_blank()
  )

ggsave("13_sa_mutants/sa_mutants.pdf", width = 9, height = 4.5, units = "cm", scale = 1, dpi = 300, device = cairo_pdf)

save.image(file = "13_sa_mutants/sa_mutants.RData")