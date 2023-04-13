# ROS methylated DNA

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

# Load data
data <- read.csv("8_methylation/ros_met.csv")

# Create levels and labels for factors
methylation_levels <- c("natural", "methylated", "pcr")
methylation_labels <- c("Natural", "Methylated", "PCR")

treatment_levels <- c("ctl", "col", "cvi", "br")
treatment_labels <- c("Control", "Col-0", "Cvi-0", "Br")

# Create factors
data <- data %>% 
  mutate(
    treatment = factor(treatment,
                      levels = treatment_levels),
    methylation = factor(methylation,
                       levels = methylation_levels,
                       labels = methylation_labels))


# Summary statistics
data_ss <- data %>% 
  select(treatment, methylation, ros) %>% 
  group_by(treatment, methylation) %>% 
  get_summary_stats() %>% 
  arrange(desc(mean))

# Two way anova
anova <- aov(ros ~ methylation*treatment, data = data)
summary(anova)
export::table2csv(anova, "8_methylation/two_way_anova_result.csv")


capture_anova <- summary(anova)
capture.output(capture_anova, file = "8_methylation/anova_results.doc")

# Tukey
tukey <- TukeyHSD(anova)
tukey

tukey_capture <- as.data.frame(tukey[3])
write.csv(tukey_capture, "8_methylation/tukey.csv")

# Compact letter display
tukey.cld <- multcompLetters4(anova, tukey)

cld <- as.data.frame.list(tukey.cld$`methylation:treatment`)
data_ss$tukey <- cld$Letters

# Colors
plot_colors <- c("#000000", # black
                 "#D10101", # red
                 "#404040", # dark grey
                 "#A6A6A6") # light 


# Plot
ggplot(data, aes(x = treatment, y = ros)) +
  # Geoms
  stat_summary(
    aes(color = treatment),
    geom = "crossbar",
    fun = mean
  ) +
  geom_beeswarm(
    aes(fill = treatment),
    shape = 21,
    cex = 3.5,
    size = 2,
    alpha = 0.8
  ) +
  geom_text(
    data = data_ss,
    aes(x = treatment, y = max, label = tukey),
    #nudge_x = 0.375,
    nudge_y = 6,
    size = 2
  ) +
  # Facet
  facet_wrap(~ methylation) +
  # Scales
  scale_fill_manual(values = plot_colors) +
  scale_color_manual(values = plot_colors) +
  scale_x_discrete(
    guide = guide_prism_bracket(angle = 45),
    labels = treatment_labels
  ) +
  scale_y_continuous(
    guide = "prism_offset_minor",
    limits = c(10, 80)
    ) +
  # Labels
  labs(
    #title = "ROS response to different DNA methylation status",
    x = "DNA treatment",
    y = bquote(H[2]*O[2]*" [nmol · g"^-1*" FW]"),
    #caption = "Tukey test, p < 0.05, n = 5"
    ) +
  # Theme
  theme_prism(
    base_fontface = "plain",
    base_line_size = 0.35
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 8),
    # axis.title = element_blank(),
    # axis.text.x = element_blank()
  )

ggsave("8_methylation/ros_met.pdf", width = 9, height = 4.5, units = "cm", scale = 1, dpi = 300, device = cairo_pdf)

save.image(file = "8_methylation/ros_met.RData")