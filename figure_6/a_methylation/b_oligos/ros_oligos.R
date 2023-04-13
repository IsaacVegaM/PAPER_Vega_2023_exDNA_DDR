# ROS oligo DNAs

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
data <- read.csv("9_oligos/ros_oligos.csv") %>% 
  select(treatment, methylation, ros)

# Create levels and labels for factors
treatment_levels <- c("control", "2006s", "2006c", "2006ds","504s", "504c", "504ds")
treatment_labels <- c("Control", "2006s", "2006c", "2006ds","504s", "504c", "504ds")

methylation_levels <- c("control", "CG", "nonCG" )
methylation_labels <- c("Control", "CG", "non-CG")

# Create factors
data <- data %>% 
  mutate(
    methylation = factor(methylation,
                       levels = methylation_levels,
                       labels = methylation_labels),
    treatment = factor(treatment,
                       levels = treatment_levels,
                       labels = treatment_labels)
    )

# Summary statistics
data_ss <- data %>% 
  group_by(treatment) %>% 
  get_summary_stats() %>% 
  arrange(desc(mean))

# One way anova
anova <- aov(ros ~ treatment, data = data)
summary(anova)
export::table2csv(anova, "9_oligos/anova_result.csv")



capture_anova <- summary(anova)
capture.output(capture_anova, file = "9_oligos/anova_results_oligos.doc")

# Tukey
tukey <- TukeyHSD(anova)
tukey

tukey_capture <- as.data.frame(tukey[1])
write.csv(tukey_capture, "9_oligos/tukey.csv")

# Compact letter display
tukey.cld <- multcompLetters4(anova, tukey)

cld <- as.data.frame.list(tukey.cld$treatment)
data_ss$tukey <- cld$Letters



# Plot colors
plot_colors <- c("#000000", # black
                 "#A6A6A6",  # light 
                 "#404040" # dark grey
                 )


# Plot
ggplot(data, aes(x = treatment, y = ros)) +
  # Geom
  stat_summary(
    aes(color = methylation),
    fun = "mean",
    geom = "crossbar",
    alpha = 0.8,
    width = 0.6,
    show.legend = FALSE
  ) +
  geom_beeswarm(
    aes(fill = methylation),
    shape = 21,
    cex = 3.5,
    size = 2,
    alpha = 0.8
  ) +
  # P-value
  geom_text(
    data = data_ss,
    aes(x = treatment, y = max, label = tukey),
    #nudge_x = 0.375,
    nudge_y = 5,
    size = 2
  ) +
  # Scales
  scale_color_manual(values = plot_colors) +
  scale_fill_manual(values = plot_colors) +
  scale_x_discrete(guide = guide_prism_bracket(angle = 45)) +
  scale_y_continuous(
    guide = "prism_offset_minor",
    limits = c(10, 80),
    ) +
  # Labs
  labs(
    x = "DNA source",
    y = bquote(H[2]*O[2]*" [nmol · g"^-1*" FW]"),
    #caption = "Tukey test, p < 0.05, n = 5"
  ) +
  # Theme
  theme_prism(
    base_line_size = 0.35,
    base_fontface = "plain"
  ) +
  theme(
    text = element_text(size = 8),
    #legend.position = "none",
    legend.position = c(0.5, 0.90),
    legend.direction = "horizontal",
    legend.box.background = element_rect(color = "black"),
    # axis.title = element_blank(),
    # axis.text.x = element_blank()
  )

ggsave("9_oligos/ros_met_oligos.pdf", width = 9, height = 4.1, units = "cm", scale = 1, dpi = 300, device = cairo_pdf)

save.image(file = "9_oligos/anova_result.RData")