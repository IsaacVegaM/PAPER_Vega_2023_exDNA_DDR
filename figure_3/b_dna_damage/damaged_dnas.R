# Damaged DNAs ROS

# Load required libraries
# Cleaning and manipulating data
library(dplyr)

# Statistic tests
library(rstatix)
library(multcompView)

# Plotting
library(ggplot2)
library(ggbeeswarm)
library(ggprism)

# Load data
data <- read.csv("7_damaged_dnas/damaged_dnas.csv")

# Treatment levels and labels
treatment_levels <- c(
  "ctl",
  "fr",
  "D26",
  "D27",
  "DC3000",
  "bleomycin",
  "SA",
  "h2o2",
  "mechanical"
)

treatment_labs <- c(
  "Control",
  "Sonication",
  "avrRpt2 (-)",
  "avrRpt2 (+)",
  "DC3000",
  "Bleomycin",
  "SA",
  "H2O2",
  "MD"
)

# Make independent variable a factor
data <- data %>% 
  mutate(treatment = factor(treatment,
                            levels = treatment_levels))
str(data)

# Make independent variable a factor
data <- data %>% 
  mutate(treatment = factor(treatment,
                            levels = treatment_levels))
str(data)

# Summary statistics
data_ss <- data %>% 
  select(treatment, ros) %>% 
  group_by(treatment) %>% 
  get_summary_stats() %>% 
  arrange(desc(mean))

# Anova
anova <- aov(ros ~ treatment, data = data)
summary(anova)
export::table2csv(anova, "7_damaged_dnas/anova_result.csv")


capture_anova <- summary(anova)
capture.output(capture_anova, file = "7_damaged_dnas/anova_results.doc")

# Tukey
tukey <- TukeyHSD(anova)
tukey

tukey_capture <- as.data.frame(tukey[1])
write.csv(tukey_capture, "7_damaged_dnas/tukey.csv")


# Compact letter display
tukey.cld <- multcompLetters4(anova, tukey)

cld <- as.data.frame.list(tukey.cld$treatment)
data_ss$tukey_letters <- cld$Letters

# Colors
plot_colors <- c(
  "#000000",
  "#D10101",
  "#4DBBD5FF",
  "#00A087FF",
  "#3C5488FF",
  "#F39B7FFF",
  "#8491B4FF",
  "#91D1C2FF",
  "#E64B35FF",
  "#7E6148FF",
  "#B09C85FF"
)

# Plot
ggplot(data, aes(x = treatment, y = ros)) +
  # Geom
  stat_summary(
    aes(color = treatment),
    fun = "mean",
    geom = "crossbar",
    alpha = 0.8,
    width = 0.8
  ) +
  geom_beeswarm(
    aes(fill = treatment),
    shape = 21,
    cex = 3.5,
    size = 2,
    alpha = 0.8
  ) +
  # P-value
  geom_text(
    data = data_ss,
    aes(x = treatment, y = max, label = tukey_letters),
    nudge_y = 3,
    size = 2
  ) +
  # Scales
  scale_fill_manual(values = plot_colors) +
  scale_color_manual(values = plot_colors) +
  scale_x_discrete(
    guide = guide_prism_bracket(angle = 45),
    labels = treatment_labs
  ) +
  scale_y_continuous(
    guide = "prism_offset_minor",
    limits = c(20, 60),
    breaks = seq(20, 60, 10)
    ) +
  # Labels
  labs(
    #title = "ROS response to leaked DNA",
    x = "Damaged DNA",
    y = bquote(H[2]*O[2]*" nmol·g"^-1*" FW"),
    #caption = "Tukey test, p < 0.05, n = 3"
  ) +
  # Theme
  theme_prism(
    base_line_size = 0.35,
    base_fontface = "plain"
  ) +
  theme(
    text = element_text(size =8),
    legend.position = "none",
    # axis.text.x = element_blank(),
    # axis.title = element_blank()
  )

ggsave("7_damaged_dnas/damaged_dnas.pdf", width = 9, height = 6, units = "cm", scale = 1, dpi = 300, device = cairo_pdf)


save.image(file = "7_damaged_dnas/damaged_dnas.RData")