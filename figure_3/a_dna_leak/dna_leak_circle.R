# DNA leak

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
library(ggsci)

# Load data
data <- read.csv("6_dna_leak/dna_leak_circle.csv")

# Treatment levels and labels
treatment_levels <- c(
  "ctrl",
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
  "Sonicated",
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

# Summary statistics
data_ss <- data %>% 
  select(treatment, concentration_disk) %>% 
  filter(!treatment == "fr") %>% 
  group_by(treatment) %>% 
  get_summary_stats() %>% 
  arrange(desc(mean))

# Anova
anova <- aov(concentration_disk ~ treatment, data = data)
summary(anova)
export::table2csv(anova, "6_dna_leak/anova_results.csv")


capture_anova <- summary(anova)
capture.output(capture_anova, file = "6_dna_leak/anova_results.doc")


plot_colors <- c(
  "#000000",
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
ggplot(data, aes(x = treatment, y = concentration_disk)) +
  # Geom
  stat_summary(
    aes(color = treatment),
    fun = "mean",
    geom = "crossbar",
    alpha = 0.8,
    width = 0.6
  ) +
  geom_beeswarm(
    aes(fill = treatment),
    shape = 21,
    cex = 3.5,
    size = 2,
    alpha = 0.8
  ) +
  geom_hline(
    yintercept = 5.88,
    linetype = "dashed"
  ) +
  annotate(
    geom = "text",
    x = 4,
    y = 5.68,
    label = "total DNA in leaf circle",
    size = 3
  ) +
  geom_hline(
    yintercept = 0.27,
    linetype = "dashed"
  ) +
  annotate(
    geom = "text",
    x = 4,
    y = 0.07,
    label = "DNA in circunference cells",
    size = 3
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
    #limits = c(0, 1)
  ) +
  # Labels
  labs(
    x = "Treatment",
    y = bquote("Leaked DNA [ng · leaf disk]"^-1),
    #title = "Leaked DNA concentration per leaf disk",
  ) +
  # Theme
  theme_prism(
    base_line_size = 0.35,
    base_fontface = "plain"
  ) +
  theme(
    text = element_text(size = 8),
    legend.position = "none",
    # axis.text.x = element_blank(),
    # axis.title = element_blank()
  )

ggsave("6_dna_leak/dna_leak_circle.pdf", width = 9, height = 6, units = "cm", scale = 1, dpi = 300, device = cairo_pdf)

save.image(file = "6_dna_leak/dna_leak_circle.RData")
