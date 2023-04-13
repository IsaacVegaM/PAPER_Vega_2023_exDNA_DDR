# JA 30 minutes

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
data <- read.csv("4_ja_curve/ja_curve.csv") %>% 
  filter(time == 30) %>% 
  select(treatment, ja)

# Make independent variable a factor
data <- data %>% 
  mutate(treatment = factor(treatment,
                            levels = c("ctrl", "col", "cvi", "br")))

# Summary statistics
data_ss <- data %>% 
  group_by(treatment) %>% 
  get_summary_stats() %>% 
  arrange(desc(mean))

# Anova
anova <- aov(ja ~ treatment, data = data)
summary(anova)
export::table2csv(anova, "4_ja_curve/anova_results_30min.csv")


capture_anova <- summary(anova)
capture.output(capture_anova, file = "4_ja_curve/anova_results.doc")

# Tukey
tukey <- TukeyHSD(anova)
tukey

tukey_capture <- as.data.frame(tukey[1])
write.csv(tukey_capture, "4_ja_curve/tukey_30.csv")

# Compact letter display
tukey.cld <- multcompLetters4(anova, tukey)

cld <- as.data.frame.list(tukey.cld$treatment)
data_ss$tukey_letters <- cld$Letters


# Colors
plot_colors <- c(
  #"#FFFFFF", # white
  "#000000", # black
  "#D10101", # red
  "#404040", # dark grey
  "#A6A6A6"  # light grey
)


# Plot
ggplot(data, aes(x = treatment, y = ja)) +
  #geom_violin(trim = F) +
  # Geom
  stat_summary(
    aes(color = treatment),
    fun = "mean",
    geom = "crossbar",
    #alpha = 0.8,
    width = 0.4
  ) +
  geom_beeswarm(
    aes(fill = treatment),
    shape = 21,
    cex = 3.5,
    size = 4,
    alpha = 0.8
  ) +
  # P-value
  geom_text(
    data = data_ss,
    aes(x = treatment, y = mean, label = tukey_letters),
    nudge_x = 0.275,
    nudge_y = 1.5,
    size = 5
  ) +
  # Scales
  scale_fill_manual(values = plot_colors) +
  scale_color_manual(values = plot_colors) +
  scale_x_discrete(
    guide = "prism_bracket",
    labels = c("Control", "Col-0", "Cvi-0", "Br")
  ) +
  scale_y_continuous(
    guide = "prism_offset_minor",
    limits = c(0, 20)
    ) +
  # Labs
  labs(
    x = "DNA source",
    y = bquote("JA [ng · g"^-1*" FW]"),
    #caption = "Tukey test, p < 0.05, n = 5"
  ) +
  # Theme
  theme_prism(
    base_line_size = 0.7,
    base_fontface = "plain"
  ) +
  theme(
    text = element_text(size = 16),
    legend.position = "none",
    plot.title = element_text(size = 14),
    aspect.ratio = 1,
    #axis.text.x = element_blank(),
    #axis.title.x = element_blank(),
    #axis.title.y = element_blank()
  )

ggsave("4_ja_curve/ja_30.pdf", width = 16, height = 16, units = "cm", scale = 0.5, dpi = 300, device = cairo_pdf)

save.image(file = "4_ja_curve/ja_30min.RData")
