# SA 24h

# Load required libraries
# Cleaning and manipulating data
library(dplyr)

# Statistic tests
library(rstatix)
library(multcompView)

# Plotting
library(ggplot2)
library(ggprism)
library(ggrepel)

# Load data
data <- read.csv("5_sa_curve/sa_curve.csv") %>% 
  filter(time == 24) %>% 
  select(treatment, sa)


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
anova <- aov(sa ~ treatment, data = data)
summary(anova)
export::table2csv(anova, "5_sa_curve/anova_results_24min.csv")


capture_anova <- summary(anova)
capture.output(capture_anova, file = "5_sa_curve/anova_results.doc")

# Tukey
tukey <- TukeyHSD(anova)
tukey

tukey_capture <- as.data.frame(tukey[1])
write.csv(tukey_capture, "5_sa_curve/tukey_24.csv")

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
ggplot(data, aes(x = treatment, y = sa)) +
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
    nudge_x = 0.3,
    nudge_y = 0.75,
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
    limits = c(0, 12),
    breaks = seq(0, 12, 2)
  ) +
  # Labs
  labs(
    x = "DNA source",
    y = bquote("SA [µg · g"^-1*" FW]"),
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
    #axis.text.x = element_text(size = 10),
    # axis.text.x = element_blank(),
    # axis.title.x = element_blank(),
    # axis.title.y = element_blank()
  )

ggsave("5_sa_curve/sa_24h.pdf", width = 16, height = 16, units = "cm", scale = 0.5, dpi = 300, device = cairo_pdf)

save.image(file = "5_sa_curve/sa_24h.RData")
