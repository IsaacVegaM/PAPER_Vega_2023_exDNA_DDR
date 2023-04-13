# JA curve

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
data <- read.csv("4_ja_curve/ja_curve.csv")

# Make independent variable a factor
data <- data %>% 
  mutate(treatment = factor(treatment,
                            levels = c("ctrl", "col", "cvi", "br")),
         time_factor = (factor(time))
         )

# Summary statistics
data_ss <- data %>% 
  group_by(treatment, time) %>% 
  get_summary_stats() %>% 
  arrange(desc(mean))

# Two way anova
anova <- aov(ja ~ treatment*time_factor, data = data)
summary(anova)

export::table2csv(anova, "4_ja_curve/anova_results_curve.csv")


# Tukey
tukey <- TukeyHSD(anova)
tukey

tukey_capture <- as.data.frame(tukey[3])
write.csv(tukey_capture, "4_ja_curve//tukey.csv")

# Compact letter display
tukey.cld <- multcompLetters4(anova, tukey)

cld <- as.data.frame.list(tukey.cld$`treatment:time_factor`)
data_ss$tukey <- cld$Letters


# Treatment labels
treatment_labels = c("Control", "Col-0", "Cvi-0", "Br")

# Plot colors
plot_colors <- c("#000000", # black
                 "#D10101", # red
                 "#404040", # dark grey
                 "#A6A6A6") # light


# Plot
ggplot(data_ss, aes(x = time, y = mean)) +
  # Geoms
  geom_line(
    aes(linetype = treatment, group = treatment, color = treatment)
  ) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se, color = treatment),
    width = 0.4
  ) +
  geom_point(
    aes(fill = treatment, shape = treatment),
    size = 2
  ) +
  # P-Value
  geom_text_repel(
    aes(label = tukey),
    min.segment.length = Inf,
    size = 3
  ) +
  # Scales
  scale_fill_manual(
    values = plot_colors,
    labels = treatment_labels
    ) +
  scale_color_manual(
    values = plot_colors,
    labels = treatment_labels
    ) +
  scale_shape_manual(
    values = c(21:24),
    labels = treatment_labels
    ) +
  scale_linetype_manual(
    values = c("dashed", "solid", "dotdash", "twodash"),
    labels = treatment_labels
    ) +
  scale_x_continuous(
    breaks = unique(data$time),
    guide = "prism_offset"
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, 5),
    guide = "prism_offset_minor"
  ) +
  # Labs
  labs(
    #title = "Jasmonic Acid induction",
    x = "Minutes",
    y = bquote("JA ng · g"^-1*" FW"),
    #caption = "Tukey test, p < 0.05, n = 5",
    fill  = "DNA source",
    color = "DNA source",
    shape = "DNA source",
    linetype = "DNA source"
    ) +
  # Theme
  theme_prism(
    base_line_size = 0.5,
    base_fontface = "plain"
  ) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.position = c(0.8, 0.8),
    plot.title = element_text(size = 14),
    aspect.ratio = 1,
    #axis.title = element_blank()
  )

ggsave("4_ja_curve/ja_curve.pdf", width = 4.5, height = 4.5, scale = 1, device = cairo_pdf)
  
save.image(file = "4_ja_curve/ja_curve.RData")