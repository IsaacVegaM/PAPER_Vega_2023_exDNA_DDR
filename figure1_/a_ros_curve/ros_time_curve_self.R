# ROS time curve self
# Two way ANOVA

# Load required libraries

# Cleaning and manipulating data
library(dplyr)

# Statistic tests
library(multcompView)
library(rstatix)

# Plotting
library(ggplot2)
library(ggrepel)
library(ggprism)
library(scales)

# Loading and checking the data
data <- read.csv("2_ros_time_curve_self/ros_time_curve_self.csv")

# Independent variables as factors for anova
data <- data %>% 
  mutate(
    conc = factor(conc),
    time_factor = factor(time)
  )

# ANOVA
anova <- aov(ros ~ conc*time_factor, data = data)
summary(anova)
export::table2csv(anova, "2_ros_time_curve_self/anova_results.csv")


capture_anova <- summary(anova)
capture.output(capture_anova, file = "2_ros_time_curve_self/anova_results.doc")


# Tukey test
tukey <- TukeyHSD(anova)
tukey

tukey_capture <- as.data.frame(tukey[3])
write.csv(tukey_capture, "2_ros_time_curve_self/tukey.csv")

# Summary statistics
data_ss <- data %>% 
  group_by(conc, time) %>% 
  get_summary_stats(type = "mean_se") %>% 
  arrange(desc(mean))

# Compact letter display
tukey.cld <- multcompLetters4(anova, tukey)
tukey.cld

cld <- as.data.frame.list(tukey.cld$`conc:time_factor`)
data_ss$tukey <- cld$Letters

head(data_ss)

# Colors
colors <- c("#000000", "#D10101", "#b73779")

# Plot
ggplot(data_ss, aes(x = time, y = mean)) +
  # Geoms
  geom_point(
    data = data,
    aes(x = time, y = ros, fill = conc, shape = conc),
    alpha = 0.5,
    size = 2
  ) +
  # geom_errorbar(
  #   aes(ymin = mean - se, ymax = mean + se),
  #   width = 0.4
  #   ) +
  geom_line(
    aes(color = conc, linetype = conc)
    ) +
  geom_point(
    aes(shape = conc, fill = conc),
    size = 3
    ) +
  geom_text_repel(
    data = data_ss,
    aes(x = time, y = mean, label = tukey),
    #min.segment.length = Inf,
    max.overlaps = 5,
    size = 5
    ) + 
  # Scales
  scale_shape_manual(
    values = c(21, 22, 24)
    ) +
  scale_linetype_manual(
    values = c("dashed", "solid", "dotdash", "twodash")
    ) +
  scale_x_continuous(
    breaks = unique(data$time),
    guide = "prism_offset"
    ) +
  scale_y_continuous(
    limits = c(10, 50),
    breaks = seq(0, 50, 10),
    guide = "prism_offset_minor"
    ) +
  # scale_colour_viridis_d(option = "F") +
  # scale_fill_viridis_d(option = "F") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
#  Labs
  labs(
    x = "Minutes",
    y = bquote(H[2]*O[2]*" [nmol·g"^-1*" FW]"),
    #caption = "Tukey test, p < 0.05, n = 3",
    fill  = "Self-DNA [µg/ml]",
    color = "Self-DNA [µg/ml]",
    shape = "Self-DNA [µg/ml]",
    linetype = "Self-DNA [µg/ml]"
    ) +
  # Theme
  theme_prism(
    base_fontface = "plain",
    base_line_size = 0.7,
    #base_size = 16
    ) +
  theme(
    text = element_text(size = 15),
    legend.title = element_text(size = 13),
    legend.position = c(0.8, 0.8),
    #axis.title.y = element_blank()
    
    )

ggsave("2_ros_time_curve_self/ros_time_curve_self.pdf", width = 16, height = 9, units = "cm", scale = 1, dpi = 300, device = cairo_pdf)

save.image(file = "2_ros_time_curve_self/ros_time_curve_self.RData")
