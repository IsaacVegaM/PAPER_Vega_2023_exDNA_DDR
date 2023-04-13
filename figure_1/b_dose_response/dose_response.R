# Dose response curve to self-dna

# Load required libraries

# Cleaning and manipulating data
library(dplyr)

# Statistic tests
library(drc)

# Plotting
library(ggplot2)
library(ggprism)
library(scales)
library(ggbeeswarm)

# Load data
data <- read.csv("1_dose_response/dose_response.csv")

# Creating Dose-response curve model
model <- drm(ros ~ conc,
             data = data,
             fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))

summary(model)

ed <- ED(model, c(5, 10, 50, 99), interval = "delta")

p_value <- broom::tidy(model) %>% 
  filter(term == "ED50") %>% 
  pull(p.value) %>% 
  signif(digits = 3)


capture_model <- summary(model)
capture.output(capture_model, file = "1_dose_response/dose_response_model.doc")
capture.output(ed, file = "1_dose_response/effective_doses.doc")

# Plot
data$conc0 <- data$conc
data$conc0[data$conc0 == 0] <- 0.0005 # Just to plot log 0 values

# Plot
ggplot(data, aes(x = conc0, y = ros)) +
  # Geom
  geom_smooth(
    method = drm,
    method.args = list(fct = L.4()),
    se = F,
    color = "black"
    ) +
  geom_beeswarm(
    aes(alpha = alpha),
    fill = "#D10101",
    shape = 21,
    cex = 2,
    size = 4,
    show.legend = F
  ) +
  geom_beeswarm(
    color = "black",
    shape = 21,
    cex = 2,
    size = 4
  ) +
  geom_beeswarm(
    data = filter(data, conc == 0),
    fill = "black",
    shape = 21,
    cex = 2,
    size = 4
  ) +  
  geom_beeswarm(
    data = filter(data, conc == 50),
    fill = "#b73779",
    shape = 21,
    cex = 2,
    size = 4
  ) +
  annotate(
    geom = "text",
    x = 15, y = 15,
    label = paste("p value = ", p_value)
    ) +
  # Scale
  scale_x_log10(
    breaks = c(0.0005, 0.005, 0.05, 0.5, 5, 50),
    labels = c("0", "0.005", "0.05", "0.5", "5", "50"),
    guide = "prism_offset"
    ) +
  scale_y_continuous(
    limits = c(10, 100),
    guide = "prism_offset_minor"
    ) +
   scale_alpha_continuous(
     breaks = seq(0, 1, 0.2),
     labels = c("0", "0.005", "0.05", "0.5", "5", "50")
     ) +
  # Labels
  labs(
    x = "Self-DNA [µg/ml]",
    y = bquote(H[2]*O[2]*" [nmol·g"^-1*" FW]"),
    alpha = "Self-DNA [µg/ml]",
    caption = "Log-logistic ED50 model fitted, n = 5"
  ) +
  # Theme
  theme_prism(
    base_fontface = "plain",
    base_line_size = 0.7
    ) +
  theme(
    legend.title = element_text(),
    legend.position = c(0.2, 0.75),
    #axis.title.y = element_blank()
    )

ggsave("1_dose_response/dose_response_self.pdf", width = 16, height = 9, units = "cm", scale = 1, dpi = 300, device = cairo_pdf)

save.image(file = "1_dose_response/dose_response.RData")
