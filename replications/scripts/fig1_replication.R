#### Preamble ####
# Purpose: Figure 2 replication
# Author: Xiyou Wang
# Date: 12 February 2024
# Contact: xiyou.wang@mail.utoronto
# License: MIT

library(ggplot2)

# Load and clean data for wave 4
d <- readRDS(here::here("inputs/data/fig1data"))
d <- lapply(d, function(x) round(x, 2))

# Classify all p-value
p_values_1st_2nd <- d[[3]][c(1, 4, 7)]
p_values_2nd_3rd <- d[[3]][c(2, 5,8 )]
p_values_total <- d[[3]][c(3, 6, 9)]

# Summary of basic data information
summary_data <- data.frame(
  group = rep(c("All", "Male (18-44)", "Male (30-40)"), each = 3),
  tercile = rep(c("1st\ntercile", "2nd\ntercile", "3rd\ntercile"), 3),
  mean = d[[1]],
  se = d[[2]]
)

# Function to add p-value annotations
add_pvalues <- function(plot, p_values_1st_2nd, p_values_2nd_3rd, p_values_total) {
  plot <- plot + geom_text(data = subset(summary_data, tercile == "2nd tercile" & group == unique(summary_data$group)[1]), 
                           aes(label = sprintf("pv = %.2f", p_values_1st_2nd[1]), y = 0.2),
                           size = 2.7)
  
  plot <- plot + geom_text(data = subset(summary_data, tercile == "3rd tercile" & group == unique(summary_data$group)[1]), 
                           aes(label = sprintf("pv = %.2f", p_values_2nd_3rd[1]), y = 0.4),
                           hjust = 1.15, vjust = 20, size = 2.7)
  
  plot <- plot + geom_text(data = subset(summary_data, tercile == "2nd tercile" & group == unique(summary_data$group)[1]), 
                           aes(label = sprintf("pv = %.2f", p_values_total[1]), y = 0.4), 
                           vjust = 0, fontface = "italic", size = 5)
  
  plot <- plot + geom_text(data = subset(summary_data, tercile == "2nd tercile" & group == unique(summary_data$group)[2]), 
                           aes(label = sprintf("pv = %.2f", p_values_1st_2nd[2]), y = 0.5),
                           hjust = 1.15, vjust = 25, size = 2.7)
  
  plot <- plot + geom_text(data = subset(summary_data, tercile == "3rd tercile" & group == unique(summary_data$group)[2]), 
                           aes(label = sprintf("pv = %.2f", p_values_2nd_3rd[2]), y = 0.5),
                           hjust = 1.15, vjust = 20, size = 2.7)
  
  plot <- plot + geom_text(data = subset(summary_data, tercile == "2nd tercile" & group == unique(summary_data$group)[2]), 
                           aes(label = sprintf("pv = %.2f", p_values_total[2]), y = 0.5), 
                           vjust = 0, fontface = "italic", size = 5)
  
  plot <- plot + geom_text(data = subset(summary_data, tercile == "2nd tercile" & group == unique(summary_data$group)[3]), 
                           aes(label = sprintf("pv = %.2f", p_values_1st_2nd[3]), y = 0.7),
                           hjust = 1.15, vjust = 25, size = 2.7)
  
  plot <- plot + geom_text(data = subset(summary_data, tercile == "3rd tercile" & group == unique(summary_data$group)[3]), 
                           aes(label = sprintf("pv = %.2f", p_values_2nd_3rd[3]), y = 0.7),
                           hjust = 1.15, vjust = 20, size = 2.7)
  
  plot <- plot + geom_text(data = subset(summary_data, tercile == "2nd tercile" & group == unique(summary_data$group)[3]), 
                           aes(label = sprintf("pv = %.2f", p_values_total[3]), y = 0.7), 
                           vjust = 0, fontface = "italic", size = 5)
  
  return(plot)
}

# Initial plot
p <- ggplot(summary_data, aes(x = tercile, y = mean, group = group)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_point() +
  facet_wrap(~group, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Excess Males",
    y = "Proportion Receiving Male Competition",
    group = "Group Name"
  ) +
  theme(
    strip.text = element_text(size = 20)
  )

# Use function to add p-value annotation
add_pvalues(p, p_values_1st_2nd, p_values_2nd_3rd, p_values_total)