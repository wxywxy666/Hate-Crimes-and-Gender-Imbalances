#### Preamble ####
# Purpose: simulate data
# Author: Xiyou Wang
# Date: 12 February 2024
# Contact: xiyou.wang@mail.utoronto
# License: MIT

# Set seed
set.seed(888)

# Genertate se_p_list
se_p_list <- lapply(1:4, function(x) runif(5, 0.006, 0.009))
se_p_list[[5]] <- runif(5, 0.018, 0.021)
se_p_list[[6]] <- se_p_list[[5]]

# Genertate points_list
points_list <- lapply(1:4, function(x) runif(5, 0.15, 0.25))
points_list[[5]] <- runif(5, 0.02, 0.20)
points_list[[6]] <- points_list[[5]]

# Genertate p-values
pv <- list(
  runif(9, min = 0.17, max = 0.47), # 第一组数据，范围从0.17到0.47，共9个值
  runif(9, min = 0.01, max = 0.07), # 第二组数据，范围从0.01到0.07，共9个值
  runif(9, min = 0.00, max = 0.40)  # 第三组数据，范围从0.00到0.40，共9个值
)

## #################
## Figure 1
## #################
p_values_1st_2nd <- pv[[3]][c(1, 4, 7)]
p_values_2nd_3rd <- pv[[3]][c(2, 5,8 )]
p_values_total <- pv[[3]][c(3, 6, 9)]

# Summary of basic data information
summary_data <- data.frame(
  group = rep(c("All", "Male (18-44)", "Male (30-40)"), each = 3),
  tercile = rep(c("1st\ntercile", "2nd\ntercile", "3rd\ntercile"), 3),
  mean = pv[[1]],
  se = pv[[2]]
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

## #################
## Figure 2
## #################
# Bar names
bar_name <- rep("", 5)
bar_name_u <- c("Only Means\n(List)", "Only Means\n(Direct)", "Justified", "Message", "Condemn")

# Set plot format
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

# Loop to make 4 panels
for (i in 1:4) {
  point <- points_list[[i]]
  se_p <- se_p_list[[i]]
  
  base <- barplot(point, ylim = c(0, 0.3), names.arg = bar_name, 
                  col = c("cornflowerblue", rep("lightblue1", 4)),
                  ylab = "Proportion of respondents",
                  las = 2,
                  main = paste("Wave", i))
  
  arrows(base, point - 1.96*se_p, base, point + 1.96*se_p, 
         lwd = 2, angle = 90, length = 0.05, code = 3,
         col = adjustcolor("black", alpha.f = 0.7))
  
  mtext(bar_name_u, at = base, side = 1, line = 2.5, cex = 1)
}

## #################
## Figure 3
## #################

# Calculate points and standard errors again for clarity
point <- points_list[[5]]
se <- se_p_list("inputs/data/se")[[5]]

# Preparation of figure settings
colors <- c("skyblue", "salmon", "lightgreen", "gold", "violet")
pch_values <- c(15, 16, 17, 18, 19)
names_point <- c("Only Means", "Justified", "Message", "Prevent", "Condemn")

par(mar = c(5, 5, 4, 2))

# Initial plot
plot(seq(1:5), point, pch = pch_values, col = colors, ylim = c(-0.05, 0.35), xlim = c(0.5, 5.5), 
     xlab = "", xaxt = "n", ylab = "Estimated Effects", 
     cex.lab = 1.25, cex.axis = 1.25, cex.main = 1.5, cex = 1.5)

segments(seq(1:5), point - 1.96*se, 
         seq(1:5), point + 1.96*se, col = colors, lwd = 2)

Axis(side = 1, at = seq(1:5), labels = names_point, cex.axis = 1)

abline(h = 0, lty = 2)

text(2.5, 0.30, "Significant Effects", cex = 0.8)

# Add four arrows
for (i in 1:4) {
  arrows(2.5, 0.29, i, point[i] + 0.02, length = 0.05)
}