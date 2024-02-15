#### Preamble ####
# Purpose: Figure 4 replication
# Author: Xiyou Wang
# Date: 12 February 2024
# Contact: xiyou.wang@mail.utoronto
# License: MIT

# Calculate points and standard errors again for clarity
point <- readRDS("inputs/data/points")[[5]]
se <- readRDS("inputs/data/se")[[5]]

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