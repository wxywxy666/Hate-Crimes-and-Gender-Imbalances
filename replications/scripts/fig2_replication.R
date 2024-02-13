#### Preamble ####
# Purpose: Figure 3 replication
# Author: Xiyou Wang
# Date: 12 February 2024
# Contact: xiyou.wang@mail.utoronto
# License: MIT

# Load data
points_list <- readRDS("inputs/data/points")
se_p_list <- readRDS("inputs/data/se")

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