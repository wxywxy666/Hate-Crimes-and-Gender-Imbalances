# Load libraries
require(readstata13)
require(list)

# Load data and create empty list for loop
dat <- read.dta13(file =  "inputs/data/survey.dta")

## #################
## Figure 1
## #################
# Only discuss wave 4
dat_use <- dat[dat$wave == 4, ]
# Prepare Two data sets
dat_male <- dat_use[dat_use$gender == "Male" & dat_use$age <= 44 & dat_use$age >= 18, ]
dat_male_y <- dat_use[dat_use$gender == "Male" & dat_use$age <= 40 & dat_use$age >= 30, ]

# Overall Samples 
dat_use$MateComp.cont_bin <- ifelse(dat_use$MateComp.cont >= 3, 1, 0)
dat_use$excess_c <- ifelse(dat_use$pop_15_44_muni_gendergap_2015 < 1.04, "1", 
                           ifelse(dat_use$pop_15_44_muni_gendergap_2015 < 1.12, "2", "3"))
mean_all <- tapply(dat_use$MateComp.cont_bin, dat_use$excess_c, mean)
se_all <- tapply(dat_use$MateComp.cont_bin, dat_use$excess_c, sd)/sqrt(table(dat_use$excess_c))

# Male (18 - 44)
dat_male$MateComp.cont_bin <- ifelse(dat_male$MateComp.cont >= 3, 1, 0)
dat_male$excess_c <- ifelse(dat_male$pop_15_44_muni_gendergap_2015 < 1.04, "1",
                            ifelse(dat_male$pop_15_44_muni_gendergap_2015 < 1.12, "2", "3"))
mean_all_m <- tapply(dat_male$MateComp.cont_bin, dat_male$excess_c, mean)
se_all_m <- tapply(dat_male$MateComp.cont_bin, dat_male$excess_c, sd)/sqrt(table(dat_male$excess_c))

# Male (30 - 40)
dat_male_y$MateComp.cont_bin <- ifelse(dat_male_y$MateComp.cont >= 3, 1, 0)
dat_male_y$excess_c <- ifelse(dat_male_y$pop_15_44_muni_gendergap_2015 < 1.04, "1", 
                              ifelse(dat_male_y$pop_15_44_muni_gendergap_2015 < 1.12, "2", "3"))
mean_all_y <- tapply(dat_male_y$MateComp.cont_bin, dat_male_y$excess_c, mean)
se_all_y <- tapply(dat_male_y$MateComp.cont_bin, dat_male_y$excess_c, sd)/sqrt(table(dat_male_y$excess_c))

# Calculate p_value
diff <- c(mean_all[2] - mean_all[1], 
          mean_all[3] - mean_all[2],  
          mean_all[3] - mean_all[1])
sd_d <- c(sqrt(se_all[2]^2 + se_all[1]^2), 
          sqrt(se_all[3]^2 + se_all[2]^2),  
          sqrt(se_all[3]^2 + se_all[1]^2))
diff_m <- c(mean_all_m[2] - mean_all_m[1], 
            mean_all_m[3] - mean_all_m[2],  
            mean_all_m[3] - mean_all_m[1])
sd_d_m <- c(sqrt(se_all_m[2]^2 + se_all_m[1]^2), 
            sqrt(se_all_m[3]^2 + se_all_m[2]^2),  
            sqrt(se_all_m[3]^2 + se_all_m[1]^2))
diff_y <- c(mean_all_y[2] - mean_all_y[1], 
            mean_all_y[3] - mean_all_y[2],  
            mean_all_y[3] - mean_all_y[1])
sd_d_y <- c(sqrt(se_all_y[2]^2 + se_all_y[1]^2), 
            sqrt(se_all_y[3]^2 + se_all_y[2]^2),  
            sqrt(se_all_y[3]^2 + se_all_y[1]^2))

diff_l <- c(diff, diff_m, diff_y)
se_l <- c(sd_d, sd_d_m, sd_d_y)
p_value <- 2*(1 - pnorm(abs(diff_l/se_l)))
diff_table <- cbind(diff_l, se_l, p_value)

# Collect data and save
mean <- unname(c(mean_all, mean_all_m, mean_all_y))
se <- unname(c(se_all, se_all_m, se_all_y))
p_value <- unname(p_value)

fig1 <- list(mean, se, p_value)

# Check if file already exist to prevent same data being saved
if (file.exists("inputs/data/fig1")) {
  message("File already exists.")
} else {
  saveRDS(fig1, file = "inputs/data/fig1data")
}

## #################
## Figure 2
## #################
# creat empty list for loop
point <- list()
se_p <- list()

for (i in 1:4) {
  # Open dataset and divided by wave
  data.u2 <- dat[dat$wave == i, ]
  
  # Means: When it comes to the refugee problem, violence is sometimes the only means that citizens have to get the attention of German politicians
  data.list.u2   <- data.u2[data.u2$list == "1",]
  data.direct.u2 <- data.u2[data.u2$list == "2",]
  data.list.u2 <- data.list.u2[is.na(data.list.u2$treatment_list)==FALSE,]
  data.list.u2$List.treat <- ifelse(data.list.u2$treatment_list == "Scenario 2", 1, 0)
  
  ## with Mean = 0.15401 sd = 0.03358
  if (dat$wave[1] != 1) {  
    diff.in.means.results2 <- ictreg(outcome_list ~ 1, data = data.list.u2,
                                     treat = "List.treat", J=3, method = "lm")
  }
  
  ## Compare to All People who answered Direct Question (n = 2170)
  data.u2.all.direct <- data.u2[is.na(data.u2$hate_violence_means) == FALSE, ]
  data.u2.all.direct$hate.direct.bin <- ifelse(data.u2.all.direct$hate_violence_means >=3, 1, 0)
  point_dir2 <- mean(data.u2.all.direct$hate.direct.bin) ## 0.181 
  se_dir2 <- sd(data.u2.all.direct$hate.direct.bin)/sqrt(length(data.u2.all.direct$hate.direct.bin)) # 0.0083
  
  # Direct Questions
  data.u2$message.bin <- ifelse(data.u2$hate_pol_message >= 3, 1, 0)
  data.u2$condemn.bin <- ifelse(data.u2$hate_polcondemn >= 3, 1, 0)
  data.u2$justified.bin <- ifelse(data.u2$hate_justified >= 3, 1, 0)
  
  message.mean2 <- mean(data.u2$message.bin)
  condemn.mean2 <- mean(data.u2$condemn.bin)
  justified.mean2 <- mean(data.u2$justified.bin)
  message.se2 <- sd(data.u2$message.bin)/sqrt(length(data.u2$message.bin)) # 0.0070
  condemn.se2 <- sd(data.u2$condemn.bin)/sqrt(length(data.u2$condemn.bin)) # 0.0079
  justified.se2 <- sd(data.u2$justified.bin)/sqrt(length(data.u2$justified.bin)) # 0.0074
  
  # Save each waves' data
  point[[i]] <- c(summary(diff.in.means.results2)$par.treat, point_dir2, justified.mean2, message.mean2, condemn.mean2)
  se_p[[i]] <- c(summary(diff.in.means.results2)$se.treat,  se_dir2, justified.se2, message.se2, condemn.se2)
}

## #################
## Figure 3
## #################
# Set formula context
formula <- 
  as.character("hate_violence_means ~ MateComp.cont + JobComp.cont + 
               LifeSatis.cont +  factor(age_group) + factor(gender) + 
               factor(state) + factor(citizenship) + factor(marital) + 
               factor(religion) + eduyrs + factor(occupation) + 
               factor(income) + factor(household_size) + factor(self_econ) + 
               factor(ref_integrating) + factor(ref_citizenship) + factor(ref_reduce) + 
               factor(ref_moredone) + factor(ref_cultgiveup) + 
               factor(ref_economy) + factor(ref_crime) + factor(ref_terror)  + 
               factor(ref_loc_services) +  factor(ref_loc_economy) + factor(ref_loc_crime) + 
               factor(ref_loc_culture) + factor(ref_loc_islam) + 
               factor(ref_loc_schools) + factor(ref_loc_housing) + factor(ref_loc_wayoflife) + 
               factor(distance_ref) + factor(settle_ref) + 
               lrscale + afd + muslim_ind + afd_ind + contact_ind")

# Set up different linear model in order to get the co-efficient
lm <- lm(as.formula(formula), data=dat_use)
f <- as.character(as.formula(formula))[3]

formula.means   <- paste("hate_violence_means ~ ", f, sep = "")
formula.message <- paste("hate_pol_message ~", f, sep = "")
formula.prevent <- paste("hate_prevent_settlement ~", f, sep = "")
formula.condemn <- paste("hate_polcondemn ~ ", f, sep = "")
formula.justified <- paste("hate_justified ~ ", f, sep = "")

lm.means <- lm(as.formula(formula.means), data=dat_use)
lm.justified <- lm(as.formula(formula.justified), data=dat_use)
lm.message <- lm(as.formula(formula.message), data=dat_use)
lm.prevent <- lm(as.formula(formula.prevent), data=dat_use)
lm.condemn <- lm(as.formula(formula.condemn), data=dat_use)

# Get all the point estimators and standard errors
p <- c(coef(lm.means)["MateComp.cont"],
       coef(lm.justified)["MateComp.cont"], coef(lm.message)["MateComp.cont"], 
       coef(lm.prevent)["MateComp.cont"], coef(lm.condemn)["MateComp.cont"])

se <- c(summary(lm.means)$coef["MateComp.cont", 2],
        summary(lm.justified)$coef["MateComp.cont", 2], summary(lm.message)$coef["MateComp.cont", 2],
        summary(lm.prevent)$coef["MateComp.cont", 2], summary(lm.condemn)$coef["MateComp.cont", 2])

# Combine all the points and standard errors
point <- c(point, list(unname(p)))
se_p <- c(se_p, list(unname(se)))

# Check if file already exist to prevent same data being saved
if (file.exists("inputs/data/points")) {
  message("File already exists.")
} else {
  saveRDS(point, file = "inputs/data/points")
}

if (file.exists("inputs/data/se")) {
  message("File already exists.")
} else {
  saveRDS(se_p, file = "inputs/data/se")
}