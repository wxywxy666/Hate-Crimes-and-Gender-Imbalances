# Load se_p_list and points_list
se_p_list <- readRDS("inputs/data/se")
se_p_list <- lapply(se_p_list, function(x) ifelse(is.na(x), 0, x))

points_list <- readRDS("inputs/data/points")
points_list <- lapply(points_list, function(x) ifelse(is.na(x), 0, x))
# Load fig1 data
pv <- readRDS("inputs/data/fig1data")

# Test length
if(length(se_p_list) == 6 && length(points_list) == 6 && length(pv) == 3) {
  cat("List lengths are correct.\n")
} else {
  cat("Error in list lengths.\n")
}

# Test range
if(check_data_range(se_p_list, 0, 0.05) && check_data_range(points_list, 0, 0.3) && check_data_range(pv[[3]], 0, 0.4)) {
  cat("Data ranges in se_p_list, points_list and pv are correct.\n")
} else {
  cat("Error in data ranges of se_p_list or points_list or pv.\n")
}

