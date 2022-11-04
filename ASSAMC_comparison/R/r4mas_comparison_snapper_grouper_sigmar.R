# Install and library packages --------------------------------------------

remotes::install_github("Bai-Li-NOAA/Age_Structured_Stock_Assessment_Model_Comparison", ref="a591ec6e1022893f75ab4bc4193f91fe49b51055")
remotes::install_github("nmfs-fish-tools/r4MAS", ref="a3fdf46f441d1058c776e20dba69936867d5b51f")

library(ASSAMC)
library(r4MAS)
library(jsonlite)

library(parallel)
library(doParallel)

project_dir <- "C:/Users/bai.li/Documents/bai/r4MAS-model-comparison"
# Set up OM ---------------------------------------------------------------

# Need to have the em_input folder in the working directory run other estimation models
maindir <- file.path(project_dir, "ASSAMC_comparison", "snapper_grouper_sigmar")
if(!dir.exists(maindir)) dir.create(maindir)


# om_sim_num <- 120 # total number of iterations per case
# keep_sim_num <- 100 # number of kept iterations per case
# figure_number <- 10 # number of individual iteration to plot

om_sim_num <- 60 # total number of iterations per case
keep_sim_num <- 50 # number of kept iterations per case
figure_number <- 10 # number of individual iteration to plot

seed_num <- 9924

## Life-history parameters
year <- 1:30
ages <- 1:12 # Age structure of the popn

initial_equilibrium_F <- TRUE
median_R0 <- 1000000 # Average annual unfished recruitment (scales the popn)
median_h <- 0.75 # Steepness of the Beverton-Holt spawner-recruit relationship.
mean_R0 <- NULL
mean_h <- NULL
SRmodel <- 1 # 1=Beverton-Holt; 2=Ricker
M <- 0.2 # Age-invariant natural mortality

Linf <- 800 # Asymptotic average length
K <- 0.18 # Growth coefficient
a0 <- -1.36 # Theoretical age at size 0
a.lw <- 0.000000025 # Length-weight coefficient
b.lw <- 3.0 # Length-weight exponent
A50.mat <- 2.25 # Age at 50% maturity
slope.mat <- 3 # Slope of maturity ogive
pattern.mat <- 1 # Simple logistic maturity
female.proportion <- 0.5 # Sex ratio

## Fleet settings
fleet_num <- 1

# CV of landings for OM
cv.L <- list()
cv.L$fleet1 <- 0.005

# Input CV of landings for EMs
input.cv.L <- list()
input.cv.L$fleet1 <- 0.01

# Annual sample size (nfish) of age comp samples
n.L <- list()
n.L$fleet1 <- 200

# Define fleet selectivity
sel_fleet <- list()

sel_fleet$fleet1$pattern <- 1
sel_fleet$fleet1$A50.sel1 <- 2
sel_fleet$fleet1$slope.sel1 <- 1

## Survey settings
survey_num <- 1

# CV of surveys for OM
cv.survey <- list()
cv.survey$survey1 <- 0.1

# Input CV of surveys for EMs
input.cv.survey <- list()
input.cv.survey$survey1 <- 0.2

# Annual sample size (nfish) of age comp samples
n.survey <- list()
n.survey$survey1 <- 200

# Define survey selectivity
sel_survey <- list()

sel_survey$survey1$pattern <- 1
sel_survey$survey1$A50.sel1 <- 1.5
sel_survey$survey1$slope.sel1 <- 2

## Other settings
logf_sd <- 0.2
f_dev_change <- FALSE
f_pattern <- 1
start_val <- 0.01
middle_val <- NULL
end_val <- 0.39
f_val <- NULL
start_year <- 1
middle_year <- NULL

logR_sd <- 0.2
r_dev_change <- TRUE

om_bias_cor <- FALSE
bias_cor_method <- "none" # Options: "none", "median_unbiased", and "mean_unbiased"
em_bias_cor <- FALSE


# Case 1 ------------------------------------------------------------------

null_case_input <- save_initial_input(base_case = TRUE, case_name = "C1")

## Run OM
run_om(input_list = null_case_input, show_iter_num = T)

## Run EMs
run_em(em_names = c("MAS"), input_list = null_case_input)
# run_em(em_names = c("ASAP"), input_list = null_case_input)

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = null_case_input
)

# Case 2 ------------------------------------------------------------------

updated_input <- save_initial_input(
  base_case = FALSE,
  input_list = null_case_input,
  case_name = "C2",
  logR_sd = 0.4
)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)

# Case 3 ------------------------------------------------------------------

updated_input <- save_initial_input(
  base_case = FALSE,
  input_list = null_case_input,
  case_name = "C3",
  logR_sd = 0.6
)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)

# Case 4 ------------------------------------------------------------------

updated_input <- save_initial_input(
  base_case = FALSE,
  input_list = null_case_input,
  case_name = "C4",
  logR_sd = 0.8
)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)

# Case 5 ------------------------------------------------------------------

updated_input <- save_initial_input(
  base_case = FALSE,
  input_list = null_case_input,
  case_name = "C5",
  logR_sd = 1.0
)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)

# Case 6 ------------------------------------------------------------------

updated_input <- save_initial_input(
  base_case = FALSE,
  input_list = null_case_input,
  case_name = "C6",
  logR_sd = 1.2
)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)

# Case 7 ------------------------------------------------------------------

updated_input <- save_initial_input(
  base_case = FALSE,
  input_list = null_case_input,
  case_name = "C7",
  logR_sd = 1.4
)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)

# Case 8 ------------------------------------------------------------------

updated_input <- save_initial_input(
  base_case = FALSE,
  input_list = null_case_input,
  case_name = "C8",
  logR_sd = 1.6
)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)

# Case 9 ------------------------------------------------------------------

updated_input <- save_initial_input(
  base_case = FALSE,
  input_list = null_case_input,
  case_name = "C9",
  logR_sd = 1.8
)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)


# Case 10 ------------------------------------------------------------------

updated_input <- save_initial_input(
  base_case = FALSE,
  input_list = null_case_input,
  case_name = "C10",
  logR_sd = 2.0
)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)

# Create summary table --------------------------------------------

project_dir <- "C:/Users/bai.li/Documents/bai/r4MAS-model-comparison/ASSAMC_comparison/snapper_grouper_sigmar/"
id <- 1:10
case_id <- paste("C", id, sep = "")
maindir_list <- paste(project_dir, case_id, sep = "")

em_num <- 1
sigmar <- seq(0.2, 2, by=0.2)
var <- c("ssb", "recruit", "Ftot", "ssbratio", "fratio")
mare <- matrix(NA, nrow = length(maindir_list), ncol = length(var))
row.names(mare) <- paste("Case", id)
colnames(mare) <- c("SSB", "R", "F", "Relative SSB", "Relative F")

nyear <- 30

for (var_id in 1:length(var)) {
  temp_matrix <- matrix(NA, nrow = nyear, ncol = length(maindir_list))
  for (j in 1:length(maindir_list)) {
    load(file.path(maindir_list[j], "output", "performance_measure.RData"))
    temp <- sapply(are_list, `[[`, x = var[var_id]) # ARE over years for a specific iteration
    for (i in 1:nyear) {
      temp_matrix[i, j] <- median(sapply(temp, `[[`, i))
    }
  }
  mare[, var_id] <- round(apply(temp_matrix, 2, mean) * 100, digits = 2)
}

write.csv(mare, file = file.path(project_dir, "mare.csv"))

jpeg(file=file.path(project_dir, "Mean MARE.jpg"), width=200, height=120, units="mm", res=800)
par(mfrow=c(2,3))
for (i in 1:ncol(mare)){
  plot(sigmar,
       mare[,i], 
       xlab = "Sigma R", 
       ylab = paste("Mean MARE for", colnames(mare)[i]),
       pch = 16,
       type="o")
}
dev.off()