# Install and library packages --------------------------------------------

remotes::install_github("Bai-Li-NOAA/Age_Structured_Stock_Assessment_Model_Comparison")
# remotes::install_github("nmfs-fish-tools/r4MAS", ref="a3fdf46f441d1058c776e20dba69936867d5b51f")

## Check that you have the required library packages installed
## Load required packages with library()
library(ASSAMC)
library(r4MAS)
library(jsonlite)
library(parallel)
library(doParallel)

## Change project_dir to folder on your computer
# project_dir <- "C:/Users/squid/OneDrive/Desktop/Documents/github/r4MAS-model-comparison"
project_dir <- "C:/Users/bai.li/Desktop/r4MAS-model-comparison"

## SET UP OPERATING MODEL FOR ENSEMBLE MODEL (SET OF N=3 EM MODELS) FOR CASE STUDY 1
## MODELS M111-M131 WITH LOGISTIC SELECTIVITY

## Change "file.path() arguments to match subfolders on your computer
maindir <- file.path(project_dir, "ensemble")
# source(file.path("C:/Users/squid/OneDrive/Desktop/Documents/github/r4MAS-model-comparison/ensemble/R/run_mas_ensemble.R"))
source(file.path("C:/Users/bai.li/Desktop/r4MAS-model-comparison/ensemble/R/run_mas_ensemble.R"))

## Set up single OM simulation to start (may want to do ensemble run with multiple sims per model later)
## Apply each EM to the single OM simulation (to start, may want to do multiple OM sims later)

model_input <- save_initial_input()

model_input$maindir <- maindir
model_input$om_sim_num <- 2 # total number of iterations per case
model_input$keep_sim_num <- 2 # number of kept iterations per case
keep_sim_num <- model_input$keep_sim_num # global variable: number of kept iterations per case
model_input$figure_number <- 2 # number of individual iteration to plot

## Use a prime number for good luck
model_input$seed_num <- 39667

## Use life history and fishery and survey set up from case study 1
## Life-history parameters
model_input$year <- 1:30
year <- model_input$year
model_input$ages <- 1:12 # Age structure of the popn
ages <- model_input$ages

model_input$initial_equilibrium_F <- TRUE
model_input$median_R0 <- 1000000 # Average annual unfished recruitment (scales the popn)
model_input$median_h <- 0.75 # Steepness of the Beverton-Holt spawner-recruit relationship.
model_input$mean_R0 <- NULL
model_input$mean_h <- NULL
model_input$SRmodel <- 1 # 1=Beverton-Holt; 2=Ricker
model_input$M <- 0.2 # Age-invariant natural mortality

model_input$Linf <- 800 # Asymptotic average length
model_input$K <- 0.18 # Growth coefficient
model_input$a0 <- -1.36 # Theoretical age at size 0
model_input$a.lw <- 0.000000025 # Length-weight coefficient
model_input$b.lw <- 3.0 # Length-weight exponent
model_input$A50.mat <- 2.25 # Age at 50% maturity
model_input$slope.mat <- 3 # Slope of maturity ogive
model_input$pattern.mat <- 1 # Simple logistic maturity
model_input$female.proportion <- 0.5 # Sex ratio

## Fleet settings
model_input$fleet_num <- 1

# CV of landings for OM
model_input$cv.L <- list()
model_input$cv.L$fleet1 <- 0.005

# Input CV of landings for EMs
model_input$input.cv.L <- list()
model_input$input.cv.L$fleet1 <- 0.01

# Annual sample size (nfish) of age comp samples
model_input$n.L <- list()
model_input$n.L$fleet1 <- 200

# Define fleet selectivity
model_input$sel_fleet <- list()

model_input$sel_fleet$fleet1$pattern <- 1
model_input$sel_fleet$fleet1$A50.sel1 <- 2
model_input$sel_fleet$fleet1$slope.sel1 <- 1

## Survey settings
model_input$survey_num <- 1

# CV of surveys for OM
model_input$cv.survey <- list()
model_input$cv.survey$survey1 <- 0.1

# Input CV of surveys for EMs
model_input$input.cv.survey <- list()
model_input$input.cv.survey$survey1 <- 0.2

# Annual sample size (nfish) of age comp samples
model_input$n.survey <- list()
model_input$n.survey$survey1 <- 200

# Define survey selectivity
model_input$sel_survey <- list()

model_input$sel_survey$survey1$pattern <- 1
model_input$sel_survey$survey1$A50.sel1 <- 1.5
model_input$sel_survey$survey1$slope.sel1 <- 2

## Other settings
model_input$logf_sd <- 0.2
model_input$f_dev_change <- FALSE
model_input$f_pattern <- 1
model_input$start_val <- 0.01
model_input$middle_val <- NULL
model_input$end_val <- 0.39
model_input$f_val <- NULL
model_input$start_year <- 1
model_input$middle_year <- NULL

model_input$logR_sd <- 0.4
model_input$r_dev_change <- TRUE

model_input$om_bias_cor <- FALSE
model_input$bias_cor_method <- "none" # Options: "none", "median_unbiased", and "mean_unbiased"
model_input$em_bias_cor <- FALSE

###############################################################################

# M111 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.75, 0.20)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M111")


## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.75,
  natural_mortality_input = rep(0.20, length(case_input$ages))
  )

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M121 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.65, 0.20)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M121")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.65,
  natural_mortality_input = rep(0.20, length(case_input$ages))
)

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M131 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.85, 0.20)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M131")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.85,
  natural_mortality_input = rep(0.20, length(case_input$ages))
)

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M112 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.75, 0.16)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M112")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.75,
  natural_mortality_input = rep(0.16, length(case_input$ages))
  )

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M122 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.65, 0.16)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M122")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.65,
  natural_mortality_input = rep(0.16, length(case_input$ages))
)

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M132 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.85, 0.16)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M132")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.85,
  natural_mortality_input = rep(0.16, length(case_input$ages))
)

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M113 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.75, 0.18)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M113")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.75,
  natural_mortality_input = rep(0.18, length(case_input$ages))
  )

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M123 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.65, 0.18)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M123")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.65,
  natural_mortality_input = rep(0.18, length(case_input$ages))
)

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M133 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.85, 0.18)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M133")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.85,
  natural_mortality_input = rep(0.18, length(case_input$ages))
)

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M114 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.75, 0.22)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M114")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.75,
  natural_mortality_input = rep(0.22, length(case_input$ages))
  )

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M124 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.65, 0.22)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M124")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.65,
  natural_mortality_input = rep(0.22, length(case_input$ages))
)

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M134 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.85, 0.22)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M134")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.85,
  natural_mortality_input = rep(0.22, length(case_input$ages))
)

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M115 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.75, 0.24)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M115")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.75,
  natural_mortality_input = rep(0.24, length(case_input$ages))
  )

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M125 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.65, 0.24)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M125")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.65,
  natural_mortality_input = rep(0.24, length(case_input$ages))
)

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

# M135 ------------------------------------------------------------------
# {Selectivity, Recruitment, Natural Mortality} = {L, 0.85, 0.24)

case_input <- save_initial_input(base_case = FALSE, input_list = model_input, case_name = "C1_M135")

## Run OM
run_om(input_list = case_input, show_iter_num = T)

## Run EMs
fishery_selectivity_input <- list()
fishery_selectivity_input$fleet1$pattern <- 1
fishery_selectivity_input$fleet1$A50.sel1 <- 2
fishery_selectivity_input$fleet1$slope.sel1 <- 1

run_mas_ensemble(
  input_list = case_input,
  fishery_selectivity_input = fishery_selectivity_input, 
  recruitment_steepness_input = 0.85,
  natural_mortality_input = rep(0.24, length(case_input$ages))
)

## Plot comparison outputs
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = case_input
)

###############################################################################
## Output processing for ensemble model runs for case study 1
## Will code this section after we get two example EMs running correctly
###############################################################################
#
#
# Create summary table
# project_dir <- "C:/Users/bai.li/Documents/Github/r4MAS-model-comparison/ASSAMC_comparison/snapper_grouper/"
# case_id <- paste("C", 1:12, sep="")
# maindir_list <- paste(project_dir, case_id, sep="")

# em_num <- 1

# var <- c("ssb", "recruit", "Ftot", "ssbratio", "fratio")
# mare <- matrix(NA, nrow = length(maindir_list), ncol=length(var))
# row.names(mare) <- paste("Case", 1:12)
# colnames(mare) <- c("SSB", "R", "F", "Relative SSB", "Relative F")

# nyear <- 30

# for (var_id in 1:length(var)){
#  temp_matrix <- matrix(NA, nrow = nyear, ncol = length(maindir_list))
#  for (j in 1:length(maindir_list)) {
#    load(file.path(maindir_list[j], "output", "performance_measure.RData"))
#    temp <- sapply(are_list, `[[`, x = var[var_id]) # ARE over years for a specific iteration
#    for (i in 1:nyear){
#      temp_matrix[i, j] <- median(sapply(temp, `[[`, i))
#    }
#  }
#  mare[, var_id] <- round(apply(temp_matrix, 2, mean)*100, digits = 2)
#  
# }

# write.csv(mare, file=file.path(project_dir, "mare.csv"))