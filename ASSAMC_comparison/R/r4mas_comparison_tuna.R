# Install and library packages --------------------------------------------

# remotes::install_github("Bai-Li-NOAA/Age_Structured_Stock_Assessment_Model_Comparison")
# remotes::install_github("cmlegault/ASAPplots")
# remotes::install_github("r4ss/r4ss")
# remotes::install_github(repo = "nmfs-fish-tools/r4MAS",
#                         ref = "719f99f0cd36784eb4709af80b1991e60acdd17f")
# install.packages("PBSadmb")

library(ASSAMC)
library(r4ss)
library(r4MAS)
library(readxl)
library(PBSadmb)
library(jsonlite)

library(parallel)
library(doParallel)


# Set up OM ---------------------------------------------------------------

# Need to have the em_input folder in the working directory run other estimation models
maindir <- "#######"

om_sim_num <- 160 # total number of iterations per case
keep_sim_num <- 100 # number of kept iterations per case
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


# TUNA life history parameters - medians from scombrid database 

Linf <- 85.6 # Asymptotic average length
K <- 0.31 # Growth coefficient
a0 <- -0.86 # Theoretical age at size 0
a.lw <- 0.0000423 # Length-weight coefficient
b.lw <- 3.02 # Length-weight exponent
A50.mat <- 2 # Age at 50% maturity
slope.mat <- 4.08 # Slope of maturity 

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

logR_sd <- 0.4
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

# generate_plot(
#   em_names = c("MAS", "ASAP"),
#   plot_ncol = 2, plot_nrow = 1,
#   plot_color = c("deepskyblue3", "coral3"),
#   input_list = null_case_input
# )

# Case 2 ------------------------------------------------------------------

updated_input <- save_initial_input(
  base_case = FALSE,
  input_list = null_case_input,
  case_name = "C2",
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


# Case 3 ------------------------------------------------------------------

updated_input <- save_initial_input(
  base_case = FALSE,
  input_list = null_case_input,
  case_name = "C3",
  f_dev_change = TRUE
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
updated_input <- save_initial_input(base_case=FALSE,
                                    input_list=null_case_input,
                                    case_name="C4",
                                    f_pattern=3,
                                    start_val=0.01,
                                    middle_val=0.39,
                                    end_val=0.08,
                                    f_val=NULL,
                                    start_year=1,
                                    middle_year=25)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)


# Case 5 ------------------------------------------------------------------

updated_input <- save_initial_input(base_case=FALSE,
                                    input_list=null_case_input,
                                    case_name="C5",
                                    f_pattern=5,
                                    start_val=0.01,
                                    middle_val=0.08,
                                    end_val=NULL,
                                    f_val=NULL,
                                    start_year=1,
                                    middle_year=6)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)


# Case 6 ------------------------------------------------------------------

updated_input <- save_initial_input(base_case=FALSE,
                                    input_list=null_case_input,
                                    case_name="C6",
                                    f_pattern=5,
                                    start_val=0.01,
                                    middle_val=0.19,
                                    end_val=NULL,
                                    f_val=NULL,
                                    start_year=1,
                                    middle_year=6)

run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)


# Case 7 ------------------------------------------------------------------

updated_input <- save_initial_input(base_case=FALSE,
                                    input_list=null_case_input,
                                    case_name="C7",
                                    f_pattern=5,
                                    start_val=0.01,
                                    middle_val=0.39,
                                    end_val=NULL,
                                    f_val=NULL,
                                    start_year=1,
                                    middle_year=6)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)


# Case 8 ------------------------------------------------------------------

sel_fleet <- list()

sel_fleet$fleet1$pattern <- 2
sel_fleet$fleet1$A50.sel1 <- 4
sel_fleet$fleet1$slope.sel1 <- 1
sel_fleet$fleet1$slope.sel2 <- 0.7
sel_fleet$fleet1$A50.sel2 <- 11

#Define survey selectivity
sel_survey <- list()

sel_survey$survey1$pattern <- 2
sel_survey$survey1$A50.sel1 <- 2
sel_survey$survey1$slope.sel1 <- 1.5
sel_survey$survey1$A50.sel2 <- 12
sel_survey$survey1$slope.sel2 <- 0.37
updated_input <- save_initial_input(base_case=FALSE,
                                    input_list=null_case_input,
                                    case_name="C8",
                                    sel_fleet=sel_fleet,
                                    sel_survey=sel_survey)

run_om(input_list = updated_input, show_iter_num = F)
# run_em(em_names = c("MAS"), input_list = updated_input)
# generate_plot(
#   em_names = c("MAS"),
#   plot_ncol = 1, plot_nrow = 1,
#   plot_color = c("deepskyblue3"),
#   input_list = updated_input
# )


# Case 9 ------------------------------------------------------------------

survey_num <- 2
#CV of surveys
cv.survey <- list()
cv.survey$survey1 <- 0.1
cv.survey$survey2 <- 0.1

input.cv.survey <- list()
input.cv.survey$survey1 <- 0.2
input.cv.survey$survey2 <- 0.2
#annual sample size (nfish) of age comp samples
n.survey <- list()
n.survey$survey1 <- 200
n.survey$survey2 <- 200
#define survey selectivity
sel_survey <- list()
sel_survey$survey1$pattern <- 1
sel_survey$survey1$A50.sel1 <- 1.5
sel_survey$survey1$slope.sel1 <- 2

sel_survey$survey2$pattern <- 1
sel_survey$survey2$A50.sel1 <- 1.5
sel_survey$survey2$slope.sel1 <- 2

updated_input <- save_initial_input(base_case=FALSE,
                                    input_list=null_case_input,
                                    case_name="C9",
                                    survey_num=survey_num,
                                    cv.survey=cv.survey,
                                    input.cv.survey=input.cv.survey,
                                    n.survey=n.survey,
                                    sel_survey=sel_survey)
run_om(input_list = updated_input, show_iter_num = F)
# run_em(em_names = c("MAS"), input_list = updated_input)
# generate_plot(
#   em_names = c("MAS"),
#   plot_ncol = 1, plot_nrow = 1,
#   plot_color = c("deepskyblue3"),
#   input_list = updated_input
# )


# Case 10 -----------------------------------------------------------------

updated_input <- save_initial_input(base_case=FALSE,
                                    input_list=null_case_input,
                                    case_name="C10",
                                    initial_equilibrium_F=FALSE)
run_om(input_list = updated_input, show_iter_num = F)
run_em(em_names = c("MAS"), input_list = updated_input)
generate_plot(
  em_names = c("MAS"),
  plot_ncol = 1, plot_nrow = 1,
  plot_color = c("deepskyblue3"),
  input_list = updated_input
)