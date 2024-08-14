### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date: 2024-07-31
# About: This is the main control file for this pipeline demonstrating the linkage 
# of synthetic data sets 
# Open Access Papers used for the Examples: 
# 1: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0271110#sec028
# 2: https://bmjopen.bmj.com/content/bmjopen/12/10/e063046.full.pdf

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Preparation ###

# clean workspace
rm(list = ls())  # remove all objects from work space 
gc(full = TRUE)  # deep clean garbage

# install & load libraries
# install.packages(c("data.table", "ggplot2", "MatchIt"))
library(data.table)
library(ggplot2)
library(MatchIt)


# key definitions for simulation
definitions <- list()
definitions$n_sim_runs  <- 1000
definitions$n_lowest_f  <- 10000

# key systems definition
definitions$seed     <- 20240801
definitions$cores    <- 100

# apply systems definition
data.table::setDTthreads(percent = definitions$cores)
set.seed(definitions$seed ) 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Source Actual Files ###
source("RCode/02_example_lowest_fidelity.R")     
source("RCode/03_example_low_fidelity.R")     
source("RCode/04_example_medium_fidelity_deterministic.R")  
source("RCode/05_example_medium_fidelity_matching.R")  

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
