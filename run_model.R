rm(list = ls())

library(tidyverse)
library(scales)
library(reshape)

# Source functions
source("R/HelperFun.R") # functions to help in simulations
source("R/decision_tree.R") # Decision Tree model & summary functions

# TASK 1
# choose a population and think about what the IBCM data indicates about bite patient incidence 
# Set up code to run the decision tree (examples of how to do this are commented in the source function)
# Run 1000 simulations parameterized for this population over a 5y period and save output
# Get a summary of the outputs for this model (i.e. median and 95% PIs)
test <- decision_tree_ndraw(ndraw = 1000, 
                       pop = 300000, HDR = 8, horizon = 5, discount = 0.03,
                       rabies_inc = c(0.0075, 0.0125), LR_range = c(6.6,12.8), mu = 0.38, k = 0.14,
                       pStart = 0.6666667, pComplete = 0.3968254, pDeath = 0.1660119, pPrevent = 0.986,
                       full_cost = 45, partial_cost = 25, campaign_cost = 0)
                   
                       
# TASK 2
# Compare average deaths & deaths averted over a ten year time horizon assuming changes in PEP policy
# i.e. assume that PEP is made free so bite victims are more likely to start and complete PEP
# Explore the probabilities to get an idea of how much these probabilities influence deaths and lives saved

# TASK 3
# Repeat task 2 for a largely muslim population where the human: dog ratio is high e.g. >30:1 (on Pemba it is ~100!)
# and compare to a christian population with a lot of livestock keeping (e.g. HDR in Mara region is closer to 7:1) 
# remember to change bite patient incidence (IBCM data with low risk bites can give you an idea of parameters)

# TASK 4
# Imagine mass dog vaccination halves the incidence of rabies in dogs
# Compare average deaths & deaths averted over a ten year time horizon assuming MDV campaigns 
# Remember to include some costs of organizing MDV!

# TASK 5
# See if you can adapt the decision tree function to examine other aspects of health seeking
# can you revise the function to have probabilities for:
# - patients seeking PEP & patients starting PEP if they reach a clinic

