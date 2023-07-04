
library(tidyverse)

source("Code/HelperFun.R") # functions to help in simulations
source("Code/decision_tree.R") # Decision Tree model & summary functions

# Decision tree model that can be applied to create different scenarios 

decision_tree <- function(N, pop, HDR, horizon, discount,
                          rabies_inc, LR_range, mu, k, pSeek_healthy,
                          pStart_healthy, pComplete_healthy, pSeek_exposure,
                          pStart_exposure, pComplete_exposure, pDeath, pPrevent, 
                          full_cost, partial_cost, #campaign_cost,
                          vaccinate_dog_cost, target_vax_cov,
                          pInvestigate, pFound,  pTestable, pFN
                          ) {
  

# SIMULATE TIMESERIES

# Estimate dog population
HDR <- runif(n=N, min = HDR[1], max = HDR[2])  # Explore uncertainty in HDR - uniform distribution w/ upper & lower limits 
dog_pop <- pop/HDR 

# effect of dog vaccination on no. of rabid dogs

# # Approach 1 - assume vaccination coverage determined by campaign costs
# 
# dogs_vaccinated_from_campaign_cost <- function(campaign_cost, vaccinate_dog_cost, dog_pop){
#   # uncertainty in dog vaccination costs
#   vaccinate_dog_cost <- runif(n=N, min = vaccinate_dog_cost[1], max = vaccinate_dog_cost[2]) 
#   # dogs vaccinated
#   dogs_vaccinated <- campaign_cost/vaccinate_dog_cost
#   # QC step to keep number of vaccinated dogs in check
#   for (position in seq(1, N)){
#     if (dogs_vaccinated[position] > dog_pop[position]){
#       dogs_vaccinated[position] <- dog_pop[position]
#     }
#   }
#   return(dogs_vaccinated)
# }
# 
# dogs_vaccinated <- dogs_vaccinated_from_campaign_cost(campaign_cost=campaign_cost, vaccinate_dog_cost=vaccinate_dog_cost, dog_pop=dog_pop)
# 
#   # vaccination coverage
# vax_cov<- dogs_vaccinated/dog_pop
#   # Estimate susceptible dogs in population 
# sus_dogs <- dog_pop - dogs_vaccinated 


# Approach 2 - assume a target vaccine coverage e.g. 70%, therefore campaign costs are an `outcome` rather than input
dogs_vaccinated <- dog_pop * target_vax_cov
sus_dogs <- dog_pop - dogs_vaccinated # Estimate susceptible dogs in population
# MDV campaign cost 
#campaign_cost <- dogs_vaccinated * (runif(n=N, min = vaccinate_dog_cost[1], max = vaccinate_dog_cost[2]))

MDV_campaign_cost <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  MDV_campaign_cost[,year] <- dogs_vaccinated * (runif(n=N, min = vaccinate_dog_cost[1], max = vaccinate_dog_cost[2]))
}



# To do ####
# modify this to take set budget as input- in place of target coverage 

vax_coverage_over_x_years <- function(target_vax_cov, horizon){
  # assume it takes 3 years to hit target vaccination coverage
  list1<- map2(0.05, target_vax_cov, seq, length.out = 3) # withought campaigns, baseline vax cov is 5-7% (private vets)
  list2<- rep(target_vax_cov, (horizon-3))
  annual_vax_cov<- unlist(append(list1, list2))
  # Condition for 0% vax cov 
  if(target_vax_cov==0){
    return(runif(n=horizon, min = target_vax_cov, max = 0.07))
  }else{
    return(annual_vax_cov)
  }
}



predict_cases <- function(nreps=N, target_vax_cov, horizon, dog_pop, rabies_inc){
  set.seed(123)
  # model inputs
  vax_model_samples <- read.csv("./data/cases_from_vax_par_samples.csv")
  vax_case_model_samples <- read.csv("./data/cases_from_vax+cases_par_samples.csv")
  # model output
  vc_last_year <- vax_coverage_over_x_years(target_vax_cov, horizon)
  cases_mat <- matrix(NA,nrow=nreps,ncol=horizon)
  
  # Estimate cases 
  for(rep in 1:nreps){
    pars_sim <- vax_model_samples[sample.int(nrow(vax_model_samples),size=1),]
    mu <- exp(sum(pars_sim[1:2]*c(1,vc_last_year[1]),log(dog_pop[rep])))
    cases_mat[rep,1] <- min(rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[3])),rabies_inc[2]*dog_pop[rep])
    
    # if condition
    
    
    pars_sim <- vax_case_model_samples[sample.int(nrow(vax_case_model_samples),size=1),]
    
    for(year in 2:horizon){
      mu <- exp(sum(pars_sim[1:3]*c(1,vc_last_year[year],log(cases_mat[rep,year-1]+1)),log(dog_pop[rep])))
      cases_mat[rep,year] <- min(rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[4])),rabies_inc[2]*dog_pop[rep])
    }
    
  }
  return(cases_mat)
}


ts_rabid_dogs<- predict_cases(nreps=N, target_vax_cov=target_vax_cov, horizon = horizon, dog_pop= dog_pop, rabies_inc=rabies_inc) 


# Rabid bites
# Simulate expected exposures for a given number of rabid dogs (
nBites <- function(dog_pop, pBite, pBiteK){
  sum(rnbinom(n = dog_pop,  mu = pBite, size = pBiteK))
}

## Exposures from times series of rabid dogs
ts_exposures <- matrix(NA,nrow=N,ncol=horizon)
for (year in seq(1,horizon)){
  ts_exposures[,year] <- sapply(FUN = nBites, pBite = mu, pBiteK = k, X = ts_rabid_dogs[,year]) # select which column to run this on
}

## IBCM
# Rabid biting dogs
ts_rabid_biting_dogs <- matrix(NA,nrow=N,ncol=horizon)
#######
# To come back here
for (year in 1:horizon){
  ts_rabid_biting_dogs[,year] <- rbinom(n=N,  size = ts_exposures[,year], prob = k)
}


# Rabid biting dogs that are investigated
ts_rabid_biting_investigated <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  ts_rabid_biting_investigated[,year] <- rbinom(n=N,  size = ts_rabid_biting_dogs[,year], prob = pInvestigate)
}

# Rabid biting dogs that are found
ts_rabid_biting_found <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  ts_rabid_biting_found[,year] <- rbinom(n=N,  size = ts_rabid_biting_investigated[,year], prob = pFound)
}

# Rabid biting dogs that are testable
ts_rabid_biting_testable <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  ts_rabid_biting_testable[,year] <- rbinom(n=N,  size = ts_rabid_biting_found[,year], prob = pTestable)
}


# Add probability that a healthy animal bite is flagged as potentially suspicious (0.05)
# false positives
ts_healthy_FP <- matrix(NA,nrow=N,ncol=horizon)
for (year in 1:horizon){
  ts_healthy_FP[,year] <- rbinom(n=N,  size = ts_rabid_biting_found[,year], prob = pFN)
}

# Create patient time series for a given population 
# consider using IBCM data and running for both low- and high-risk bite patient incidence 
# for range of bite incidence (high or low risk!) simulate bite patient time series 

# Healthy bites
sim_patient_ts = function(pop, inc_range, horizon){
  inc <- runif(horizon, min = inc_range[1], max = inc_range[2]) # select incidence each year over time horizon
  ts <- unlist(lapply(FUN = rpois, n=N, X = inc/100000 * pop)) # convert incidence into bite patients per year for population
  return(ts) 
}


# Healthy bites #####
sim_patient_ts = function(pop, inc_range, horizon){
  inc <- runif(horizon, min = inc_range[1], max = inc_range[2]) # select incidence each year over time horizon
  # open matrix for output
  ts_healthy_bites <- matrix(nrow = N, ncol = horizon)
  #loop through years
  for (year in seq(1, horizon)){
    ts_healthy_bites[,year] <- unlist(lapply(FUN = rpois, n=N, X = inc[year]/100000 * pop)) # convert incidence into bite patients per year for population
  }
  return(ts_healthy_bites) 
}

# Persons bitten by healthy dogs
# healthy_bites <- sim_patient_ts(pop, inc_range = LR_range, horizon)
    # healthy_bites <- round(rgamma(N, shape=6.675, rate=2889.090)* dog_pop) 

ts_healthy_bites <- sim_patient_ts(pop, inc_range = LR_range, horizon)


# pSEEK for rabid bites
# exposures_seek_care <- rbinom(n=N,  size = exposures, prob = pSeek_exposure)
# exposures_do_not_seek_care <- exposures - exposures_seek_care

# time series ts_exposures_seek_care
ts_exposures_seek_care <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  ts_exposures_seek_care[,year] <- rbinom(n=N,  size = ts_exposures[,year], prob = pSeek_exposure)
}

# time series ts_exposures_do_not_seek_care
ts_exposures_do_not_seek_care <- ts_exposures - ts_exposures_seek_care


# To do #######
# PEP USE: based on health seeking (probabilities depend on PEP policies e.g. if free or charged


# Rabid bite victims:

ts_exp_start <- matrix(nrow = N, ncol = horizon)
ts_exp_complete <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  # time series start PEP
  ts_exp_start[,year] <- rbinom(n=N,  size = ts_exposures_seek_care[,year], prob = pStart_exposure)
  # time series complete PEP
  ts_exp_complete[,year] <- rbinom(n=N,  size = ts_exp_start[,year], prob = pComplete_exposure)
}

# for (year in seq(1, horizon)){
#   # time series start PEP
#   ts_exp_start[,year] <- unlist(lapply(FUN = rbinom, n=N, prob = pStart_exposure, X = ts_exposures_seek_care[,year]))
#   # time series complete PEP
#   ts_exp_complete[,year] <- unlist(lapply(FUN = rbinom, n=N, prob = pComplete_exposure, X = ts_exp_start[,year]))
# }

ts_exp_no_start <- (ts_exposures_seek_care - ts_exp_start) + ts_exposures_do_not_seek_care
ts_exp_incomplete <- ts_exp_start - ts_exp_complete


# Healthy bites

# pSEEK for healthy bites

# time series ts_exposures_seek_care
ts_healthy_seek_care <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  ts_healthy_seek_care[,year] <- rbinom(n=N,  size = ts_healthy_bites[,year], prob = pSeek_healthy)
}

# time series ts_exposures_do_not_seek_care
ts_healthy_do_not_seek_care <- ts_healthy_bites - ts_healthy_seek_care


# pStart
healthy_start <- matrix(nrow = N, ncol = horizon)
ts_healthy_complete <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  # time series start PEP
  healthy_start[,year] <- rbinom(n=N,  size = ts_healthy_seek_care[,year], prob = pStart_healthy)
  # time series complete PEP
  ts_healthy_complete[,year] <- rbinom(n=N,  size = healthy_start[,year], prob = pComplete_healthy)
}

healthy_no_start <- (ts_healthy_seek_care - healthy_start) + ts_healthy_do_not_seek_care
ts_healthy_incomplete <- healthy_start - ts_healthy_complete


# DEATHS

# pDeath
ts_deaths_no_PEP <- matrix(nrow = N, ncol = horizon)
deaths_incomplete_PEP <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  # deaths no PEP
  ts_deaths_no_PEP[,year] <- rbinom(n=N,  size = ts_exp_no_start[,year], prob = pDeath)
  # deaths incomplete PEP
  deaths_incomplete_PEP[,year] <- rbinom(n=N,  size = ts_exp_incomplete[,year], prob = 1-pPrevent)
}

ts_deaths <- ts_deaths_no_PEP + deaths_incomplete_PEP


# PEP IMPACTS (because we can see who got PEP!)

deaths_averted_PEP_complete <- matrix(nrow = N, ncol = horizon)
deaths_averted_PEP_incomplete <- matrix(nrow = N, ncol = horizon)
for (year in seq(1, horizon)){
  # deaths averted complete PEP
  deaths_averted_PEP_complete[,year] <- rbinom(n=N,  size = ts_exp_complete[,year], prob = pDeath) 
  # deaths incomplete PEP
  deaths_averted_PEP_incomplete[,year] <- rbinom(n=N,  size = (ts_exp_incomplete[,year] - deaths_incomplete_PEP[,year]), prob = pPrevent * pDeath)
}

ts_deaths_averted_PEP <-  deaths_averted_PEP_complete + deaths_averted_PEP_incomplete

# deaths_averted_PEP <- unlist(lapply(FUN = rbinom, n=N, prob = pDeath, X = ts_exp_complete[,1])) # pDeath ??
# deaths_averted_PEP_incomplete <- unlist(lapply(FUN = rbinom, n=N, prob = pPrevent * pDeath, X = exp_incomplete - deaths_incomplete_PEP))
# deaths_averted <-  deaths_averted_PEP + deaths_averted_PEP_incomplete


# PEP DELIVERED
ts_complete_PEP <- ts_exp_complete + ts_healthy_complete
ts_incomplete_PEP <- ts_exp_incomplete + ts_healthy_incomplete

# Economics #####
  ## Discount costs
future <- (1:horizon)-1
ts_cost_PEP_per_year <- ((ts_complete_PEP * full_cost) + (ts_incomplete_PEP * partial_cost)) * exp(-discount*future)
ts_MDV_campaign_cost <- MDV_campaign_cost * exp(-discount*future)

  ## Add
ts_cost_per_year <- ts_cost_PEP_per_year + ts_MDV_campaign_cost 


# list all ts outputs
my_list<- ls(pattern="ts_")
# get the matrices 
out_matrices <- lapply(my_list, function(mat) get(mat))
# name
names(out_matrices) <- my_list

# output
return(out_matrices)

}


    
no_interventions <- decision_tree(
  N=100,pop = 500000,HDR = c(98,100),horizon = 7,discount = 0.03,rabies_inc = c(0.0075, 0.0125),
  LR_range = c(6.6,12.8),mu = 0.7054917 ,k = 0.3862238,pSeek_healthy=0.6,pStart_healthy= 0.6666667,
  pComplete_healthy = 0.3968254,pSeek_exposure=0.7,pStart_exposure = 0.6666667,pComplete_exposure = 0.3968254,
  pDeath = 0.1660119,pPrevent = 0.986,full_cost = 45,partial_cost = 25,
  vaccinate_dog_cost = c(2,4),target_vax_cov = 0,
  pInvestigate = 0.9, pFound = 0.6, pTestable = 0.7, pFN = 0.05
)


summarise_stochasticity <-  function(my_matrix){
  out<- apply(my_matrix, 2, quantile, c(0.025, 0.5, 0.975), na.rm=TRUE)
  rownames(out)<-NULL
  return(out)
}


select_variable <- function(variable, scenario){
    my_matrix <- scenario[[variable]]
    out<- summarise_stochasticity(my_matrix)
    df <- as.data.frame(t(out))
    names(df) <- c('LL', 'Median', 'UL')
    return(df)
}

# return time series values 
df <- select_variable(variable='ts_healthy_seek_care', scenario=no_interventions)


# Plot
ggplot(df, aes(x = as.numeric(row.names(df)), y = Median)) +
    geom_line() +
    geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
    ylab("Value")+ xlab("Year")+
    theme_bw() 
    
