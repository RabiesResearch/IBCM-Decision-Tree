# Decision tree model that can be applied to create different scenarios 
# Simulate scenarios & est. impacts & CE of interventions
# examine over time horizon (specify years) & can discount (3% typically)

# FUNCTIONS:
# decision_tree() # The core decision tree model
# decision_tree_ndraw() # To run decision tree N times
# horizon_iter() # summarise outcomes of decision tree (Nx) over horizon
# sum_horizon() # Summarise horizon_iter() outcomes by quantiles  
# horizon_CEA() # Estimate intervention cost-effectiveness (per death averted) in comparison to a specified baseline
# calc_ICER() # Calculate ICER (difference in deaths!) of intervention vs baseline
# year_summary() # summarise outcomes of decision tree (Nx) EACH YEAR in quantiles (i.e. see changes over time across horizon)
# year_avg() # summarise mean outcomes of decision tree (Nx) EACH YEAR (so we can see non-integer estimates of deaths!)

# Note that HelperFun.R must be loaded first!

decision_tree <- function(pop, HDR, horizon, discount,
                          rabies_inc, LR_range, mu, k, pStart, pComplete, pDeath, pPrevent, 
                          full_cost, partial_cost, campaign_cost)
  {
  
  # SIMULATE TIMESERIES
  # Rabies cases based on case detection & relative to VC round (1,2,3,4)
  ts_rabid <- sim_rabies_ts(pop, HDR, rabies_inc, horizon)
  # Exposures from times series of rabid dogs
  exposures <- unlist(lapply(FUN = nBites, pBite = mu, pBiteK = k, X = ts_rabid))
  # Persons bitten by healthy dogs
  healthy_bites <- sim_patient_ts(pop, inc_range = LR_range, horizon) 

  # PEP USE: based on health seeking (probabilities depend on PEP policies e.g. if free or charged
  # Rabid bite victims:
  exp_start <- unlist(lapply(FUN = rbinom, n=1, prob = pStart, X = exposures))
  exp_no_start <- exposures - exp_start
  exp_complete <- unlist(lapply(FUN = rbinom, n=1, prob = pComplete, X = exp_start))
  exp_incomplete <- exp_start - exp_complete
  
  # healthy bite victims
  healthy_start <-  healthy_bites # Start PEP
  healthy_complete <- unlist(lapply(FUN = rbinom, n=1, prob = pComplete, X = healthy_start)) # Complete PEP
  healthy_incomplete <- healthy_start - healthy_complete
 
  # DEATHS
  deaths_no_PEP <- unlist(lapply(FUN = rbinom, n=1, prob = pDeath, X = exp_no_start))
  deaths_incomplete_PEP <- unlist(lapply(FUN = rbinom, n=1, prob = 1-pPrevent, X = exp_incomplete))
  deaths <- deaths_no_PEP + deaths_incomplete_PEP

  # PEP IMPACTS (because we can see who got PEP!)
  deaths_averted_PEP <- unlist(lapply(FUN = rbinom, n=1, prob = pDeath, X = exp_complete))
  deaths_averted_PEP_incomplete <- unlist(lapply(FUN = rbinom, n=1, prob = pPrevent * pDeath, X = exp_incomplete - deaths_incomplete_PEP))
  deaths_averted <-  deaths_averted_PEP + deaths_averted_PEP_incomplete
  
  # PEP DELIVERED
  complete <- exp_complete + healthy_complete
  incomplete <- exp_incomplete + healthy_incomplete
  
  # Discount costs
  future <- (1:horizon)-1
  cost_PEP_per_year <- ((complete * full_cost) + (incomplete * partial_cost)) * exp(-discount*future)
  cost_MDV_per_year <- campaign_cost * exp(-discount*future)
  cost_per_year <- cost_PEP_per_year + cost_MDV_per_year
  
  # Output results
  return( 
    data.frame(
      years = 1:horizon,
      rabid_dogs = ts_rabid, 
      exposures = exposures,
      bite_patients = healthy_start + exp_start,
      healthy_bite_patients = healthy_start,
      deaths = deaths, 
      lives_saved = deaths_averted, # From PEP only (not MDV!)
      PEP_complete = complete,
      PEP_incomplete = incomplete, 
      PEP_cost = cost_PEP_per_year,
      MDV_cost = cost_MDV_per_year, # MDV costs included but impact on dog rabies incidence not yet incorporated (so leave as zero if no MDV)
      all_cost = cost_per_year
    )
  )
}

# # Example of low health seeking like on Pemba when charged for PEP
# test1 <- decision_tree(pop = 400000, HDR = 100, horizon = 10, discount = 0.03,
#               rabies_inc = c(0.0075, 0.0125), LR_range = c(6.6,12.8), mu = 0.38, k = 0.14,
#               pStart = 0.6666667, pComplete = 0.3968254, pDeath = 0.1660119, pPrevent = 0.986,
#               full_cost = 45, partial_cost = 25, campaign_cost = 0)
# # Example of better health seeking like on Pemba when PEP free
# test2 <- decision_tree(pop = 400000, HDR = 100, horizon = 10, discount = 0.03,
#               rabies_inc = c(0.0075, 0.0125), LR_range = c(20,37), mu = 0.38, k = 0.14,
#               pStart = 0.7833333, pComplete = 0.8722222, pDeath = 0.1660119, pPrevent = 0.986,
#               full_cost = 45, partial_cost = 25, campaign_cost = 0)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# function to run decision tree N times
decision_tree_ndraw <- function(ndraw, 
                                pop, HDR, horizon, discount,
                                rabies_inc, LR_range, mu, k, 
                                pStart, pCompletePEP, pDeath, pPrevent, 
                                full_cost, partial_cost, campaign_cost){
  draws<-vector("list",ndraw)
  for(i in 1:ndraw){
    draws[[i]]<-cbind.data.frame(
      decision_tree(
        pop, HDR, horizon, discount, 
        rabies_inc, LR_range, mu, k, 
        pStart, pCompletePEP, pDeath, pPrevent, 
        full_cost, partial_cost, campaign_cost),
      iter=i)
  }
  draws<-do.call("rbind",draws)
  return(draws)
}

# # TEST
# out <- decision_tree_ndraw(ndraw = 1000, 
#                            pop = 400000, HDR = 100, horizon = 10, discount = 0.03, 
#                            rabies_inc = c(0.0075, 0.0125), LR_range = c(6.6,12.8), mu = 0.38, k = 0.14,
#                            pStart = 0.6666667, pComplete = 0.3968254, pDeath = 0.1660119, pPrevent = 0.986,
#                            full_cost = 45, partial_cost = 25, campaign_cost = 0)

###########################################
horizon_iter <- function(model_output){
  sums = model_output %>%
    dplyr::group_by(iter) %>%
    dplyr::summarise(deaths = sum(deaths),
                     exposures = sum(exposures),
                     deaths_averted = sum(lives_saved), # Deaths averted from PEP only (depends on rabies incidence)
                     PEP_cost = sum(PEP_cost), # cost of PEP for both healthy bite patients & rabies exposures who sought care
                     MDV_cost = sum(MDV_cost),
                     total_cost = sum(all_cost))
  sums$cost_per_death_averted_PEP <-  sums$total_cost/sums$deaths_averted 
  return(sums)
}
# SQ = horizon_iter(out)
# apply(SQ, 2, quantile, c(0.025, 0.5, 0.975))

sum_horizon <- function(out, scenario, disc, PEP){
  out_horizon <- horizon_iter(out) # summarize over entire horizon
  out_summary <- as.data.frame(apply(out_horizon, 2, quantile, c(0.025, 0.5, 0.975), na.rm=TRUE)) # quantiles
  colnames(out_summary)<-c("iter","deaths","exposures","DA","PEP_cost", "MDV_cost", "cost", "Cost_per_DA_PEP")
  N <- max(out_summary$iter)
  out_summary$CIs <- round(out_summary$iter*100/N)
  out_summary$scenario <- scenario
  out_summary$discount <- disc
  out_summary$PEP <- PEP
  return(out_summary)
}
# sum_base <- sum_horizon(out, "none", 0.03, "ID")

horizon_CEA <- function(baseline_sum, comparator_sum){
  comparator_sum$deaths_avert <- baseline_sum$deaths - comparator_sum$deaths
  comparator_sum$cost_per_deaths_avert <- comparator_sum$cost/comparator_sum$deaths_avert
  return(comparator_sum)
}
# horizon_CEA(sum_base, sum_OH)

# Function to calculate the incremental cost-effectiveness comparing 2 scenarios (baseline vs intervention)
calc_ICER <- function(baseline, comparator){ # provide scenario summaries
  ICER_summary <- data.frame(
    cost_diff = baseline$cost - comparator$cost,
    death_diff = baseline$deaths - comparator$deaths
    )
    ICER_summary$baseline = paste0(baseline$scenario[1], "-", baseline$discount[1], "-", baseline$PEP)
    ICER_summary$comparator = paste0(comparator$scenario[1], "-", comparator$discount[1], "-", comparator$PEP)
    ICER_summary$CIs = baseline$CIs
    return(ICER_summary)
}
# sq_scen <- sum_horizon(out_SQ, "SQ", 0.03, "IM")
# oh_scen <- sum_horizon(out_OH, "OH", 0.03, "ID")
# calc_ICER(sq_scen, oh_scen)

##########################################################
# Function to evaluate impacts of interventions over time:
# Summarize quantiles by year
year_summary <- function(model_output, q, scenario){
  sums = model_output %>%
    dplyr::group_by(years) %>%
    dplyr::summarise(deaths = quantile(deaths, q),
                     exposures = quantile(exposures, q),
                     deaths_averted = quantile(lives_saved, q),
                     total_cost = quantile(all_cost, q),
                     PEP_cost = quantile(PEP_cost, q),
                     MDV_cost = quantile(MDV_cost, q))
#  sums$cost_per_death_averted <-  sums$total_cost/sums$deaths_averted
  sums$scenario <- as.factor(scenario)
  sums$metric <- as.factor(q)
  return(sums)
}

# Mean over time
year_avg <- function(model_output, scenario){
  sums = model_output %>%
    dplyr::group_by(years) %>%
    dplyr::summarise(deaths = mean(deaths),
                     exposures = mean(exposures),
                     deaths_averted = mean(lives_saved),
                     total_cost = mean(all_cost),
                     PEP_cost = mean(PEP_cost),
                     MDV_cost = mean(MDV_cost))
  #  sums$cost_per_death_averted <-  sums$total_cost/sums$deaths_averted
  sums$scenario <- as.factor(scenario)
  sums$metric <- as.factor("avg")
  return(sums)
}
#year_avg(out_SQ, "SQ")
