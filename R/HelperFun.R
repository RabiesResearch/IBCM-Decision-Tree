####################################################################################
# HELPER FUNCTIONS
####################################################################################
# nEvents() - simulate expected events given observation probability (e.g. deaths total given those observed, or total rabid dogs given surveillance data etc
# nBites() - simulate persons bitten for given number of rabid dogs (or healthy dogs) given negative binomial process of biting
# nRabid() - simulate for given number of exposed rabid dogs (or healthy dogs) given negative binomial process of biting
# sim_ts() - simulate time series (e.g. rabid dogs) over specified horizon, given specific conditions

####################################################################################
# Simulate expected deaths given observation probability and n observations (deaths)
nEvents <- function(obsEvents, pObs){
  
  # initialize before starting while loop
  totalEvents <- 0 
  Obs <- 0
  
  # simulate trials (deaths and observation process) until all observed deaths seen
  while(Obs < obsEvents){
    totalEvents <- totalEvents +1
    Obs <- Obs + rbinom(1, 1, pObs)
  }
  totalEvents # print total deaths given input of observed deaths
}
## Check results ok:
# replicate(100, nEvents(25, 0.30))

####################################################################################
# Simulate expected exposures for a given number of rabid dogs (or bites by healthy dogs)
nBites <- function(dogs, pBite, pBiteK){
  sum(rnbinom(n = dogs,  mu = pBite, size = pBiteK))
}

# pBite = 0.38; pBiteK = 0.2
# nBites(dogs = 100,  pBite = pBite, pBiteK = pBiteK)
# # Check results ok:
# hist(replicate(1000, nBites(50, 0.38, 0.2)), breaks = 1:150)
# quantile(replicate(1000, nBites(50, 0.38, 0.2)), c(0.025, 0.5, 0.975))
# # check function can be applied to replicated rabid dogs
# rabid_vec = c(30,40,50,60,50,60)
# unlist(lapply(FUN = nBites, pBite = 0.38, pBiteK = 0.2, X = rabid_vec))

####################################################################################
# Function to simulate expected rabid dogs given exposures
# Negative binomial function (better but harder to parameterize)
nRabid_NB <- function(exposures, pBite, pBiteK){
  
  # initialize before starting while loop
  bites <- 0 
  RabidDogs <- 0
  
  # simulate trials (bites by rabid dogs) until all exposures occurred
  while(bites < exposures){
    RabidDogs <- RabidDogs + 1
    bites <- bites + nBites(1,  pBite, pBiteK)
  }
  RabidDogs 
}

# Poisson function (worse but simpler)
nRabid_pois <- function(exposures, pBite){
  
  # initialize before starting while loop
  bites <- 0 
  RabidDogs <- 0
  
  # simulate trials (bites by rabid dogs) until all exposures occurred
  while(bites < exposures){
    RabidDogs <- RabidDogs + 1
    bites <- bites + rpois(1,  pBite)
  }
  RabidDogs 
}

# ## Check results ok:
# par(mfrow = c(2,1))
# hist(replicate(1000, nRabid_pois(1000, 0.38))) # Poisson biting
# hist(replicate(1000, nRabid_NB(1000, 0.38, 0.14))) # Negative binomial biting

# ## check function can be applied to replicated exposures
# exp_vec = c(3,4,5,6,5,6)
# unlist(lapply(FUN = nRabid_pois, pBite = 0.38, X = exp_vec))
# unlist(lapply(FUN = nRabid_NB, pBite = 0.38, pBiteK = 0.14, X = exp_vec))

####################################################################################
# Create timeseries of rabid dogs - based on human population, HDR, specified incidence range & time horizon
sim_rabies_ts = function(pop, HDR, rabies_inc, horizon){ 
  dogs <- pop/HDR # Estimate dog population
  rabies_inc <- runif(1, min = rabies_inc[1], max = rabies_inc[2]) # generate rabies incidence from range (min-max)
  rabid_dogs <- rpois(horizon, lambda = rabies_inc * dogs) # get rabid dogs per year over time horizon under the incidence specified
  return(rabid_dogs)
}
# example: times series of rabid dogs from population of ~400k with high human: dog ratio (100:1)
# baseline of rabies incidence between 0.75-1.25%, over 10y time horizon
# sim_rabies_ts(400000, 100, c(0.0075, 0.0125), 10)

# Create patient time series for a given population 
# consider using IBCM data and running for both low- and high-risk bite patient incidence 
# for range of bite incidence (high or low risk!) simulate bite patient time series 
sim_patient_ts = function(pop, inc_range, horizon){
  inc <- runif(horizon, min = inc_range[1], max = inc_range[2]) # select incidence each year over time horizon
  ts <- unlist(lapply(FUN = rpois, n=1, X = inc/100000 * pop)) # convert incidence into bite patients per year for population
  return(ts) 
}
# for a population of 400,000 people, with a low risk bite patient incidence range of 20-60/ 100,000, simulate over 20 years
# sim_patient_ts(400000, inc_range = c(20, 60), horizon = 20) 

