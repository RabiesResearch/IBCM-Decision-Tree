# libraries

require(pacman)
pacman::p_load(tidyverse, # cleaning, wrangling
               scales,    # display neat number values
               paletteer  # cool color palettes
               )

# source model
source("Code/stochastic_decision_tree.R")


# scenarios

## read parameters file
parameters_df <- read.csv("data/parameters.csv")

# extract parameter values from csv
run_decision_tree_from_csv <- function(scenario_name, parameters_df, pop=60000000, horizon = 7,base_vax_cov=0.05, N = 100){
  scenario_parameters <- parameters_df[parameters_df$scenario == scenario_name, ]
  
  decision_tree(
    N = N,
    pop = pop,
    horizon = horizon, 
    base_vax_cov=base_vax_cov,
    discount = scenario_parameters$discount,
    target_vax_cov = scenario_parameters$target_vax_cov,
    # epidemiological status quo
    #LR_range = c(scenario_parameters$LR_range1, scenario_parameters$LR_range1),
    HDR = c(scenario_parameters$HDR1, scenario_parameters$HDR2),
    pBite_healthy = 0.01,
    
    mu = scenario_parameters$mu,
    k = scenario_parameters$k,
    # health seeking - healthy bites
    pSeek_healthy = scenario_parameters$pSeek_healthy,
    pStart_healthy = scenario_parameters$pStart_healthy,
    pComplete_healthy = scenario_parameters$pComplete_healthy,
    # health seeking - rabid bites
    pSeek_exposure = scenario_parameters$pSeek_exposure,
    pStart_exposure = scenario_parameters$pStart_exposure,
    pComplete_exposure = scenario_parameters$pComplete_exposure,
    # biological params
    pDeath = scenario_parameters$pDeath,
    pPrevent = scenario_parameters$pPrevent,
    # economics
    full_cost = scenario_parameters$full_cost,
    partial_cost = scenario_parameters$partial_cost,
    vaccinate_dog_cost = c(scenario_parameters$vaccinate_dog_cost1, scenario_parameters$vaccinate_dog_cost2),
    # campaign cost
    #IBCM
    pInvestigate = scenario_parameters$pInvestigate,
    pFound = scenario_parameters$pFound,
    pTestable = scenario_parameters$pTestable,
    pFN = scenario_parameters$pFalseNeg
  )
}

# scenarios on a given population 
no_interventions <- run_decision_tree_from_csv("no_interventions", parameters_df)
PEP_IM_free_only <- run_decision_tree_from_csv("PEP_IM_free_only", parameters_df)
PEP_ID_free_only <- run_decision_tree_from_csv("PEP_ID_free_only", parameters_df)
MDV_only <- run_decision_tree_from_csv("MDV_only", parameters_df)
MDV_PEP_IM_free <- run_decision_tree_from_csv("MDV_PEP_IM_free", parameters_df)
MDV_PEP_ID_free <- run_decision_tree_from_csv("MDV_PEP_ID_free", parameters_df)



# find upper limit, median and lower limit from list of length N
summarise_stochasticity <-  function(my_matrix){
  out<- apply(my_matrix, 2, quantile, c(0.025, 0.5, 0.975), na.rm=TRUE)
  rownames(out)<-NULL
  return(out)
}

# select variable from an object/scenario
select_variable <- function(variable, scenario){
  my_matrix <- scenario[[variable]]
  out<- summarise_stochasticity(my_matrix)
  df <- as.data.frame(t(out))
  names(df) <- c('LL', 'Median', 'UL')
  return(df)
}

names(no_interventions)

# return time series values 
df <- select_variable(variable='ts_rabid_dogs', scenario=MDV_only)
# run this across all variables and store the summarised object instead

# Plotting to check
ggplot(df, aes(x = as.numeric(row.names(df)), y = Median)) +
  geom_line() +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  ylab("ts_rabid_dogs")+ xlab("Year")+
  theme_bw() 


# Compare >2 scenarios (using cumulative values over horizon ie instead of yearly)
compare_scenarios <- function(variable, ...){
  # Get the list of scenarios from arguments
  scenarios <- list(...)
  scenario_names <- as.character(substitute(list(...)))[-1]  # Capture the names of the scenarios
  
  # Process each scenario
  compare_df_list <- mapply(function(scenario, name) {
    df <- colSums(select_variable(variable, scenario))
    df$scenario <- name  # Add the scenario name as a new column
    return(df)
  }, scenario = scenarios, name = scenario_names, SIMPLIFY = FALSE)
  
  # Combine the results into one dataframe
  compare_df <- as.data.frame(do.call("rbind", compare_df_list))
  
  # Convert list columns into a more manageable data type
  for (col_name in names(compare_df)) {
    if (is.list(compare_df[[col_name]])) {
      compare_df[[col_name]] <- as.character(unlist(compare_df[[col_name]]))
    }
  }
  
  return(compare_df)
  
}

compare_scenarios("ts_cost_PEP_per_year", PEP_IM_free_only, PEP_ID_free_only)

# Deaths averted by MDV (cumulative over horizon)
deaths_df <- compare_scenarios("ts_deaths", no_interventions, MDV_only) %>%
  dplyr::mutate(across(c(LL, Median, UL), as.numeric))

deaths_averted_MDV <- deaths_df[deaths_df$scenario == "no_interventions", ][,1:3] - deaths_df[deaths_df$scenario == "MDV_only", ][,1:3] 


# PEP COSTS #####
# Plot costs under different policy choice across horizon
pep_costs <- compare_scenarios("ts_cost_PEP_per_year", PEP_IM_free_only, PEP_ID_free_only) %>%
  as.data.frame() %>%
  dplyr::mutate(scenario = case_when(
    scenario == "PEP_IM_free_only" ~ "Intramuscular",
    scenario == "PEP_ID_free_only" ~ "Intradermal")
  )
  

# convert columns to numeric
pep_costs$LL <- as.numeric(pep_costs$LL)
pep_costs$Median <- as.numeric(pep_costs$Median)
pep_costs$UL <- as.numeric(pep_costs$UL)


ggplot(pep_costs, aes(x=scenario, y=Median, color=scenario)) +
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2,
                position=position_dodge(0.05)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values=c('#999999', '#E69F00'))+
  labs(title = "Estimated national costs of PEP",
       y = "US dollars")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle=0, color="black", size=13),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.y = element_text(color="black", size=13),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) 


# Compare deaths &/ lives saved across policy choices #####

cumulative_deaths <- compare_scenarios("ts_deaths", no_interventions, PEP_ID_free_only, MDV_only, MDV_PEP_ID_free) %>%
  as.data.frame() %>%
  dplyr::mutate(scenario = case_when(
    scenario == "no_interventions" ~ "Status quo",
    scenario == "PEP_ID_free_only" ~ "free PEP",
    scenario == "MDV_only" ~ "MDV",
    scenario == "MDV_PEP_ID_free" ~ "MDV and free PEP")
  )

# convert columns to numeric
cumulative_deaths$LL <- as.numeric(cumulative_deaths$LL)
cumulative_deaths$Median <- as.numeric(cumulative_deaths$Median)
cumulative_deaths$UL <- as.numeric(cumulative_deaths$UL)

# Reorder the levels of scenario factor
cumulative_deaths <- cumulative_deaths %>%
  mutate(scenario = factor(scenario, levels = unique(scenario[order(-Median)])))


ggplot(cumulative_deaths, aes(x=scenario, y=Median, color=scenario)) +
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2,
                position=position_dodge(0.05)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = grDevices::rgb(1,0,0, alpha = c(1, 0.8, 0.6, 0.4))) +
  #scale_color_paletteer_d("colorBlindness::Blue2DarkRed12Steps") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle=0, color="black", size=13),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.text.y = element_text(color="black", size=13),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  coord_flip() +
  labs(title = "Impact of policy choice",
       y = "Human deaths")





# plot

# plot_slope_chart <- function(data) {
#   # Ensure data is ordered by scenario
#   #data <- data[order(data$scenario), ]
#   
#   # Define the color palette
#   colors <- paletteer_d("beyonce::X6")
#   
#   ggplot(my_data, aes(x = scenario, y = Median, group = 1)) +
#     geom_line(color = "grey", size = 0.5) +
#     geom_point(aes(y = Median, color = scenario), size = 3) #+
#     # #geom_crossbar(aes(ymin = LL, ymax = UL, color = scenario), width = 0.2, size = 0.5) +
#     # geom_linerange(aes(ymin = LL, ymax = UL, color = scenario), size = 0.5) +
#     # labs(y = "Value", x = "Scenario") +
#     # #theme_minimal() +
#     # #scale_color_manual(values = colors, name = "Scenario") +
#     # scale_y_continuous(labels = comma)+ 
#     # theme(legend.position = "none")
# }
# 
# plot_slope_chart(my_data)
# 
# 
# as.factor(unlist(my_data$scenario))
# 
# 
# data <- my_data$LL



