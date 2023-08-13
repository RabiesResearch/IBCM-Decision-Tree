library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(RColorBrewer)
library(knitr)
library(dplyr)
library(tidyverse)

source("Code/decision_tree_ForDavid.R") # Decision Tree model & summary functions

# https://debruine.github.io/shinyintro/reports.html
# https://stackoverflow.com/questions/57802225/how-to-pass-table-and-plot-in-shiny-app-as-parameters-to-r-markdown

ui <- dashboardPage(
  dashboardHeader(title = "IBCM Planning",
                  titleWidth = 280,
                  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Model Parameters"),
        fluidRow(
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 selectInput("GraphicalOutput", "Graphical Output", 
                             c("Complete PEP" = "ts_complete_PEP",
                               "PEP Cost Per Year" = "ts_cost_PEP_per_year",
                               "Cost Per Year" = "ts_cost_per_year",
                               "Deaths" = "ts_deaths",
                               "Deaths Without PEP" = "ts_deaths_no_PEP",
                               "Deaths Averted" = "ts_deaths_averted_PEP",
                               "Exposures" = "ts_exposures",
                               "Exposures Completing PEP" = "ts_exp_complete",
                               "Exposures Not Completing PEP" = "ts_exp_incomplete",
                               "Exposures Starting PEP" = "ts_exp_start",
                               "Esposures Not Starting PEP" = "ts_exp_no_start",
                               "Exposures Seeking Care" = "ts_exposures_seek_care",
                               "Exposures Not Seeking Care" = "ts_exposures_do_not_seek_care",
                               "Healthy Bites" = "ts_healthy_bites",
                               "Healthy Bites Completing PEP" = "ts_healthy_complete",
                               "Healthy Bites Not Completing PEP" = "ts_healthy_incomplete",
                               "Healthy Bites FP" = "ts_healthy_FP",
                               "Healthy Bites Seeking Care" = "ts_healthy_seek_care",
                               "Healthy Bites Not Seeking Care" = "ts_healthy_do_not_seek_care",
                               "Total Incomplet PEP" = "ts_incomplete_pep",
                               "MDV Campaign Costs" = "ts_MDV_campaign_cost",
                               "Rabid Dogs" = "ts_rabid_dogs",
                               "Rabid Biting Dogs" = "ts_rabid_biting_dogs",
                               "Rabid Biting Dogs Investigated" = "ts_rabid_biting_investigated",
                               "Rabid Biting Dogs Found" = "ts_rabid_biting_found",
                               "Rabid Biting Dogs Testable" = "ts_rabid_biting_testable")))
        ),
        h3("Model Parameters"),
        fluidRow(
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("Years", "Years", value = 7, min = 1, step = 1))
        ),
        h3("Population Inputs"),
        fluidRow(
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("Humans", "Human Population", value = 500000, min = 1, step = 1)),
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericRangeInput("HDR", "Human Dog Ratio", value = c(8,10), min = 0.1, max = 100000, step = 0.1))
        ),
        h3("Healthy Bites"),
        fluidRow(
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pSeekH", "pSeek", value = 0.6, min = 0, max = 1, step = 0.001)),
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pStartH", "pStart", value = 0.6666667, min = 0, max = 1, step = 0.001)),
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pCompleteH", "pComplete", value = 0.3968254, min = 0, max = 1, step = 0.001))
        ),
        h3("Rabid Bites"),
        fluidRow(
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pSeekE", "pSeek", value = 0.7, min = 0, max = 1, step = 0.001)),
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pStartE", "pStart", value = 0.6666667, min = 0, max = 1, step = 0.001)),
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pCompleteE", "pComplete", value = 0.3968254, min = 0, max = 1, step = 0.001)),
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pDeath", "pDeath", value = 0.1660119, min = 0, max = 1, step = 0.001)),
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pPrevent", "pPrevent", value = 0.986, min = 0, max = 1, step = 0.001))
        ),
        h3("Economic Inputs"),
        fluidRow(
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("FullCost", "Complete PEP Cost", value = 45, min = 0, step = 0.01)),
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("PartCost", "Incomplete PEP Cost", value = 25, min = 0, step = 0.01)),
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("Inflation", "Inflation", value = 0.03, min = 0, max = 1, step = 0.001))
        ),
        h3("Vaccination Inputs"),
        fluidRow(
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericRangeInput("VacDogCost", "Cost Per Vaccination", value = c(2,4), min = 0, step = 0.01)),
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("VacCovTarget", "Vaccination Coverage Target", value = 0.0, min = 0, max = 1, step = 0.01))
        ),
        h3("IBCM Investiagtions"),
        fluidRow(
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pInvestigate", "Percentage of dogs investigated", value = 0.9, min = 0, step = 0.01)),
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pFound", "Percentage of investigated dogs found", value = 0.6, min = 0, max = 1, step = 0.001)),
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pTestable", "Percentage of found dogs testable", value = 0.7, min = 0, step = 0.01)),
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pFN", "Percentage of test errors", value = 0.05, min = 0, max = 1, step = 0.001))
        ),
      ),
      mainPanel(
        fluidPage(
          plotOutput("Graph"),
          valueBoxOutput("TotalCost"),
          valueBoxOutput("TotalCostPEP"),
          valueBoxOutput("TotalCostMDV"),
          valueBoxOutput("TotalExposures"),
          valueBoxOutput("TotalExposuresSeek"),
          valueBoxOutput("TotalExposuresNoSeek"),
          valueBoxOutput("TotalExposuresStart"),
          valueBoxOutput("TotalExposuresNoStart"),
          valueBoxOutput("TotalExposuresComplete"),
          valueBoxOutput("TotalDeaths"),
          valueBoxOutput("TotalDeathsNoPEP"),
          valueBoxOutput("TotalDeathsAverted"),
          valueBoxOutput("TotalRabidDogs"),
          valueBoxOutput("TotalRabidBitingDogs"),
          valueBoxOutput("TotalRabidBitingInvestigated"),
          valueBoxOutput("TotalRabidBitingFound"),
          valueBoxOutput("TotalRabidBitingTestable")
        )
      )
    ))
))

server <- function(input,output) {
  
  Results <- reactive({
    
    Results <- decision_tree(N = 100, 
                                   pop = input$Humans, 
                                   HDR = c(input$HDR[1], input$HDR[2]), 
                                   horizon = input$Years, 
                                   discount = input$Inflation, 
                                   rabies_inc = c(0.0075, 0.0125), 
                                   LR_range = c(6.6, 12.8), 
                                   mu = 0.7054917, 
                                   k = 0.3862238, 
                                   pSeek_healthy = input$pSeekH, 
                                   pStart_healthy = input$pStartH, 
                                   pComplete_healthy = input$pCompleteH, 
                                   pSeek_exposure = input$pSeekE, 
                                   pStart_exposure = input$pStartE, 
                                   pComplete_exposure = input$pCompleteE, 
                                   pDeath = input$pDeath, 
                                   pPrevent = input$pPrevent, 
                                   full_cost = input$FullCost, 
                                   partial_cost = input$PartCost, 
                                   vaccinate_dog_cost = c(input$VacDogCost[1], input$VacDogCost[2]), 
                                   target_vax_cov = input$VacCovTarget, 
                                   pInvestigate = input$pInvestigate,
                                   pFound = input$pFound,
                                   pTestable = input$pTestable,
                                   pFN = input$pFN)
    
    Results
    
  })
  
  ts_complete_PEP <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_complete_PEP', scenario=DF1)
    DF2
  })

  ts_cost_PEP_per_year <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_cost_PEP_per_year', scenario=DF1)
    DF2
  })
  
  ts_cost_per_year <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_cost_per_year', scenario=DF1)
    DF2
  })
  
  ts_deaths <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_deaths', scenario=DF1)
    DF2
  })
  
  ts_deaths_averted_PEP <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_deaths_averted_PEP', scenario=DF1)
    DF2
  })
  
  ts_deaths_no_PEP <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_deaths_no_PEP', scenario=DF1)
    DF2
  })
  
  ts_exp_complete <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_exp_complete', scenario=DF1)
    DF2
  })
  
  ts_exp_incomplete <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_exp_incomplete', scenario=DF1)
    DF2
  })
  
  ts_exp_no_start <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_exp_no_start', scenario=DF1)
    DF2
  })
  
  ts_exp_start <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_exp_start', scenario=DF1)
    DF2
  })
  
  ts_exposures <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_exposures', scenario=DF1)
    DF2
  })
  
  ts_exposures_do_not_seek_care <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_exposures_do_not_seek_care', scenario=DF1)
    DF2
  })
  
  ts_exposures_seek_care <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_exposures_seek_care', scenario=DF1)
    DF2
  })
  
  ts_healthy_bites <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_healthy_bites', scenario=DF1)
    DF2
  })
  
  ts_healthy_complete <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_healthy_complete', scenario=DF1)
    DF2
  })
  
  ts_healthy_do_not_seek_care <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_healthy_do_not_seek_care', scenario=DF1)
    DF2
  })
  
  ts_healthy_FP <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_healthy_FP', scenario=DF1)
    DF2
  })
  
  ts_healthy_incomplete <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_healthy_incomplete', scenario=DF1)
    DF2
  })
  
  ts_healthy_seek_care <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_healthy_seek_care', scenario=DF1)
    DF2
  })
  
  ts_incomplere_PEP <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_incomplere_PEP', scenario=DF1)
    DF2
  })
  
  ts_MDV_campaign_cost <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_MDV_campaign_cost', scenario=DF1)
    DF2
  })
  
  ts_rabid_biting_dogs <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_rabid_biting_dogs', scenario=DF1)
    DF2
  })
  
  ts_rabid_biting_found <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_rabid_biting_found', scenario=DF1)
    DF2
  })
  
  ts_rabid_biting_investigated <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_rabid_biting_investigated', scenario=DF1)
    DF2
  })
  
  ts_rabid_biting_testable <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_rabid_biting_testable', scenario=DF1)
    DF2
  })
  
  ts_rabid_dogs <- reactive({
    DF1 <- Results()
    DF2 <- select_variable(variable='ts_rabid_dogs', scenario=DF1)
    DF2
  })
  
  observe({
    selectedDataframe <- switch(input$GraphicalOutput,
                                "ts_complete_PEP" = ts_complete_PEP(),
                                "ts_cost_PEP_per_year" = ts_cost_PEP_per_year(),
                                "ts_cost_per_year" = ts_cost_per_year(),
                                "ts_deaths" = ts_deaths(),
                                "ts_deaths_averted_PEP" = ts_deaths_averted_PEP(),
                                "ts_deaths_no_PEP" = ts_deaths_no_PEP(),
                                "ts_exp_complete" = ts_exp_complete(),
                                "ts_exp_incomplete" = ts_exp_incomplete(),
                                "ts_exp_no_start" = ts_exp_no_start(),
                                "ts_exp_start" = ts_exp_start(),
                                "ts_exposures" = ts_exposures(),
                                "ts_exposures_do_not_seek_care" = ts_exposures_do_not_seek_care(),
                                "ts_exposures_seek_care" = ts_exposures_seek_care(),
                                "ts_healthy_bites" = ts_healthy_bites(),
                                "ts_healthy_complete" = ts_healthy_complete(),
                                "ts_healthy_do_not_seek_care" = ts_healthy_do_not_seek_care(),
                                "ts_healthy_FP" = ts_healthy_FP(),
                                "ts_healthy_incomplete" = ts_healthy_incomplete(),
                                "ts_healthy_seek_care" = ts_healthy_seek_care(),
                                "ts_incomplere_PEP" = ts_incomplere_PEP(),
                                "ts_MDV_campaign_cost" = ts_MDV_campaign_cost(),
                                "ts_rabid_biting_dogs" = ts_rabid_biting_dogs(),
                                "ts_rabid_biting_found" = ts_rabid_biting_found(),
                                "ts_rabid_biting_investigated" = ts_rabid_biting_investigated(),
                                "ts_rabid_biting_testable" = ts_rabid_biting_testable(),
                                "ts_rabid_dogs" = ts_rabid_dogs(),
    )
    
    output$Graph <- renderPlot({ggplot(selectedDataframe, aes(x = as.numeric(row.names(selectedDataframe)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Value")+ xlab("Year")+
        theme_bw()
    })
  })
  
  ## TotalCost
  output$TotalCost <- renderValueBox({
    datadata <- ts_cost_per_year()
    valueBox(
      paste0(round(sum(datadata$Median),2)), "Total Costs", icon = icon("money-bill-trend-up"),
      color = "green"
    )
  })
  
  ## TotalCostPEP
  output$TotalCostPEP <- renderValueBox({
    datadata <- ts_cost_PEP_per_year()
    valueBox(
      paste0(round(sum(datadata$Median),2)), "Total PEP Costs", icon = icon("money-bill-transfer"),
      color = "green"
    )
  })
  
  ## TotalCostMDV
  output$TotalCostMDV <- renderValueBox({
    datadata <- ts_MDV_campaign_cost()
    valueBox(
      paste0(round(sum(datadata$Median),2)), "Total Vaccination Campaign Costs", icon = icon("money-bill-transfer"),
      color = "green"
    )
  })
  
  ## TotalExposures
  output$TotalExposures <- renderValueBox({
    datadata <- ts_exposures()
    valueBox(
      paste0(ceiling(sum(datadata$Median))), "Total Rabies Exposures", icon = icon("viruses"),
      color = "blue"
    )
  })
  
  ## TotalExposuresSeek
  output$TotalExposuresSeek <- renderValueBox({
    datadata <- ts_exposures_seek_care()
    valueBox(
      paste0(round(sum(datadata$Median))), "Total Exposures Seeking Care", icon = icon("house-medical-circle-check"),
      color = "blue"
    )
  })
  
  ## TotalExposuresNoSeek
  output$TotalExposuresNoSeek <- renderValueBox({
    datadata <- ts_exposures_do_not_seek_care()
    valueBox(
      paste0(round(sum(datadata$Median))), "Total Exposures Not Seeking Care", icon = icon("house-medical-circle-xmark"),
      color = "blue"
    )
  })
  
  ## TotalExposuresStart
  output$TotalExposuresStart <- renderValueBox({
    datadata <- ts_exp_start()
    valueBox(
      paste0(round(sum(datadata$Median))), "Total Exposures Starting Care", icon = icon("heart-circle-plus"),
      color = "blue"
    )
  })
  
  ## TotalExposuresNoStart
  output$TotalExposuresNoStart <- renderValueBox({
    datadata <- ts_exp_no_start()
    valueBox(
      paste0(round(sum(datadata$Median))), "Total Exposures Not Starting Care", icon = icon("heart-circle-exclamation"),
      color = "blue"
    )
  })
  
  ## TotalExposuresComplete
  output$TotalExposuresComplete <- renderValueBox({
    datadata <- ts_exp_complete()
    valueBox(
      paste0(round(sum(datadata$Median))), "Total Exposures Completing Care", icon = icon("heart-circle-check"),
      color = "blue"
    )
  })
  
  ## TotalDeaths
  output$TotalDeaths <- renderValueBox({
    datadata <- ts_deaths()
    valueBox(
      paste0(round(sum(datadata$Median))), "Total Deaths", icon = icon("skull"),
      color = "red"
    )
  })
  
  ## TotalDeathsNoPEP
  output$TotalDeathsNoPEP <- renderValueBox({
    datadata <- ts_deaths_no_PEP()
    valueBox(
      paste0(round(sum(datadata$Median))), "Total Deaths Without PEP", icon = icon("skull"),
      color = "red"
    )
  })
  
  ## TotalDeathsAverted
  output$TotalDeathsAverted <- renderValueBox({
    datadata <- ts_deaths_averted_PEP()
    valueBox(
      paste0(round(sum(datadata$Median))), "Total Deaths Averted", icon = icon("heart-pulse"),
      color = "red"
    )
  })
  
  ## TotalRabidDogs
  output$TotalRabidDogs <- renderValueBox({
    datadata <- ts_rabid_dogs()
    valueBox(
      paste0(ceiling(sum(datadata$Median))), "Total Rabid Dogs", icon = icon("dog"),
      color = "teal"
    )
  })
  
  ## TotalRabidBitingDogs
  output$TotalRabidBitingDogs <- renderValueBox({
    datadata <- ts_rabid_biting_dogs()
    valueBox(
      paste0(ceiling(sum(datadata$Median))), "Total Rabid Biting Dogs", icon = icon("shield-dog"),
      color = "teal"
    )
  })
  
  ## TotalRabidBitingInvestigated
  output$TotalRabidBitingInvestigated <- renderValueBox({
    datadata <- ts_rabid_biting_investigated()
    valueBox(
      paste0(ceiling(sum(datadata$Median))), "Total Rabid Biting Dogs Investigated", icon = icon("magnifying-glass"),
      color = "teal"
    )
  })
  
  ## TotalRabidBitingFound
  output$TotalRabidBitingFound <- renderValueBox({
    datadata <- ts_rabid_biting_found()
    valueBox(
      paste0(ceiling(sum(datadata$Median))), "Total Rabid Biting Dogs Found", icon = icon("magnifying-glass-location"),
      color = "teal"
    )
  })
  
  ## TotalRabidBitingTestable
  output$TotalRabidBitingTestable <- renderValueBox({
    datadata <- ts_rabid_biting_testable()
    valueBox(
      paste0(ceiling(sum(datadata$Median))), "Total Rabid Biting Dogs Testable", icon = icon("microscope"),
      color = "teal"
    )
  })
  
}

# Running the App
shinyApp(ui, server)

