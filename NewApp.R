library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(RColorBrewer)
library(knitr)
library(dplyr)
library(tidyverse)

source("Code/stochastic_decision_tree.R") # Decision Tree model & summary functions

# https://debruine.github.io/shinyintro/reports.html
# https://stackoverflow.com/questions/57802225/how-to-pass-table-and-plot-in-shiny-app-as-parameters-to-r-markdown

ui <- navbarPage("Program Planning",
                 tabPanel("Full Programs",
                          dashboardBody(
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h3("Years"),
                                fluidRow(
                                  helpText("Select the number of years you would like to examine."),
                                  column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                         numericInput("Years", "Years", value = 7, min = 1, step = 1))
                                ),
                                h3("Population Inputs"),
                                fluidRow(
                                  helpText("Input the human population and the Human Dog Ratio.",
                                           "This is used to calculate the estimated dog population."),
                                  column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                         numericInput("Humans", "Human Population", value = 500000, min = 1, step = 1)),
                                  column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                         numericRangeInput("HDR", "Human Dog Ratio", value = c(8,10), min = 0.1, max = 100000, step = 0.1))
                                ),
                                h3("PEP Costs"),
                                fluidRow(
                                  helpText("Input the Post-Exposure Prophylaxis costs.",
                                           "As not everyone ends up completing a full course of PEP the costs for both a complete and incomplete corse should be accounted for."),
                                  column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                                         numericInput("FullCost", "Complete PEP Cost", value = 45, min = 0, step = 0.01)),
                                  column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                                         numericInput("PartCost", "Incomplete PEP Cost", value = 25, min = 0, step = 0.01))
                                ),
                                h3("Vaccination Inputs"),
                                fluidRow(
                                  helpText("The vaccination campaign will determine the number of rabid dogs.",
                                           "As such, the number of dogs being vaccinated and the accosiated costs must be calculated.",
                                           "The vaccination baseline represents the coverage before the vaccination campaign starts.",
                                           "If you do not wish to include a vaccination campaign in cost analysis then consider using the 'IBCM' tab at the top."),
                                  column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                         numericRangeInput("VacDogCost", "Cost Per Vaccination", value = c(2,4), min = 0, step = 0.01)),
                                  column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                         numericInput("VacCovBase", "Vaccination Coverage Baseline", value = 5.0, min = 0, max = 100, step = 0.1)),
                                  column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                         numericInput("VacCovTarget", "Vaccination Coverage Target", value = 20.0, min = 0, max = 100, step = 0.1))
                                )

                              ),
                              mainPanel(
                                fluidPage(
                                  tags$h1("IBCM with Vaccination Campaign"),
                                  p(style="text-align: justify; font-size = 35px",
                                    "Intro Paragraph thing"),
                                  tags$h2("Dogs"),
                                  textOutput("CombinedDogsText"),
                                  tabsetPanel(
                                    id = "CombinedDogsGraphs",
                                    tabPanel("Rabid Dogs", plotOutput("CombinedRabidDogsGraph")),
                                    tabPanel("Rabid Biting Dogs", plotOutput("CombinedRabidBitingDogsGraph")),
                                    tabPanel("Rabid Biting Dogs Investigated", plotOutput("CombinedRabidBitingDogsInvestigatedGraph")),
                                    tabPanel("Rabid Biting Dogs Found", plotOutput("CombinedRabidBitingDogsFoundGraph")),
                                    tabPanel("Rabid Biting Dogs Testable", plotOutput("CombinedRabidBitingDogsTestableGraph")),
                                    tabPanel("Vaccinated Dogs", plotOutput("CombinedVaccinatedDogsGraph"))),
                                  tags$h2("Patients"),
                                  p(style="text-align: justify; font-size = 35px",
                                    "Intro Paragraph thing"),
                                  tags$h3("Rabies Exposures"),
                                  textOutput("CombinedExposuresText"),
                                  tabsetPanel(
                                    id = "CombinedExposuresGraphs",
                                    tabPanel("Rabies Exposures", plotOutput("CombinedExpGraph")),
                                    tabPanel("Exposures Seeking Care", plotOutput("CombinedExpSeekGraph")),
                                    tabPanel("Exposures Starting PEP", plotOutput("CombinedExpStartGraph")),
                                    tabPanel("Exposures Completing PEP", plotOutput("CombinedExpCompGraph")),
                                    tabPanel("Exposures Starting But Not Completing PEP", plotOutput("CombinedExpNoCompGraph"))),
                                  tags$h3("Rabies Deaths"),
                                  textOutput("CombinedDeathsText"),
                                  tabsetPanel(
                                    id = "CombinedDeathsGraphs",
                                    tabPanel("Total Rabies Deaths", plotOutput("CombinedDeathsGraph")),
                                    tabPanel("Rabies Deaths From No PEP", plotOutput("CombinedDeathsNoPEPGraph")),
                                    tabPanel("Deaths Averted", plotOutput("CombinedDeathsAvGraph"))),
                                  tags$h3("Healthy Bites"),
                                  textOutput("CombinedHBText"),
                                  tabsetPanel(
                                    id = "CombinedHBGraphs",
                                    tabPanel("Total Healthy Patients Seeking Care", plotOutput("CombinedHBGraph")),
                                    tabPanel("Healthy Patients Completing PEP", plotOutput("CombinedHBCPGraph")),
                                    tabPanel("Healthy Patients Starting But Not Completing PEP", plotOutput("CombinedHBNoCPGraph"))),
                                  tags$h2("Costs"),
                                  textOutput("CombinedCostText"),
                                  tabsetPanel(
                                    id = "CombinedCostGraphs",
                                    tabPanel("Total Costs", plotOutput("CombinedCostGraph")),
                                    tabPanel("Total PEP Costs", plotOutput("CombinedPEPCostGraph")),
                                    tabPanel("Total Vaccination Costs", plotOutput("CombinedVacCostGraph")))
                                  )
                                    
                                    
                                  )
                                  
                                )
                              )
                            )
                          )
)


server <- function(input,output) {
  
  ######################
  ####   COMBINED   ####
  ######################
  
  CombinedResults <- reactive({
    
    CombinedResults <- decision_tree(N = 100, 
                             pop = input$Humans, 
                             HDR = c(input$HDR[1], input$HDR[2]), 
                             horizon = input$Years, 
                             discount = 0,
                             pBite_healthy=0.1,
                             mu = 0.7054917, 
                             k = 0.3862238, 
                             pSeek_healthy=0.6,
                             pStart_healthy= 0.6666667,
                             pComplete_healthy = 0.3968254,
                             pSeek_exposure=0.7,
                             pStart_exposure = 0.6666667,
                             pComplete_exposure = 0.3968254,
                             pDeath = 0.1660119,
                             pPrevent = 0.986,
                             full_cost = input$FullCost, 
                             partial_cost = input$PartCost, 
                             base_vax_cov = (input$VacCovBase / 100),
                             vaccinate_dog_cost = c(input$VacDogCost[1], input$VacDogCost[2]), 
                             target_vax_cov = (input$VacCovTarget / 100), 
                             pInvestigate = 0.9, 
                             pFound = 0.6, 
                             pTestable = 0.7, 
                             pFN = 0.05)
    
    CombinedResults
    
  })
  
  ###  COMBINED DOGS  ###
  
  Combined_Rabid_Dogs <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_rabid_dogs', scenario=CR)
    CR2
  })
 
  Combined_Rabid_Bite_Dogs <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_rabid_biting_dogs', scenario=CR)
    CR2
  })
  
  Combined_Rabid_Bite_Dogs_Investigated <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_rabid_biting_investigated', scenario=CR)
    CR2
  })
  
  Combined_Rabid_Bite_Dogs_Found <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_rabid_biting_found', scenario=CR)
    CR2
  })
  
  Combined_Rabid_Bite_Dogs_Testable <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_rabid_biting_testable', scenario=CR)
    CR2
  })
  
  Combined_Vaccinated_Dogs <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_dogs_vaccinated', scenario=CR)
    CR2
  })
  
  output$CombinedDogsText <- renderText({
    CRD <- Combined_Rabid_Dogs()
    CRDB <- Combined_Rabid_Bite_Dogs()
    CRDI <- Combined_Rabid_Bite_Dogs_Investigated()
    CRDF <- Combined_Rabid_Bite_Dogs_Found()
    CRDT <- Combined_Rabid_Bite_Dogs_Testable()
    CVD <- Combined_Vaccinated_Dogs()
    paste("Over the", 
          input$Years,
          "years, there will be approximatly",
          ceiling(sum(CRD$Median)),
          "rabid dogs.  Of these dogs, approximatly",
          ceiling(sum(CRDB$Median)),
          "will go on to bite someone.  Of these biting dogs, an estimated",
          ceiling(sum(CRDI$Median)),
          "will be investigated by a member of the IBCM team.  Not all investigated dogs will be found, with an estimated",
          ceiling(sum(CRDF$Median)),
          "being found and of these",
          ceiling(sum(CRDT$Median)),
          "being testable.  Additionally, over the",
          input$Years,
          "years, there will be approximatly",
          ceiling(sum(CVD$Median)),
          "dogs vaccinated in the vaccination campaign."
          )
  })
  
  
  observe({
    CR <- Combined_Rabid_Dogs()
    output$CombinedRabidDogsGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Dogs")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Rabid_Bite_Dogs()
    output$CombinedRabidBitingDogsGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Dogs")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Rabid_Bite_Dogs_Investigated()
    output$CombinedRabidBitingDogsInvestigatedGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Dogs")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Rabid_Bite_Dogs_Found()
    output$CombinedRabidBitingDogsFoundGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Dogs")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Rabid_Bite_Dogs_Testable()
    output$CombinedRabidBitingDogsTestableGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Dogs")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Vaccinated_Dogs()
    output$CombinedVaccinatedDogsGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Dogs Vaccinated")+ xlab("Year")+
        theme_bw()})
  })

  
  ###  COMBINED PATIENTS  ###  

  Combined_Exposures <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_exposures', scenario=CR)
    CR2
  })
  
  Combined_Exposures_Seek <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_exposures_seek_care', scenario=CR)
    CR2
  })
  
  Combined_Exposures_Start <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_exp_start', scenario=CR)
    CR2
  })
  
  Combined_Exposures_Complete <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_exp_complete', scenario=CR)
    CR2
  })
  
  Combined_Exposures_Incomplete <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_exp_incomplete', scenario=CR)
    CR2
  })
  
  output$CombinedExposuresText <- renderText({
    CE <- Combined_Exposures()
    CESE <- Combined_Exposures_Seek()
    CEST <- Combined_Exposures_Start()
    CEC <- Combined_Exposures_Complete()
    CEIC <- Combined_Exposures_Incomplete()
    paste("Over the", 
          input$Years,
          "years, there will be approximatly",
          ceiling(sum(CE$Median)),
          "people exposed to a rabid dog in some way such as a bite.  Of these people, an estimated",
          ceiling(sum(CESE$Median)),
          "will seek care.  Of these people, an estimated",
          ceiling(sum(CEST$Median)),
          "will start care in the form of post-exposure prophylaxis (PEP).  Of those starting PEP, approximatly",
          ceiling(sum(CEC$Median)),
          "will complete the full recomended course, with the remaining",
          ceiling(sum(CEIC$Median)),
          "patients starting but not completing it."
    )
  })
  
  observe({
    CR <- Combined_Exposures()
    output$CombinedExpGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Patients")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Exposures_Seek()
    output$CombinedExpSeekGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Patients")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Exposures_Start()
    output$CombinedExpStartGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Patients")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Exposures_Complete()
    output$CombinedExpCompGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Patients")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Exposures_Incomplete()
    output$CombinedExpNoCompGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Patients")+ xlab("Year")+
        theme_bw()})
  })
  
  
  ###  COMBINED DEATHS  ###  
  
  Combined_Deaths <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_deaths', scenario=CR)
    CR2
  })
  
  Combined_Deaths_No_PEP <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_deaths_no_PEP', scenario=CR)
    CR2
  })
  
  Combined_Deaths_Averted <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_deaths_averted_PEP', scenario=CR)
    CR2
  })
  
  output$CombinedDeathsText <- renderText({
    CD <- Combined_Deaths()
    CDP <- Combined_Deaths_No_PEP()
    CDA <- Combined_Deaths_Averted()
    paste("Over the", 
          input$Years,
          "years, there will be approximatly",
          ceiling(sum(CD$Median)),
          "human deaths due to rabies exposure.  Of these deaths,",
          ceiling(sum(CDP$Median)),
          "will be due to a the patient recieving no PEP, with any remaining deaths being due to incomplete or inefective PEP.  However, an estimated",
          ceiling(sum(CDA$Median)),
          "lives will be saved by access to PEP."
    )
  })
  
  observe({
    CR <- Combined_Deaths()
    output$CombinedDeathsGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Deaths")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Deaths_No_PEP()
    output$CombinedDeathsNoPEPGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Deaths")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Deaths_Averted()
    output$CombinedDeathsAvGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Deaths Averted")+ xlab("Year")+
        theme_bw()})
  })
  
  
  ###  COMBINED HEALTHY BITES  ###  
  
  Combined_Healthy_Seek <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_exposures_seek_care', scenario=CR)
    CR2
  })
  
  Combined_Healthy_Complete <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_exp_complete', scenario=CR)
    CR2
  })
  
  Combined_Healthy_Incomplete <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_exp_incomplete', scenario=CR)
    CR2
  })
  
  output$CombinedHBText <- renderText({
    CHS <- Combined_Healthy_Seek()
    CHC <- Combined_Healthy_Complete()
    CHIC <- Combined_Healthy_Incomplete()
    CEST <- Combined_Exposures_Start()
    CEC <- Combined_Exposures_Complete()
    paste("Over the", 
          input$Years,
          "years, there will be approximatly",
          ceiling(sum(CHS$Median)),
          "patients seeking rabies treatment who have not been exposed to a rabid dog.  Of these patients, an estimated",
          ceiling(sum(CHC$Median) + sum(CHIC$Median)),
          "will start a course of PEP, with approximatly",
          ceiling(sum(CHC$Median)),
          "of these patients completing the full course.  This results in a total of",
          ceiling(sum(CHC$Median) + sum(CHIC$Median) + sum(CEST$Median)),
          "patients (exposed or not) starting PEP, of which an estimated",
          ceiling(sum(CHC$Median) + sum(CEC$Median)),
          "will complete the full course of PEP."
    )
  })
  
  observe({
    CR <- Combined_Healthy_Seek()
    output$CombinedHBGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Healthy Patients")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Healthy_Complete()
    output$CombinedHBCPGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Healthy Patients")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Healthy_Incomplete()
    output$CombinedHBNoCPGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Healthy Patients")+ xlab("Year")+
        theme_bw()})
  })
  
  ###  COMBINED COSTS  ###  
  
  Combined_Total_Costs <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_cost_per_year', scenario=CR)
    CR2
  })
  
  Combined_Total_PEP_Costs <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_cost_PEP_per_year', scenario=CR)
    CR2
  })
  
  Combined_Total_MDV_Costs <- reactive({
    CR <- CombinedResults()
    CR2 <- select_variable(variable='ts_MDV_campaign_cost', scenario=CR)
    CR2
  })
  
  output$CombinedCostText <- renderText({
    CTC <- Combined_Total_Costs()
    CTPC <- Combined_Total_PEP_Costs()
    CTVC <- Combined_Total_MDV_Costs()
    CDA <- Combined_Deaths_Averted()
    paste("Over the", 
          input$Years,
          "years, the combined IBCM and mass dog vaccination campaign costs will total approximatly",
          round(sum(CTC$Median), 2),
          ".  This can be split as",
          round(sum(CTPC$Median), 2),
          "for the PEP costs and",
          round(sum(CTVC$Median), 2),
          "for the vaccination campaign.  This works out as approximatly",
          round((sum(CTVC$Median) / sum(CDA$Median)), 2),
          "per death averted."
    )
  })
  
  observe({
    CR <- Combined_Total_Costs()
    output$CombinedCostGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Costs")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Total_PEP_Costs()
    output$CombinedPEPCostGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Costs")+ xlab("Year")+
        theme_bw()})
  })
  
  observe({
    CR <- Combined_Total_MDV_Costs()
    output$CombinedVacCostGraph <- renderPlot({ggplot(CR, aes(x = as.numeric(row.names(CR)), y = Median)) +
        geom_line() +
        geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
        ylab("Costs")+ xlab("Year")+
        theme_bw()})
  })
  
  
}

# Running the App
shinyApp(ui, server)

