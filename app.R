library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(RColorBrewer)
library(knitr)
library(dplyr)
library(tidyverse)

source("Code/stochastic_decision_tree.R") # Decision Tree model & summary functions
source("Code/HelperFun.R") # Decision Tree model & summary functions

parameters <- read_csv("data/parameters.csv")

# https://debruine.github.io/shinyintro/reports.html
# https://stackoverflow.com/questions/57802225/how-to-pass-table-and-plot-in-shiny-app-as-parameters-to-r-markdown

# Define custom names for campaign types
custom_campaign_names <- c(
  "No Interventions" = "no_interventions",
  "Free Intramuscular PEP" = "PEP_IM_free_only",
  "Free Intradermal PEP" = "PEP_ID_free_only",
  "Mass Dog Vaccination Only" = "MDV_only",
  "MDV + Free Intramuscular PEP" = "MDV_PEP_IM_free",
  "MDV + Free Intradermal PEP" = "MDV_PEP_ID_free")

ui <- navbarPage("Program Planning",
                 tabPanel("IBCM Campaign",
                          dashboardBody(
                            fluidPage(
                              sidebarLayout(
                                sidebarPanel(
                                  h3("Rabies Control Method"),
                                  fluidRow(
                                    helpText("Select the desired rabies control method."),
                                    column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                           selectInput("campaign_type", "Select Campaign Type",
                                                       choices = c(
                                                         "No Interventions" = "no_interventions",
                                                         "Free Intramuscular PEP" = "PEP_IM_free_only",
                                                         "Free Intradermal PEP" = "PEP_ID_free_only",
                                                         "Mass Dog Vaccination Only" = "MDV_only",
                                                         "MDV + Free Intramuscular PEP" = "MDV_PEP_IM_free",
                                                         "MDV + Free Intradermal PEP" = "MDV_PEP_ID_free")))
                                  ),
                                  h3("Years"),
                                  fluidRow(
                                    helpText("Select the number of years you would like to examine."),
                                    column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                           numericInput("I_Years", "Years", value = 7, min = 2, step = 1))
                                  ),
                                  h3("Population Inputs"),
                                  fluidRow(
                                    helpText("Input the human population and the Human Dog Ratio.",
                                             "This is used to calculate the estimated dog population."),
                                    column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                           numericInput("I_Humans", "Human Population", value = 500000, min = 1, step = 1)),
                                    column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                           numericRangeInput("I_HDR", "Human Dog Ratio", value = c(8,10), min = 0.1, max = 100000, step = 0.1))
                                  ),
                                  h3("PEP Costs"),
                                  fluidRow(
                                    helpText("Input the Post-Exposure Prophylaxis costs.",
                                             "As not everyone ends up completing a full course of PEP the costs for both a complete and incomplete corse should be accounted for."),
                                    column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                                           numericInput("I_FullCost", "Complete PEP Cost", value = 45, min = 0, step = 0.01)),
                                    column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                                           numericInput("I_PartCost", "Incomplete PEP Cost", value = 25, min = 0, step = 0.01))
                                  ),
                                  h3("Vaccination Inputs"),
                                  fluidRow(
                                    conditionalPanel(
                                      condition = "input.campaign_type == 'MDV_only' | input.campaign_type == 'MDV_PEP_IM_free' | input.campaign_type == 'MDV_PEP_ID_free'",
                                      helpText("Select the vaccination coverage target for the campaign"),
                                      column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                             numericInput("VacCovTarget", "Vaccination Coverage Target", value = 50, min = 0, max = 100, step = 1))
                                    ),
                                    helpText("In most cases, even when a vaccination campaign is not being run, some people activly seek out rabies vaccinations for their dogs.",
                                             "This results in a background vaccination coverage (usually around 5%).",
                                             "If you are running (or would like to run) a mass dog vaccination campaign in tandem with an IBCM program, then select the appropriate campaign at the top."),
                                    column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                           numericInput("I_VacCovBase", "Vaccination Coverage Baseline Percentage", value = 5, min = 0, max = 100, step = 1))
                                  )
                                  
                                ),
                                mainPanel(
                                  fluidPage(
                                    tags$h1("IBCM Campaign"),
                                    p(style="text-align: justify; font-size = 35px",
                                      "Intro Paragraph thing"),
                                    tags$h2("Dogs"),
                                    textOutput("IBCMDogsText"),
                                    plotOutput("IBCMDogsGraph"),
                                    tags$h2("Patients"),
                                    p(style="text-align: justify; font-size = 35px",
                                      "Intro Paragraph thing"),
                                    tags$h3("Rabies Exposures"),
                                    textOutput("IBCMExposuresText"),
                                    plotOutput("IBCMExpGraph"),
                                    tags$h3("Rabies Deaths"),
                                    textOutput("IBCMDeathsText"),
                                    plotOutput("IBCMDeathsGraph"),
                                    tags$h3("Healthy Bites"),
                                    textOutput("IBCMHBText"),
                                    plotOutput("IBCMHBGraph"),
                                    tags$h2("Costs"),
                                    textOutput("IBCMCostText"),
                                    plotOutput("IBCMCostGraph")
                                  )
                                  
                                  
                                )
                                
                              )
                            )
                          )
                 )
)


server <- function(input,output) {
  
  ###########################
  ####   IBCM Campaign   ####
  ###########################
  
  observe({
    
    InputTable <- reactive({
      SelectedInput <- input$campaign_type
      InputTable <- parameters[which(parameters$scenario==SelectedInput),]
      InputTable
    })
    
    pSeekH <- reactive({
      Params <- InputTable()
      pSeekH <- Params$pSeek_healthy
      pSeekH
    })
    
    pCompH <- reactive({
      Params <- InputTable()
      pCompH <- Params$pComplete_healthy
      pCompH
    })
    
    pStartExp <- reactive({
      Params <- InputTable()
      pStartExp <- Params$pStart_exposure
      pStartExp
    })
    
    pCompExp <- reactive({
      Params <- InputTable()
      pCompExp <- Params$pComplete_exposure
      pCompExp
    })
    
    FCost <- reactive({
      Params <- InputTable()
      FCost <- Params$full_cost
      FCost
    })
    
    PCost <- reactive({
      Params <- InputTable()
      PCost <- Params$partial_cost
      PCost
    })
    
    VCost1 <- reactive({
      Params <- InputTable()
      VCost1 <- Params$vaccinate_dog_cost1
      VCost1
    })
    
    VCost2 <- reactive({
      Params <- InputTable()
      VCost2 <- Params$vaccinate_dog_cost2
      VCost2
    })
    
    TargCov <- reactive({
      if(input$campaign_type == "no_interventions" | input$campaign_type == "PEP_IM_free_only" | input$campaign_type == "PEP_ID_free_only"){
        TargCov <- input$I_VacCovBase
      } else {
        TargCov <- input$VacCovTarget
      }
      TargCov
    })
    
    IBCMResults <- reactive({
      
      I_pSeekH <- pSeekH()
      I_pCompH <- pCompH()
      I_pStartExp <- pStartExp()
      I_pCompExp <- pCompExp()
      I_FCost <- FCost()
      I_PCost <- PCost()
      I_VCost1 <- VCost1()
      I_VCost2 <- VCost2()
      I_TargCov <- TargCov()
      
      
      IBCMResults <- decision_tree(N = 100, 
                                   pop = input$I_Humans, 
                                   HDR = c(input$I_HDR[1], input$I_HDR[2]), 
                                   horizon = input$I_Years, 
                                   discount = 0,
                                   mu = 0.38, 
                                   k = 0.14, 
                                   pSeek_healthy=I_pSeekH,
                                   pBite_healthy=0.01,
                                   pStart_healthy= 0.2,
                                   pComplete_healthy = I_pCompH,
                                   pSeek_exposure=0.7,
                                   pStart_exposure = I_pStartExp,
                                   pComplete_exposure = I_pCompExp,
                                   pDeath = 0.1660119,
                                   pPrevent = 0.986,
                                   full_cost = I_FCost, 
                                   partial_cost = I_PCost, 
                                   base_vax_cov = (input$I_VacCovBase / 100),
                                   vaccinate_dog_cost = c(I_VCost1, I_VCost2), 
                                   target_vax_cov = (I_TargCov / 100), 
                                   pInvestigate = 0.9, 
                                   pFound = 0.6, 
                                   pTestable = 0.7, 
                                   pFN = 0.05)
      
      IBCMResults
      
    })
    
    ###  IBCM DOGS  ###
    
    IBCM_Rabid_Dogs <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_rabid_dogs', scenario=IR)
      IR2
    })
    
    IBCM_Rabid_Bite_Dogs <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_rabid_biting_dogs', scenario=IR)
      IR2
    })
    
    IBCM_Rabid_Bite_Dogs_Investigated <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_rabid_biting_investigated', scenario=IR)
      IR2
    })
    
    IBCM_Rabid_Bite_Dogs_Found <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_rabid_biting_found', scenario=IR)
      IR2
    })
    
    IBCM_Rabid_Bite_Dogs_Testable <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_rabid_biting_testable', scenario=IR)
      IR2
    })
    
    output$IBCMDogsText <- renderText({
      IRD <- IBCM_Rabid_Dogs()
      IRDB <- IBCM_Rabid_Bite_Dogs()
      IRDI <- IBCM_Rabid_Bite_Dogs_Investigated()
      IRDF <- IBCM_Rabid_Bite_Dogs_Found()
      IRDT <- IBCM_Rabid_Bite_Dogs_Testable()
      paste("Over the", 
            input$I_Years,
            "years, there will be approximatly",
            ceiling(sum(IRD$Median)),
            "rabid dogs.  Of these dogs, approximatly",
            ceiling(sum(IRDB$Median)),
            "will go on to bite someone.  Of these biting dogs, an estimated",
            ceiling(sum(IRDI$Median)),
            "will be investigated by a member of the IBCM team.  Not all investigated dogs will be found, with an estimated",
            ceiling(sum(IRDF$Median)),
            "being found and of these",
            ceiling(sum(IRDT$Median)),
            "being testable."
      )
    })
    
    IBCM_Dog_Graph_Data <- reactive({
      IRD <- IBCM_Rabid_Dogs()
      IRDB <- IBCM_Rabid_Bite_Dogs()
      IRDI <- IBCM_Rabid_Bite_Dogs_Investigated()
      IRDF <- IBCM_Rabid_Bite_Dogs_Found()
      IRDT <- IBCM_Rabid_Bite_Dogs_Testable()
      DataName <- c("Total", "Biting Dogs","Investigated", "Found", "Tested")
      RabidDogs <- c(sum(IRD$Median),sum(IRDB$Median),sum(IRDI$Median),sum(IRDF$Median),sum(IRDT$Median))
      IBCM_Dog_Graph_Data <- data.frame(DataName,RabidDogs)
    })
    
    observe({
      ID <- IBCM_Dog_Graph_Data()
      ID$DataName <- factor(ID$DataName, levels = ID$DataName)
      output$IBCMDogsGraph <- renderPlot({ggplot(ID, aes(x = DataName, y = RabidDogs, fill = DataName)) +
          geom_bar(stat = "identity") +
          ylab("Number of Dogs")+xlab(NULL)+
          theme_bw()})
    })
    
    
    ###  IBCM PATIENTS  ###  
    
    IBCM_Exposures <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exposures', scenario=IR)
      IR2
    })
    
    IBCM_Exposures_Seek <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exposures_seek_care', scenario=IR)
      IR2
    })
    
    IBCM_Exposures_Start <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exp_start', scenario=IR)
      IR2
    })
    
    IBCM_Exposures_Complete <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exp_complete', scenario=IR)
      IR2
    })
    
    IBCM_Exposures_Incomplete <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exp_incomplete', scenario=IR)
      IR2
    })
    
    output$IBCMExposuresText <- renderText({
      IE <- IBCM_Exposures()
      IESE <- IBCM_Exposures_Seek()
      IEST <- IBCM_Exposures_Start()
      IEC <- IBCM_Exposures_Complete()
      IEIC <- IBCM_Exposures_Incomplete()
      paste("Over the", 
            input$I_Years,
            "years, there will be approximatly",
            ceiling(sum(IE$Median)),
            "people exposed to a rabid dog in some way such as a bite.  Of these people, an estimated",
            ceiling(sum(IESE$Median)),
            "will seek care.  Of these people, an estimated",
            ceiling(sum(IEST$Median)),
            "will start care in the form of post-exposure prophylaxis (PEP).  Of those starting PEP, approximatly",
            ceiling(sum(IEC$Median)),
            "will complete the full recomended course, with the remaining",
            ceiling(sum(IEIC$Median)),
            "patients starting but not completing it."
      )
    })
    
    IBCM_Exposures_Graph_Data <- reactive({
      IE <- IBCM_Exposures()
      IESE <- IBCM_Exposures_Seek()
      IEST <- IBCM_Exposures_Start()
      IEC <- IBCM_Exposures_Complete()
      IEIC <- IBCM_Exposures_Incomplete()
      DataName <- c("People Biten", "Seek Care","Start Care", "Complete Care", "Incomplete Care")
      Exposures <- c(sum(IE$Median),sum(IESE$Median),sum(IEST$Median),sum(IEC$Median),sum(IEIC$Median))
      IBCM_Exposures_Graph_Data <- data.frame(DataName,Exposures)
    })
    
    observe({
      ID <- IBCM_Exposures_Graph_Data()
      ID$DataName <- factor(ID$DataName, levels = ID$DataName)
      output$IBCMExpGraph <- renderPlot({ggplot(ID, aes(x = DataName, y = Exposures, fill = DataName)) +
          geom_bar(stat = "identity") +
          ylab("Number of Patients")+xlab(NULL)+
          theme_bw()})
    })
    
    
    ###  IBCM DEATHS  ###  
    
    IBCM_Deaths <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_deaths', scenario=IR)
      IR2
    })
    
    IBCM_Deaths_No_PEP <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_deaths_no_PEP', scenario=IR)
      IR2
    })
    
    IBCM_Deaths_Averted <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_deaths_averted_PEP', scenario=IR)
      IR2
    })
    
    output$IBCMDeathsText <- renderText({
      ID <- IBCM_Deaths()
      IDP <- IBCM_Deaths_No_PEP()
      IDA <- IBCM_Deaths_Averted()
      paste("Over the", 
            input$I_Years,
            "years, there will be approximatly",
            ceiling(sum(ID$Median)),
            "human deaths due to rabies exposure.  Of these deaths,",
            ceiling(sum(IDP$Median)),
            "will be due to a the patient recieving no PEP, with any remaining deaths being due to incomplete or inefective PEP.  However, an estimated",
            ceiling(sum(IDA$Median)),
            "lives will be saved by access to PEP."
      )
    })
    
    IBCM_Deaths_Graph_Data <- reactive({
      ID <- IBCM_Deaths()
      IDP <- IBCM_Deaths_No_PEP()
      IDA <- IBCM_Deaths_Averted()
      DataName <- c("Total", "from no PEP","Averted")
      Deaths <- c(sum(ID$Median),sum(IDP$Median),sum(IDA$Median))
      IBCM_Exposures_Graph_Data <- data.frame(DataName,Deaths)
    })
    
    observe({
      ID <- IBCM_Deaths_Graph_Data()
      ID$DataName <- factor(ID$DataName, levels = ID$DataName)
      output$IBCMDeathsGraph <- renderPlot({ggplot(ID, aes(x = DataName, y = Deaths, fill = DataName)) +
          geom_bar(stat = "identity") +
          ylab("Number of Deaths")+xlab(NULL)+
          theme_bw()})
    })
    
    
    ###  IBCM HEALTHY BITES  ###  
    
    IBCM_Healthy_Seek <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exposures_seek_care', scenario=IR)
      IR2
    })
    
    IBCM_Healthy_Complete <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exp_complete', scenario=IR)
      IR2
    })
    
    IBCM_Healthy_Incomplete <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exp_incomplete', scenario=IR)
      IR2
    })
    
    output$IBCMHBText <- renderText({
      IHS <- IBCM_Healthy_Seek()
      IHC <- IBCM_Healthy_Complete()
      IHIC <- IBCM_Healthy_Incomplete()
      IEST <- IBCM_Exposures_Start()
      IEC <- IBCM_Exposures_Complete()
      paste("Over the", 
            input$I_Years,
            "years, there will be approximatly",
            ceiling(sum(IHS$Median)),
            "patients seeking rabies treatment who have not been exposed to a rabid dog.  Of these patients, an estimated",
            ceiling(sum(IHC$Median) + sum(IHIC$Median)),
            "will start a course of PEP, with approximatly",
            ceiling(sum(IHC$Median)),
            "of these patients completing the full course.  This results in a total of",
            ceiling(sum(IHC$Median) + sum(IHIC$Median) + sum(IEST$Median)),
            "patients (exposed or not) starting PEP, of which an estimated",
            ceiling(sum(IHC$Median) + sum(IEC$Median)),
            "will complete the full course of PEP."
      )
    })
    
    IBCM_Healthy_Graph_Data <- reactive({
      IHS <- IBCM_Healthy_Seek()
      IHC <- IBCM_Healthy_Complete()
      IHIC <- IBCM_Healthy_Incomplete()
      IEST <- IBCM_Exposures_Start()
      IEC <- IBCM_Exposures_Complete()
      DataName <- c("Healthy Seek Care","Healthy Start Care", "Healthy Complete Care", "Total Starting PEP")
      Healthy <- c(sum(IHS$Median),(sum(IHC$Median) + sum(IHIC$Median)),sum(IHC$Median),(sum(IHC$Median) + sum(IHIC$Median) + sum(IEST$Median)))
      IBCM_Exposures_Graph_Data <- data.frame(DataName,Healthy)
    })
    
    observe({
      ID <- IBCM_Healthy_Graph_Data()
      ID$DataName <- factor(ID$DataName, levels = ID$DataName)
      output$IBCMHBGraph <- renderPlot({ggplot(ID, aes(x = DataName, y = Healthy, fill = DataName)) +
          geom_bar(stat = "identity") +
          ylab("Patients")+xlab(NULL) +
          theme_bw()})
    })
    
    
    
    ###  IBCM Costs  ###  
    
    IBCM_Total_Cost <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_cost_per_year', scenario=IR)
      IR2
    })
    
    IBCM_PEP_Cost <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_cost_PEP_per_year', scenario=IR)
      IR2
    })
    
    IBCM_Vac_Cost <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_MDV_campaign_cost', scenario=IR)
      IR2
    })
    
    output$IBCMCostText <- renderText({
      ITC <- IBCM_Total_Cost()
      IPC <- IBCM_PEP_Cost()
      IVC <- IBCM_Vac_Cost()
      paste("Over the", 
            input$I_Years,
            "years, the program will cost an estimated total of",
            round(sum(ITC$Median),2),
            "pounds, with an estimated yearly cost of",
            round((sum(ITC$Median) / input$I_Years),2),
            "pounds.  Splitting this up, this equates to a total of",
            round(sum(IPC$Median),2),
            "pounds (",
            round((sum(IPC$Median) / input$I_Years),2),
            "pounds per year ) on PEP, and",
            round(sum(IVC$Median),2),
            "pounds (",
            round((sum(IVC$Median) / input$I_Years),2),
            "pounds per year ) on vaccinations."
      )
    })
    
    IBCM_Cost_Graph_Data <- reactive({
      ITC <- IBCM_Total_Cost()
      IPC <- IBCM_PEP_Cost()
      IVC <- IBCM_Vac_Cost()
      DataName <- c("Total Cost","PEP Cost", "Dog Vaccination Cost")
      Cost <- c(round(sum(ITC$Median),2), round(sum(IPC$Median),2), round(sum(IVC$Median),2))
      IBCM_Exposures_Graph_Data <- data.frame(DataName,Cost)
    })
    
    observe({
      ID <- IBCM_Cost_Graph_Data()
      ID$DataName <- factor(ID$DataName, levels = ID$DataName)
      output$IBCMCostGraph <- renderPlot({ggplot(ID, aes(x = DataName, y = Cost, fill = DataName)) +
          geom_bar(stat = "identity") +
          ylab("Patients")+xlab(NULL) +
          theme_bw()})
    })
    
    
  })
  
}

# Running the App
shinyApp(ui, server)
