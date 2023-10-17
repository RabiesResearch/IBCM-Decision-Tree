library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readr)
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
                                                         "MDV + Free Intradermal PEP" = "MDV_PEP_ID_free"))),
                                    checkboxInput("show_healthy_dogs", "Show Healthy Dogs and Patients", value = TRUE)
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
                                           numericInput("I_Humans", "Human Population", value = 350000, min = 1, step = 1)),
                                    column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                           numericRangeInput("I_HDR", "Human Dog Ratio", value = c(25,35), min = 0.1, max = 100000, step = 0.1))
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
                                  h3("Dog Vaccination Inputs"),
                                  fluidRow(
                                    conditionalPanel(
                                      condition = "input.campaign_type == 'MDV_only' | input.campaign_type == 'MDV_PEP_IM_free' | input.campaign_type == 'MDV_PEP_ID_free'",
                                      helpText("Select the vaccination coverage target for the campaign"),
                                      column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                             numericInput("VacCovTarget", "Vaccination Coverage Target", value = 70, min = 0, max = 100, step = 1))
                                    ),
                                    helpText("In most cases, even when a vaccination campaign is not being run, some people activly seek out rabies vaccinations for their dogs.",
                                             "This results in a background vaccination coverage (usually around 5%).",
                                             "If you are running (or would like to run) a mass dog vaccination campaign in tandem with an IBCM program, then select the appropriate campaign at the top."),
                                    column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                           numericInput("I_VacCovBase", "Vaccination Coverage Baseline Percentage", value = 5, min = 0, max = 100, step = 1))
                                  ),
                                  h3("Time Graph"),
                                  fluidRow(
                                    helpText("The model used in this app allows for changes over time.",
                                             "Select what you would like to see change over time in the graph at the bottom of the page."),
                                    column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                                           selectInput("GraphicalOutput", "Graphical Output", 
                                                       c("Rabid Dogs" = "ts_rabid_dogs",
                                                         "Exposures" = "ts_exposures",
                                                         "Exposures Seeking Care" = "ts_exposures_seek_care",
                                                         "Exposures Starting Care" = "ts_exp_start",
                                                         "Exposures Completing Care" = "ts_exp_complete",
                                                         "Healthy Dog Bites" = "ts_healthy_bites",
                                                         "Healthy Patients Seeking Care" = "ts_healthy_seek_care",
                                                         "Healthy Patients Starting Care" = "ts_healthy_start_care",
                                                         "Healthy Patients Completing Care" = "ts_healthy_complete_care",
                                                         "Deaths" = "ts_deaths",
                                                         "Total Costs" = "ts_cost_per_year",
                                                         "PEP Costs" = "ts_cost_PEP_per_year",
                                                         "Dog Vaccination Costs" = "ts_MDV_campaign_cost")
                                           ))
                                  )
                                  
                                ),
                                mainPanel(
                                  fluidPage(
                                    tags$h1("IBCM Campaign"),
                                    p(style="text-align: justify; font-size = 35px",
                                      "This page is designed to help plan and compare different methods of rabies control over several years."),
                                    tags$h2("Dogs"),
                                    textOutput("IBCMDogsText"),
                                    plotOutput("IBCMDogsGraph"),
                                    tags$h2("Bite Patients"),
                                    tags$h3("Rabies Exposures"),
                                    textOutput("IBCMExposuresText"),
                                    plotOutput("IBCMExpGraph"),
                                    tags$h3("Rabies Deaths"),
                                    textOutput("IBCMDeathsText"),
                                    plotOutput("IBCMDeathsGraph"),
                                    tags$h2("Costs"),
                                    textOutput("IBCMCostText"),
                                    plotOutput("IBCMCostGraph"),
                                    tags$h3("Time Graph"),
                                    textOutput("IBCMTimeText"),
                                    plotOutput("IBCMTimeGraph")
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
    
    # Selecting row on parameters table
    InputTable <- reactive({
      SelectedInput <- input$campaign_type
      InputTable <- parameters[which(parameters$scenario==SelectedInput),]
      InputTable
    })
    
    # Selecting equivalent non-vaccination row on parameters table for calculating deaths averted
    DA_InputTable <- reactive({
      SelectedInput <- input$campaign_type
      if(SelectedInput=="MDV_only"){
        DA_InputTable <- parameters[which(parameters$scenario=="no_interventions"),]
      } else if(SelectedInput=="MDV_PEP_IM_free"){
        DA_InputTable <- parameters[which(parameters$scenario=="PEP_IM_free_only"),]
      } else if(SelectedInput=="MDV_PEP_ID_free"){
        DA_InputTable <- parameters[which(parameters$scenario=="PEP_ID_free_only"),]
      } else {
        DA_InputTable <- parameters[which(parameters$scenario==SelectedInput),]
      }
      DA_InputTable
    })
    
    # Target Coverage
    TargCov <- reactive({
      if(input$campaign_type == "no_interventions" | input$campaign_type == "PEP_IM_free_only" | input$campaign_type == "PEP_ID_free_only"){
        TargCov <- input$I_VacCovBase
      } else {
        TargCov <- input$VacCovTarget
      }
      TargCov
    })
    
    # Running Regular Model
    IBCMResults <- reactive({
      
      Table <- InputTable()
      
      I_pSeekH <- Table$pSeek_healthy
      I_pCompH <- Table$pComplete_healthy
      I_pStartExp <- Table$pStart_exposure
      I_pCompExp <- Table$pComplete_exposure
      I_FCost <- Table$full_cost
      I_PCost <- Table$partial_cost
      I_VCost1 <- Table$vaccinate_dog_cost1
      I_VCost2 <- Table$vaccinate_dog_cost2
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
    
    # Running Deaths Averted Model
    DA_IBCMResults <- reactive({
      
      DA_Table <- DA_InputTable()
      
      DA_pSeekH <- DA_Table$pSeek_healthy
      DA_pCompH <- DA_Table$pComplete_healthy
      DA_pStartExp <- DA_Table$pStart_exposure
      DA_pCompExp <- DA_Table$pComplete_exposure
      DA_FCost <- DA_Table$full_cost
      DA_PCost <- DA_Table$partial_cost
      DA_VCost1 <- DA_Table$vaccinate_dog_cost1
      DA_VCost2 <- DA_Table$vaccinate_dog_cost2
      
      
      DA_IBCMResults <- decision_tree(N = 100, 
                                   pop = input$I_Humans, 
                                   HDR = c(input$I_HDR[1], input$I_HDR[2]), 
                                   horizon = input$I_Years, 
                                   discount = 0,
                                   mu = 0.38, 
                                   k = 0.14, 
                                   pSeek_healthy=DA_pSeekH,
                                   pBite_healthy=0.01,
                                   pStart_healthy= 0.2,
                                   pComplete_healthy = DA_pCompH,
                                   pSeek_exposure=0.7,
                                   pStart_exposure = DA_pStartExp,
                                   pComplete_exposure = DA_pCompExp,
                                   pDeath = 0.1660119,
                                   pPrevent = 0.986,
                                   full_cost = DA_FCost, 
                                   partial_cost = DA_PCost, 
                                   base_vax_cov = (input$I_VacCovBase / 100),
                                   vaccinate_dog_cost = c(DA_VCost1, DA_VCost2), 
                                   target_vax_cov = (input$I_VacCovBase / 100), 
                                   pInvestigate = 0.9, 
                                   pFound = 0.6, 
                                   pTestable = 0.7, 
                                   pFN = 0.05)
      
      DA_IBCMResults
      
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
    
    IBCM_Healthy_Bite_Dogs <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_healthy_bites', scenario=IR)
      IR2
    })
    
    IBCM_Rabid_Bite_Dogs_Investigated <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_rabid_biting_investigated', scenario=IR)
      IR2
    })
    
    IBCM_Healthy_Bite_Dogs_Investigated <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_healthy_biting_investigated', scenario=IR)
      IR2
    })
    
    IBCM_Rabid_Bite_Dogs_Found <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_rabid_biting_found', scenario=IR)
      IR2
    })
    
    IBCM_Healthy_Bite_Dogs_Found <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_healthy_biting_found', scenario=IR)
      IR2
    })
    
    IBCM_Rabid_Bite_Dogs_Testable <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_rabid_biting_testable', scenario=IR)
      IR2
    })
    
    IBCM_Healthy_Bite_Dogs_Testable <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_healthy_biting_testable', scenario=IR)
      IR2
    })
    
    output$IBCMDogsText <- renderText({
      IRD <- IBCM_Rabid_Dogs()
      IRDB <- IBCM_Rabid_Bite_Dogs()
      IRDI <- IBCM_Rabid_Bite_Dogs_Investigated()
      IRDF <- IBCM_Rabid_Bite_Dogs_Found()
      IRDT <- IBCM_Rabid_Bite_Dogs_Testable()
      IHDI <- IBCM_Healthy_Bite_Dogs_Investigated()
      IHDF <- IBCM_Healthy_Bite_Dogs_Found()
      IHDT <- IBCM_Healthy_Bite_Dogs_Testable()
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
            "being found, and of these",
            ceiling(sum(IRDT$Median)),
            "being testable.",
            "Inevitably, some healthy dogs will end up being tested as well.  This equates to approximatly",
            ceiling(sum(IHDI$Median)),
            "healthy dogs being investigated by a member of the IBCM team.  Of these, an estimated",
            ceiling(sum(IHDF$Median)),
            "healthy dogs being found, and",
            ceiling(sum(IHDT$Median)),
            "being tested.",
            "This results in a total of",
            ceiling(sum(IRDI$Median)) + ceiling(sum(IHDI$Median)),
            "investigations, and",
            ceiling(sum(IRDT$Median)) + ceiling(sum(IHDT$Median)),
            "dogs being tested."
      )
    })
    
    IBCM_Rabid_Dog_Graph_Data <- reactive({
      IRD <- IBCM_Rabid_Dogs()
      IRDB <- IBCM_Rabid_Bite_Dogs()
      IRDI <- IBCM_Rabid_Bite_Dogs_Investigated()
      IRDF <- IBCM_Rabid_Bite_Dogs_Found()
      IRDT <- IBCM_Rabid_Bite_Dogs_Testable()
      DataName <- c("Rabid Dogs", "Biting Dogs","Investigated", "Found", "Tested")
      Dogs <- c(sum(IRD$Median),sum(IRDB$Median),sum(IRDI$Median),sum(IRDF$Median),sum(IRDT$Median))
      DogHealth <- c("Rabid","Rabid","Rabid","Rabid","Rabid")
      IBCM_Rabid_Dog_Graph_Data <- data.frame(DataName,Dogs,DogHealth)
    })
    
    IBCM_Healthy_Dog_Graph_Data <- reactive({
      IHDB <- IBCM_Healthy_Bite_Dogs()
      IHDI <- IBCM_Healthy_Bite_Dogs_Investigated()
      IHDF <- IBCM_Healthy_Bite_Dogs_Found()
      IHDT <- IBCM_Healthy_Bite_Dogs_Testable()
      DataName <- c("Rabid Dogs", "Biting Dogs","Investigated", "Found", "Tested")
      Dogs <- c(0,sum(IHDB$Median),sum(IHDI$Median),sum(IHDF$Median),sum(IHDT$Median))
      DogHealth <- c("Healthy","Healthy","Healthy","Healthy","Healthy")
      IBCM_Healthy_Dog_Graph_Data <- data.frame(DataName,Dogs,DogHealth)
    })
    
    IBCM_Dog_Graph_Data <- reactive({
      Rabid <- IBCM_Rabid_Dog_Graph_Data()
      Healthy <- IBCM_Healthy_Dog_Graph_Data()
      
      if(input$show_healthy_dogs==FALSE){
        IBCM_Dog_Graph_Data <- Rabid
      } else {
        IBCM_Dog_Graph_Data <- rbind(Rabid,Healthy)
      }
      IBCM_Dog_Graph_Data
    })
    
    
    observe({
      my_colors <- c("Rabid" = "#FF8888", "Healthy" = "#88FF88")
      ID <- IBCM_Dog_Graph_Data()
      ID$DataName <- factor(ID$DataName,levels = c("Rabid Dogs", "Biting Dogs", "Investigated", "Found", "Tested"))
      output$IBCMDogsGraph <- renderPlot({ggplot(ID, aes(x = DataName, y = Dogs, fill = DogHealth)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(values = my_colors) +
          ylab("Number of Dogs")+xlab(NULL)+
          theme_bw()})
    })
    
    
    ###  IBCM PATIENTS  ###  
    
    IBCM_Exposures <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exposures', scenario=IR)
      IR2
    })
    
    IBCM_Healthy_Bites <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_healthy_bites', scenario=IR)
      IR2
    })
    
    IBCM_Exposures_Seek <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exposures_seek_care', scenario=IR)
      IR2
    })
    
    IBCM_Healthy_Seek <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_healthy_seek_care', scenario=IR)
      IR2
    })
    
    IBCM_Exposures_Start <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exp_start', scenario=IR)
      IR2
    })
    
    IBCM_Healthy_Start <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_healthy_start', scenario=IR)
      IR2
    })
    
    IBCM_Exposures_Complete <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exp_complete', scenario=IR)
      IR2
    })
    
    IBCM_Healthy_Complete <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_healthy_complete', scenario=IR)
      IR2
    })
    
    IBCM_Exposures_Incomplete <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_exp_incomplete', scenario=IR)
      IR2
    })
    
    IBCM_Healthy_Incomplete <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_healthy_incomplete', scenario=IR)
      IR2
    })
    
    output$IBCMExposuresText <- renderText({
      IE <- IBCM_Exposures()
      IESE <- IBCM_Exposures_Seek()
      IEST <- IBCM_Exposures_Start()
      IEC <- IBCM_Exposures_Complete()
      IEIC <- IBCM_Exposures_Incomplete()
      IHSE <- IBCM_Healthy_Seek()
      IHST <- IBCM_Healthy_Start()
      IHC <- IBCM_Healthy_Complete()
      IHIC <- IBCM_Healthy_Incomplete()
      paste("Over the", 
            input$I_Years,
            "years, there will be approximatly",
            ceiling(sum(IE$Median)),
            "people exposed to a rabid dog in some way such as a bite.  Of these people, an estimated",
            ceiling(sum(IESE$Median)),
            "will seek care, of which an estimated",
            ceiling(sum(IEST$Median)),
            "will start care in the form of post-exposure prophylaxis (PEP).  Of those starting PEP, approximatly",
            ceiling(sum(IEC$Median)),
            "will complete the full recomended course, with the remaining",
            ceiling(sum(IEIC$Median)),
            "patients starting but not completing it.",
            "Not all patients presenting at health facilities will have been exposed to a rabid dog.  Approximatly",
            ceiling(sum(IHSE$Median)),
            "healthy patients will seek care for a bite, of which an estimated",
            ceiling(sum(IHST$Median)),
            "will start care in the form of post-exposure prophylaxis (PEP).  Of those healthy patients starting PEP, approximatly",
            ceiling(sum(IHC$Median)),
            "will complete the full  course.",
            "This results in a total of",
            ceiling(sum(IESE$Median)) + ceiling(sum(IHSE$Median)),
            "patients presenting to health facilities, of which",
            ceiling(sum(IEST$Median)) + ceiling(sum(IHST$Median)),
            "will start PEP, with",
            ceiling(sum(IEC$Median)) + ceiling(sum(IHC$Median)),
            "many pateints completing a course of PEP."
      )
    })
    
    IBCM_Rabies_Exposures_Graph_Data <- reactive({
      IE <- IBCM_Exposures()
      IESE <- IBCM_Exposures_Seek()
      IEST <- IBCM_Exposures_Start()
      IEC <- IBCM_Exposures_Complete()
      IEIC <- IBCM_Exposures_Incomplete()
      DataName <- c("People Biten", "Seek Care","Start Care", "Complete Care", "Incomplete Care")
      Exposures <- c(sum(IE$Median),sum(IESE$Median),sum(IEST$Median),sum(IEC$Median),sum(IEIC$Median))
      Health <- c("Rabid","Rabid","Rabid","Rabid","Rabid")
      IBCM_Rabies_Exposures_Graph_Data <- data.frame(DataName,Exposures,Health)
    })
    
    IBCM_Healthy_Exposures_Graph_Data <- reactive({
      IE <- IBCM_Healthy_Bites()
      IESE <- IBCM_Healthy_Seek()
      IEST <- IBCM_Healthy_Start()
      IEC <- IBCM_Healthy_Complete()
      IEIC <- IBCM_Healthy_Incomplete()
      DataName <- c("People Biten", "Seek Care","Start Care", "Complete Care", "Incomplete Care")
      Exposures <- c(sum(IE$Median),sum(IESE$Median),sum(IEST$Median),sum(IEC$Median),sum(IEIC$Median))
      Health <- c("Healthy","Healthy","Healthy","Healthy","Healthy")
      IBCM_Healthy_Exposures_Graph_Data <- data.frame(DataName,Exposures,Health)
    })
    
    IBCM_Exposures_Graph_Data <- reactive({
      Rabid <- IBCM_Rabies_Exposures_Graph_Data()
      Healthy <- IBCM_Healthy_Exposures_Graph_Data()
      
      if(input$show_healthy_dogs==FALSE){
        IBCM_Exposures_Graph_Data <- Rabid
      } else {
        IBCM_Exposures_Graph_Data <- rbind(Rabid,Healthy)
      }
      IBCM_Exposures_Graph_Data
    })
    
    observe({
      my_colors <- c("Rabid" = "#FF8888", "Healthy" = "#88FF88")
      ID <- IBCM_Exposures_Graph_Data()
      ID$DataName <- factor(ID$DataName, levels = c("People Biten", "Seek Care","Start Care", "Complete Care", "Incomplete Care"))
      output$IBCMExpGraph <- renderPlot({ggplot(ID, aes(x = DataName, y = Exposures, fill = Health)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(values = my_colors) +
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
    
    IBCM_Deaths_Averted_PEP <- reactive({
      IR <- IBCMResults()
      IR2 <- select_variable(variable='ts_deaths_averted_PEP', scenario=IR)
      IR2
    })
    
    IBCM_Deaths_No_Vac <- reactive({
      IR <- DA_IBCMResults()
      IR2 <- select_variable(variable='ts_deaths', scenario=IR)
      IR2
    })
    
    output$IBCMDeathsText <- renderText({
      ID <- IBCM_Deaths()
      IDNV <- IBCM_Deaths_No_Vac()
      IDP <- IBCM_Deaths_No_PEP()
      IDA <- IBCM_Deaths_Averted_PEP()
      SelectedInput <- input$campaign_type
      
      DAVac <- sum(IDNV$Median) - sum(ID$Median)
      
      if(SelectedInput=="MDV_only" | SelectedInput=="MDV_PEP_IM_free" | SelectedInput=="MDV_PEP_ID_free"){
        IBCMDeathsText <- paste("Over the", 
                                input$I_Years,
                                "years, there will be approximatly",
                                ceiling(sum(ID$Median)),
                                "human deaths due to rabies exposure.  Of these deaths,",
                                ceiling(sum(IDP$Median)),
                                "will be due to a the patient recieving no PEP, with any remaining deaths being due to incomplete or inefective PEP.  However, an estimated",
                                ceiling(sum(IDA$Median)),
                                "lives will be saved by access to PEP.  Additionally,",
                                ceiling(DAVac),
                                "lives will be saved by the mass dog vaccination.")
      } else {
        IBCMDeathsText <- paste("Over the", 
                                input$I_Years,
                                "years, there will be approximatly",
                                ceiling(sum(ID$Median)),
                                "human deaths due to rabies exposure.  Of these deaths,",
                                ceiling(sum(IDP$Median)),
                                "will be due to a the patient recieving no PEP, with any remaining deaths being due to incomplete or inefective PEP.  However, an estimated",
                                ceiling(sum(IDA$Median)),
                                "lives will be saved by access to PEP.")
      }

    })
    
    IBCM_Deaths_Graph_Data <- reactive({
      ID <- IBCM_Deaths()
      IDNV <- IBCM_Deaths_No_Vac()
      IDP <- IBCM_Deaths_No_PEP()
      IDA <- IBCM_Deaths_Averted_PEP()
      SelectedInput <- input$campaign_type
      
      if(SelectedInput=="MDV_only" | SelectedInput=="MDV_PEP_IM_free" | SelectedInput=="MDV_PEP_ID_free"){
        DataName <- c("Total Deaths", "Deaths from No PEP", "Deaths Averted by PEP", "Deaths Averted by MDV")
        Deaths <- c(ceiling(sum(ID$Median)),ceiling(sum(IDP$Median)),ceiling(sum(IDA$Median)),ceiling(sum(IDNV$Median)-sum(ID$Median)))
        IBCM_Exposures_Graph_Data <- data.frame(DataName,Deaths)
      } else {
        DataName <- c("Total Deaths", "Deaths from No PEP", "Averted by PEP")
        Deaths <- c(ceiling(sum(ID$Median)),ceiling(sum(IDP$Median)),ceiling(sum(IDA$Median)))
        IBCM_Exposures_Graph_Data <- data.frame(DataName,Deaths)
      }
      
      IBCM_Deaths_Graph_Data <- IBCM_Exposures_Graph_Data
    })
    
    observe({
      ID <- IBCM_Deaths_Graph_Data()
      ID$DataName <- factor(ID$DataName, levels = ID$DataName)
      output$IBCMDeathsGraph <- renderPlot({ggplot(ID, aes(x = DataName, y = Deaths, fill = DataName)) +
          geom_bar(stat = "identity") +
          ylab("Number of Deaths")+xlab(NULL)+
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
            round(sum(ITC$Median),-1),
            "US dollars, with an estimated yearly cost of",
            round((sum(ITC$Median) / input$I_Years),-1),
            "US dollars  Splitting this up, this equates to a total of",
            round(sum(IPC$Median),-1),
            "US dollars (",
            round((sum(IPC$Median) / input$I_Years),-1),
            "US dollars per year ) on PEP, and",
            round(sum(IVC$Median),-1),
            "US dollars (",
            round((sum(IVC$Median) / input$I_Years),-1),
            "US dollars per year ) on vaccinations."
      )
    })
    
    IBCM_Cost_Graph_Data <- reactive({
      ITC <- IBCM_Total_Cost()
      IPC <- IBCM_PEP_Cost()
      IVC <- IBCM_Vac_Cost()
      DataName <- c("Total Cost","PEP Cost", "Dog Vaccination Cost")
      Cost <- c(round(sum(ITC$Median),-1), round(sum(IPC$Median),-1), round(sum(IVC$Median),-1))
      IBCM_Exposures_Graph_Data <- data.frame(DataName,Cost)
    })
    
    observe({
      ID <- IBCM_Cost_Graph_Data()
      ID$DataName <- factor(ID$DataName, levels = ID$DataName)
      output$IBCMCostGraph <- renderPlot({ggplot(ID, aes(x = DataName, y = Cost, fill = DataName)) +
          geom_bar(stat = "identity") +
          ylab("Cost")+xlab(NULL) +
          theme_bw()})
    })
    
    
    ### Time Graph ###

    TimeTextOutput <- reactive({
      if(input$GraphicalOutput == "ts_rabid_dogs"){
        TimeTextOutput <- "Rabid Dogs"
      } else if(input$GraphicalOutput == "ts_exposures"){
        TimeTextOutput <- "Rabies Exposures"
      } else if(input$GraphicalOutput == "ts_exposures_seek_care"){
        TimeTextOutput <- "Rabies Exposures Seeking Care"
      } else if(input$GraphicalOutput == "ts_exp_start"){
        TimeTextOutput <- "Rabies Exposures Starting Care"
      } else if(input$GraphicalOutput == "ts_exp_complete"){
        TimeTextOutput <- "Rabies Exposures Completing Care"
      } else if(input$GraphicalOutput == "ts_healthy_bites"){
        TimeTextOutput <- "Healthy Dog Bites"
      } else if(input$GraphicalOutput == "ts_healthy_seek_care"){
        TimeTextOutput <- "Healthy Patients Seeking Care"
      } else if(input$GraphicalOutput == "ts_healthy_start_care"){
        TimeTextOutput <- "Healthy Patients Starting Care"
      } else if(input$GraphicalOutput == "ts_healthy_complete_care"){
        TimeTextOutput <- "Healthy Patients Completing Care"
      } else if(input$GraphicalOutput == "ts_deaths"){
        TimeTextOutput <- "Human Rabies Deaths"
      } else if(input$GraphicalOutput == "ts_cost_per_year"){
        TimeTextOutput <- "Total Costs"
      } else if(input$GraphicalOutput == "ts_cost_PEP_per_year"){
        TimeTextOutput <- "Total PEP Costs"
      } else if(input$GraphicalOutput == "ts_MDV_campaign_cost"){
        TimeTextOutput <- "Total Dog Vacination Costs"
      }
    })
    
    output$IBCMTimeText <- renderText({
      TTO <- TimeTextOutput()
      paste("This graph shows the number of", 
            TTO,
            "over the",
            input$I_Years,
            "years being looked at."
      )
    })
    
    observe({
      selectedDataframe <- switch(input$GraphicalOutput,
                                  "ts_rabid_dogs" = IBCM_Rabid_Dogs(),
                                  "ts_exposures" = IBCM_Exposures(),
                                  "ts_exposures_seek_care" = IBCM_Exposures_Seek(),
                                  "ts_exp_start" = IBCM_Exposures_Start(),
                                  "ts_exp_complete" = IBCM_Exposures_Complete(),
                                  "ts_healthy_bites" = IBCM_Healthy_Bite_Dogs(),
                                  "ts_healthy_seek_care" = IBCM_Healthy_Seek(),
                                  "ts_healthy_start_care" = IBCM_Healthy_Start(),
                                  "ts_healthy_complete_care" = IBCM_Healthy_Complete(),
                                  "ts_deaths" = IBCM_Deaths(),
                                  "ts_cost_per_year" = IBCM_Total_Cost(),
                                  "ts_cost_PEP_per_year" = IBCM_PEP_Cost(),
                                  "ts_MDV_campaign_cost" = IBCM_Vac_Cost()
      )
      
      Y_Lab <- TimeTextOutput()
      
      output$IBCMTimeGraph <- renderPlot({ggplot(selectedDataframe, aes(x = as.numeric(row.names(selectedDataframe)), y = Median)) +
          geom_line() +
          geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orchid4", alpha = 0.5) +
          ylab(Y_Lab)+ xlab("Year")+
          theme_bw()
      })
    })
    
    
  })
  
}

# Running the App
shinyApp(ui, server)
