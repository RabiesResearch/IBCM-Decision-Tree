#####################
#####  OLD APP  #####
#####################

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(RColorBrewer)
library(knitr)
library(dplyr)
library(tidyverse)

source("Code/HelperFun.R") # functions to help in simulations
source("Code/decision_tree.R") # Decision Tree model & summary functions

ui <- dashboardBody(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Model Parameters"),
        fluidRow(
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 selectInput("GraphicalOutput", "Graphical Output", 
                             c("Rabid Dogs" = "rabid_dogs",
                               "Rabies Exposures" = "exposures",
                               "Bite Patients" = "bite_patients",
                               "Healthy Bite Patients" = "healthy_bite_patients",
                               "Deaths" = "deaths",
                               "Lives Saved" = "lives_saved",
                               "Complete PEP Cources" = "PEP_complete",
                               "Incomplete PEP Cources" = "PEP_incomplete",
                               "PEP Cost" = "PEP_cost",
                               "Mass Dog Vaccination Cost" = "MDV_cost",
                               "Total Costs" = "all_cost")))
        ),
        h3("Model Parameters"),
        fluidRow(
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("Repeats", "Stochasticity Repeats", value = 500, min = 1, step = 1)),
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("Years", "Years", value = 5, min = 1, step = 1))
        ),
        h3("Population Inputs"),
        fluidRow(
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("Humans", "Human Population", value = 300000, min = 1, step = 1)),
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("HDR", "Human Dog Ratio", value = 8, min = 0.01, max = 100000, step = 0.01))
        ),
        h3("Epidemiological Inputs"),
        fluidRow(
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericRangeInput("rabies_inc", "Rabies Incedence", value = c(0.0075, 0.0125), min = 0, max = 1, step = 0.0001)),
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericRangeInput("LR_range", "LR_range???", value = c(6.6,12.8), min = 0, step = 0.1)),
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("mu", "Persons bitten per rabid dog", value = 0.38, min = 0, step = 0.01)),
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("k", "Probability of infection without PEP (k?)", value = 0.14, min = 0, step = 0.01))
        ),
        h3("Model Inputs"),
        fluidRow(
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pStart", "pStart", value = 0.6666667, min = 0, max = 1, step = 0.0000001)),
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pComplete", "pComplete", value = 0.3968254, min = 0, max = 1, step = 0.0000001)),
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pDeath", "pDeath", value = 0.1660119, min = 0, max = 1, step = 0.0000001)),
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("pPrevent", "pPrevent", value = 0.986, min = 0, max = 1, step = 0.001))
        ),
        h3("Economic Inputs"),
        fluidRow(
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("FullCost", "Complete PEP Cost", value = 45, min = 0, step = 0.01)),
          column(6,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("PartCost", "Incomplete PEP Cost", value = 25, min = 0, step = 0.01)),
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("CampCost", "Additional Campaign Costs", value = 0, min = 0, max = 1, step = 0.01)),
          column(12,style=list("padding-left: 5px;","padding-right: 5px;"),
                 numericInput("Inflation", "Inflation", value = 0.03, min = 0, max = 1, step = 0.001))
        ),
      ),
      mainPanel(
        fluidPage(
          plotOutput("Graph"),
          valueBoxOutput("TotalRabidDogs"),
          valueBoxOutput("TotalExposures"),
          valueBoxOutput("TotalBitePatients"),
          valueBoxOutput("TotalHealthyBitePatients"),
          valueBoxOutput("TotalDeaths"),
          valueBoxOutput("TotalLivesSaved"),
          valueBoxOutput("TotalPEPComplete"),
          valueBoxOutput("TotalPEPIncomplete"),
          valueBoxOutput("TotalPEPCost"),
          valueBoxOutput("TotalMDVCost"),
          valueBoxOutput("TotalCost")
        )
      )
    ))
)

server <- function(input,output) {
  
  DataFrame1 <- reactive({
    
    DataFrame1 <- decision_tree_ndraw(ndraw = input$Repeats, 
                                      pop = input$Humans, 
                                      HDR = input$HDR, 
                                      horizon = input$Years, 
                                      discount = input$Inflation,
                                      rabies_inc = c(input$rabies_inc[1], input$rabies_inc[2]), 
                                      LR_range = c(input$LR_range[1], input$LR_range[2]), 
                                      mu = input$mu, 
                                      k = input$k,
                                      pStart = input$pStart, 
                                      pComplete = input$pComplete, 
                                      pDeath = input$pDeath, 
                                      pPrevent = input$pPrevent,
                                      full_cost = input$FullCost, 
                                      partial_cost = input$PartCost, 
                                      campaign_cost = input$CampCost)
    
    DataFrame1
    
  })
  
  DataFrame2 <- reactive({
    
    DF1 <- DataFrame1()
    
    column <- input$GraphicalOutput
    
    DF1 <- group_by(DF1, years)
    DataFrame2 <- summarise(DF1, 
                            "value" = ceiling(mean(!!sym(column), na.rm = TRUE)),
                            "value_LC" = round(quantile(!!sym(column), probs = 0.025, names = FALSE, na.rm = TRUE), 0),
                            "value_UC" = round(quantile(!!sym(column), probs = 0.975, names = FALSE, na.rm = TRUE), 0))
    
    DataFrame2
    
  })
  
  DataFrame3 <- reactive({
    
    DF1 <- DataFrame1()
    
    DF1 <- group_by(DF1, years)
    DataFrame3 <- summarise(DF1, 
                            "rabid_dogs" = ceiling(mean(rabid_dogs, na.rm = TRUE)),
                            "exposures" = ceiling(mean(exposures, na.rm = TRUE)),
                            "bite_patients" = ceiling(mean(bite_patients, na.rm = TRUE)),
                            "healthy_bite_patients" = ceiling(mean(healthy_bite_patients, na.rm = TRUE)),
                            "deaths" = ceiling(mean(deaths, na.rm = TRUE)),
                            "lives_saved" = ceiling(mean(lives_saved, na.rm = TRUE)),
                            "PEP_complete" = ceiling(mean(PEP_complete, na.rm = TRUE)),
                            "PEP_incomplete" = ceiling(mean(PEP_incomplete, na.rm = TRUE)),
                            "PEP_cost" = round(mean(PEP_cost, na.rm = TRUE),2),
                            "MDV_cost" = round(mean(MDV_cost, na.rm = TRUE),2),
                            "all_cost" = round(mean(all_cost, na.rm = TRUE),2),
    )
    
    DataFrame3
    
  })
  
  
  
  output$Graph <- renderPlot({ggplot(data = DataFrame2(), aes(x=years, y=value)) +
      geom_line() +
      geom_ribbon(aes(ymin = value_LC, ymax = value_UC),alpha=0.35) +
      theme_bw()
  })
  
  ## TotalRabidDogs
  output$TotalRabidDogs <- renderValueBox({
    datadata <- DataFrame3()
    valueBox(
      paste0(sum(datadata$rabid_dogs)), "Total Rabid Dogs", icon = icon("users-cog"),
      color = "blue"
    )
  })
  
  ## TotalExposures
  output$TotalExposures <- renderValueBox({
    datadata <- DataFrame3()
    valueBox(
      paste0(sum(datadata$exposures)), "Total Rabies Exposures", icon = icon("users-cog"),
      color = "blue"
    )
  })
  
  ## TotalBitePatients
  output$TotalBitePatients <- renderValueBox({
    datadata <- DataFrame3()
    valueBox(
      paste0(sum(datadata$bite_patients)), "Total Bite Patients", icon = icon("users-cog"),
      color = "blue"
    )
  })
  
  ## TotalHealthyBitePatients
  output$TotalHealthyBitePatients <- renderValueBox({
    datadata <- DataFrame3()
    valueBox(
      paste0(sum(datadata$healthy_bite_patients)), "Total Healthy Bite Patients", icon = icon("users-cog"),
      color = "blue"
    )
  })
  
  ## TotalDeaths
  output$TotalDeaths <- renderValueBox({
    datadata <- DataFrame3()
    valueBox(
      paste0(sum(datadata$deaths)), "Total Deaths", icon = icon("users-cog"),
      color = "blue"
    )
  })
  
  ## TotalLivesSaved
  output$TotalLivesSaved <- renderValueBox({
    datadata <- DataFrame3()
    valueBox(
      paste0(sum(datadata$lives_saved)), "Total Lives Saved", icon = icon("users-cog"),
      color = "blue"
    )
  })
  
  ## TotalPEPComplete
  output$TotalPEPComplete <- renderValueBox({
    datadata <- DataFrame3()
    valueBox(
      paste0(sum(datadata$PEP_complete)), "Complete PEP Cources", icon = icon("users-cog"),
      color = "blue"
    )
  })
  
  ## TotalPEPIncomplete
  output$TotalPEPIncomplete <- renderValueBox({
    datadata <- DataFrame3()
    valueBox(
      paste0(sum(datadata$PEP_incomplete)), "Incomplete PEP Cources", icon = icon("users-cog"),
      color = "blue"
    )
  })
  
  ## TotalPEPCost
  output$TotalPEPCost <- renderValueBox({
    datadata <- DataFrame3()
    valueBox(
      paste0(sum(datadata$PEP_cost)), "Total PEP Cost", icon = icon("users-cog"),
      color = "blue"
    )
  })
  
  ## TotalMDVCost
  output$TotalMDVCost <- renderValueBox({
    datadata <- DataFrame3()
    valueBox(
      paste0(sum(datadata$MDV_cost)), "Total MDV Cost", icon = icon("users-cog"),
      color = "blue"
    )
  })
  
  ## TotalCost
  output$TotalCost <- renderValueBox({
    datadata <- DataFrame3()
    valueBox(
      paste0(sum(datadata$all_cost)), "Total Cost", icon = icon("users-cog"),
      color = "blue"
    )
  })
  
}

# Running the App
shinyApp(ui, server)
