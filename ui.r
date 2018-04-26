library(shiny)
library(plotly)
load("default_sims.RData", envir = .GlobalEnv)

shinyUI(
  fluidPage(br(),
          # Title
          titlePanel("Markov Chain Monte Carlo Illustration"),
          sidebarPanel( width = 2,
            p("Set Parameters:"),
            # ------ inputs -------------------------------
            fluidRow(
              numericInput("shape1", "shape1: ", value = 1),
              numericInput("scale1", "scale1: ", value = 1),
              numericInput("shape2", "shape2: ", value = 7.5),
              numericInput("scale2", "scale2: ", value = 2/15),
              numericInput("p", "p: ", value = 0.7),
              actionButton("gobutton", "Simulate"),
              p("will take about 2 minutes")
            )
          ),
          # ------ outputs ---------------------------
          mainPanel(
            br(),
            tabsetPanel(type = "tabs",
                        tabPanel("Description",
                                 p("This is an illustration of Gibbs Sampling and Metropolis - Hastings (MH) algorithm using the Gamma-Poisson Shrinkage Model (GPS), which is a popular hierarchical model for the problem of association study between drugs and adverse events."),
                                 tags$b("Instruction:"), p(paste("Click on tab Gibbs Sampling or Metropolis - Hastings, modify number of steps, and observe convergence. ",
                                 "You can modify parameters then click simulate to run again (will take about 2 minutes).")),
                                 tags$b("Details:"),
                                 tags$iframe(style="height:600px; width:100%", src="https://minhp.weebly.com/uploads/1/1/8/7/118740971/mcmc_app.pdf")
                        
                        ),
                        tabPanel("Gibbs Sampling",
                                 sliderInput("G.n",
                                             "Number of steps:",
                                             min = 10,  max = 20000, step = 10, value = 5000),
                                 p(textOutput("G.nconv")),
                                 fluidRow(wellPanel(plotlyOutput("G.plot1")), align="center"),
                                 fluidRow(
                                   column(width = 6, align="center",
                                          wellPanel(plotlyOutput("G.plot2"))
                                   ),
                                   column(width = 6, align="center",
                                          wellPanel(plotlyOutput("G.plot3"))
                                   )
                                 )
                                 ,
                                 fluidRow(wellPanel(plotlyOutput("G.RandomWalk")),  align="center"),
                                 fluidRow(
                                   column(width = 6, align="center",
                                          wellPanel(plotlyOutput("G.LambdaSeq"))
                                   ),
                                   column(width = 6, align="center",
                                          wellPanel(plotlyOutput("G.XSeq"))
                                   )
                                 )
                        ),
                        tabPanel("Metropolis - Hastings",
                                 sliderInput("MH.n",
                                             "Number of steps:",
                                             min = 10,  max = 20000, step = 10, value = 5000),
                                 p(textOutput("MH.nconv")),
                                 fluidRow(wellPanel(plotlyOutput("MH.plot1")), align="center"),
                                 fluidRow(
                                   column(width = 6, align="center",
                                          wellPanel(plotlyOutput("MH.plot2"))
                                   ),
                                   column(width = 6, align="center",
                                          wellPanel(plotlyOutput("MH.plot3"))
                                   )
                                 )
                                 ,
                                 fluidRow(wellPanel(plotlyOutput("MH.RandomWalk")),  align="center"),
                                 fluidRow(
                                   column(width = 6, align="center",
                                          wellPanel(plotlyOutput("MH.LambdaSeq"))
                                   ),
                                   column(width = 6, align="center",
                                          wellPanel(plotlyOutput("MH.XSeq"))
                                   )
                                 )
                        ),
                        tabPanel("About",
                                 br(),
                                 p("Source code: ", a("https://github.com/minh2182000/MCMC_for_GPS", href = "https://github.com/minh2182000/MCMC_for_GPS")),
                                 p("Author: ", a("My Website", href = "https://minhp.weebly.com/"))
                                
                        )
            )
          )
        )
)