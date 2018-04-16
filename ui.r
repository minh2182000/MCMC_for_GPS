library(shiny)
library(plotly)
shinyUI(
  fluidPage(br(),
          # Title
          titlePanel("MCMC"),
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
                        tabPanel("Description"
                        ),
                        tabPanel("Gibbs Sampling",
                                 sliderInput("G.n",
                                             "Number of samples:",
                                             min = 50,  max = 20000, step = 50, value = 5000),
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
                                 fluidRow(wellPanel(plotlyOutput("G.RandomWalk")),  align="center")
                        ),
                        tabPanel("Metropolis - Hasting",
                                 sliderInput("MH.n",
                                             "Number of samples:",
                                             min = 50,  max = 20000, step = 50, value = 5000),
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
                                 fluidRow(wellPanel(plotlyOutput("MH.RandomWalk")),  align="center")
                                 
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