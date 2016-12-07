library(shiny)
require(dygraphs)
source('functions.R')
shinyUI(navbarPage("R Backtester",
                   
                   #Tab 1: Performance Viewer
                   tabPanel("PORTFOLIO BUILDER",
                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                selectizeInput(
                                  'selectPrimary', 'Primary Series', choices = universal, multiple = TRUE
                                ),
                                
                                #date range UI 
                                dateRangeInput('dateRange',
                                               label = 'Select range (yyyy-mm-dd)',
                                               start = Sys.Date() - 1 - 365, end = Sys.Date() - 1),
                                
                                radioButtons("radio.Benchmark", "Select Benchmark",
                                             list("SPY", "SPY + TLT")),
                                
                                actionButton("submitButton", "Submit"),
                                
                                h4("Basic Stats"),
                                tableOutput("perf.Stats")
                                
                                
                              ),
                              
                              mainPanel(
                                
                                #$1 index panel UI, and link to server.R by "vami" tag
                                dygraphOutput("index", height = "400px"),
                                
                                #price panel UI, and link to server.R by "" tag
                                dygraphOutput("price", height = "200px")
                                
                              )
                            )
                   ),
                   
                   #Tab 2: Factor Analysis
                   tabPanel("FACTOR ANALYZER",
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                                selectizeInput(
                                  'selectSecondary', 'Secondary Series', choices = universal, multiple = FALSE,
                                  options = list(
                                    placeholder = 'Self Trading',
                                    onInitialize = I('function() { this.setValue(""); }'))
                                ),
                                # selectInput('selectStrategy','Select Strategy', choices = strategyMethods),
                                uiOutput("selectStrategy"),
                                sliderInput('smoothing','Smoothing',1,30,5,step = 1), 
                                sliderInput('length', 'Length', 1,90,21, step = 1),
                                conditionalPanel("input.selectStrategy != 'Momentum'", sliderInput('threshold','Threshold', 0,2.5,1,step = 0.5)),
                                actionButton("changeButton", "Submit"),
                                checkboxInput('applyStrategy', 'Apply Strategy')
                              ),
                              
                              mainPanel(
                                
                                #Strategy plot
                                dygraphOutput("strategy", height = "500px"),
                                conditionalPanel("input.selectSecondary.length > 0", dygraphOutput("secondary", height = "200px")),
                                dygraphOutput("indicator", height = "200px"),
                                conditionalPanel("input.applyStrategy = true", dygraphOutput("perf", height = "400px"))
                                
                              )
                            )
                   ),
                   
                   #Tab 3: 
                   tabPanel("TAB3_NAME",
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                
                              ),
                              
                              mainPanel(
                                
                              )
                            )
                   )
)
)
