library(shiny)
library(ggplot2)
library(plotly)

library(leaflet)

shinyUI(navbarPage(title = "WDI Explorer", 
                   tabPanel("Introduction", p("This app uses data from the World Development
                                              Inidicators database. It uses an API to access
                                              data for certain economic indicators. After clicking on
                                              World Map Explorer, you can 
                                              select the year, and pick two indicators. The map
                                              displays the data from the first indicator, providing
                                              a comparision of that indicator across the globe. The 
                                        scatterplot below the graph shows the relationship between
                                              the two indicators.")),
                   tabPanel("World Map Explorer",
                            
                            fluidPage(
                              
                            
                              fluidRow(
                                column(8,
                                       leafletOutput("mymap", width = "100%"),
                                       
                                       plotOutput(outputId = "scatterplot", 
                                                  dblclick = "scatterplot_dblclick",
                                                  brush = brushOpts(
                                                    id = "scatterplot_brush",
                                                    resetOnNew = TRUE
                                                  ),
                                                  
                                                  hover = "plot_hover"
                                                  
                                                  ),
                                       verbatimTextOutput("info")
                                      # plotlyOutput("scatterplot")
                                     # plotOutput("scatterplot")
                                       ),
                                
                                
                                
                                column(4,
                                       
                                       sliderInput(inputId = 'year',label = 'Year',  
                                                   min=as.Date("1960", format="%Y"), max=as.Date("2013",format="%Y"), 
                                                   value = as.Date("1960",format="%Y"), timeFormat="%Y"),
                                       br(),
                                       selectInput('firstindicator', 'First Indicator', indicators$name),
                                       br(),
                                       selectInput('secondindicator', 'Second Indicator', indicators$name)
                                       
                                )
                                
                            ))),
                   
                   
                   tabPanel("Data Page", DT::dataTableOutput("mytable"))
                   
                   
                   
))