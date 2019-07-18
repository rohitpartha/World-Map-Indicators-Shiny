library(shiny)
library(leaflet)
library(ggplot2)
library(geojsonio)
library(sp)
library(wbstats)
library(dplyr)
library(tidyr)
library(DT) 
library(plotly)

Countries <- 
  geojson_read( 
    "./countries.geojson"
    , what = "sp"
  )


shinyServer(function(input, output, session){
  
  ###The first section of the code gets the data from the WBI database through the wb library
  #1 First, get the correct row from the indicators table
  firstindicatorrow <- reactive({indicators %>% filter(., name == input$firstindicator)})
  secondindicatorrow <- reactive({indicators %>% filter(., name == input$secondindicator)})
  
  
  #2. get the indicator code based on the indicator the user has selected
  firstindicatorcode <- reactive({firstindicatorrow()[["indicator.code"]]})
  
  #2. get the second indicator code
  secondindicatorcode <- reactive({secondindicatorrow()[["indicator.code"]]})
  
  #get the dataframe column name
  firstindicatorcolname <- reactive({firstindicatorrow()[["dataframe.name"]]})
  secondindicatorcolname <- reactive({secondindicatorrow()[["dataframe.name"]]})
  
  #get the labels for the graphs
  firstindicatorlabel <- reactive({firstindicatorrow()[["label"]]})
  secondindicatorlabel <- reactive({firstindicatorrow()[["label"]]})
  
  #3. get the year based on the year the user has selected
  yearselected <- reactive({as.numeric(format(input$year, "%Y"))})
  
  #4. Call the WB database to get the values for the two indicators. If the first indicator is the same as the second 
  # indicator, call the WBI database for the first indicator only
  twoindicatordataset <- reactive(if(firstindicatorcode() == secondindicatorcode()){
   temp <- wb(indicator = firstindicatorcode(), startdate = yearselected(), enddate = yearselected())
   temp <- subset(temp, select = -c(indicatorID))
   temp <- spread(temp, indicator, value)
   names(temp)[names(temp) == input$firstindicator] <- firstindicatorcolname()
   
   
   if(firstindicatorcode() == "SP.POP.TOTL"){
     temp[['population']] <- temp[['population']]/1e6
     
     
   }
   if(firstindicatorcode() == "NY.GDP.MKTP.CD"){
     temp[['gdp']] <- temp[['gdp']]/1e9
     
   }
   if(firstindicatorcode() == "BX.KLT.DINV.CD.WD"){
     temp[['foreign.direct.investment']] <- temp[['foreign.direct.investment']]/1e6
     
   }
   if(firstindicatorcode() == "DT.ODA.ALLD.CD"){
     temp[['development.assistance']] <- temp[['development.assistance']]/1e6
     
   }
   return(temp)
   
   
   
  }
  else{
  
    print(firstindicatorcode())
    print(secondindicatorcode())
     temp <- wb(indicator = c(firstindicatorcode(),
                     secondindicatorcode()), startdate = yearselected(), enddate = yearselected())
   print(temp %>% filter(., indicatorID == firstindicatorcode()))
   print(temp %>% filter(., indicatorID == secondindicatorcode())) 
     temp <- subset(temp, select = -c(indicatorID))
   
   temp <- spread(temp, indicator, value)
   print(names(temp))
   names(temp)[names(temp) == input$firstindicator] <- firstindicatorcolname()
   names(temp)[names(temp) == input$secondindicator] <- secondindicatorcolname()
   
   if(firstindicatorcode()  == "SP.POP.TOTL" || secondindicatorcode() == "SP.POP.TOTL"){
     
     temp[['population']] <- temp[['population']]/1e6
     
     
   }
   if(firstindicatorcode() == "NY.GDP.MKTP.CD" || secondindicatorcode() == "NY.GDP.MKTP.CD"){
     
     temp[['gdp']] <- temp[['gdp']]/1e9
     
   }
   
   if(firstindicatorcode() == "BX.KLT.DINV.CD.WD" || secondindicatorcode() == "BX.KLT.DINV.CD.WD"){
     temp[['foreign.direct.investment']] <- temp[['foreign.direct.investment']]/1e6
     
   }
   if(firstindicatorcode() == "DT.ODA.ALLD.CD" || secondindicatorcode() == "DT.ODA.ALLD.CD" ){
     temp[['development.assistance']] <- temp[['development.assistance']]/1e6
     
   }
   
   
   
   return(temp)
   }
  
  

  
  ) 
  
  
  
  
  
  #5. merge the database data with the geojson data
  mergedcountries <- reactive({sp::merge(Countries, twoindicatordataset(), by.x = "ISO_A2", by.y = "iso2c", all.x = FALSE)})
  
  
  
  
  
  ###The next section of the code renders the data into various plots
  
  
  
  quantileinterval <- reactive(quantile(mergedcountries()@data[[firstindicatorcolname()]], probs=seq(0, 1, by = 1/6),
                                        na.rm = TRUE))
  
  
  #render the map
  
  
  
  
  output$mymap <- renderLeaflet({
   
    #bins <- c(0,10,20,50,100,500, Inf)
    #mypalette <- colorBin(palette = "YlOrRd", domain = mergedcountries()$value, na.color = "transparent", 
                          #  bins = bins)
    
    #addProviderTiles("Stamen.Watercolor",options = providerTileOptions(nowrap = TRUE))
    #leaflet(mergedcountries(), options = leafletOptions(worldCopyJump = TRUE)) %>% setView(lat = 10, lng = 0, zoom = 1) %>% 
    ##  addTiles() %>% addPolygons(
      #  fillColor = ~mypalette(value),
      #  weight = 2,
      #  opacity = 1,  
      ##  color = "white",
      #  dashArray = "3",
      #  fillOpacity = .7)
    #quantile.interval = quantile(mergedcountries()@data$value, probs=seq(0, 1, by = 1/6), na.rm = TRUE)
    
    labels <- sprintf("<strong>%s</strong><br/>
                      <strong>%s:</strong> %g", mergedcountries()[["NAME"]],
                      input$firstindicator, mergedcountries()[[firstindicatorcolname()]]   ) %>% lapply(htmltools::HTML)
    
    bins <- quantileinterval()
    pal <- colorBin("YlOrRd", domain = mergedcountries()@data[[firstindicatorcolname()]], bins = bins)
    
    title <- 
    leaflet(mergedcountries(), options = leafletOptions(worldCopyJump = TRUE)) %>% setView(lat = 10, lng = 0, zoom = 1) %>% 
      addTiles() %>% addPolygons(
        weight = 2,
        opacity = 1,  
        #color = colorQuantile("YlOrRd",mergedcountries()[[input$firstindicator]], n = 6),
        fillColor = ~pal(mergedcountries()@data[[firstindicatorcolname()]]),
        dashArray = "3",
        fillOpacity = .7,
        label = labels,
        highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = .7, bringToFront = TRUE)) %>% addLegend(position = "bottomleft", 
                                                                                                   pal = pal, values = bins, na.label = "NA", title = firstindicatorlabel() )
  
    
    })
  
 
  #generate histogram of first indicator data
  
  #output$histvalues <- renderPlot({
  #  x <- na.omit(mergedcountries()$)
  #  bins <- seq(min(x), max(x), length.out = 50)
  #  hist(x, breaks = quantileinterval())
  #})
  
  
  
   #generate scatterplot of two selected data sets for specified year
    #render the scatter plot
    
    #create zoom capability 
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    observeEvent(input$scatterplot_dblclick, {
      brush <- input$scatterplot_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
  
  
    #firstindicatordata <- reactive(spreaddata()[[input$firstindicator]])
    #econdindicatordata <- reactive(spreaddata()[[input$secondindicator]])
    
    
    
    output$scatterplot <- renderPlot({
      
                                        ggplot(mergedcountries()@data, aes_string(x = firstindicatorcolname() , y = secondindicatorcolname())) +
                                        geom_point() + 
                                        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
                                        })
    
    output$info <- renderText({
      xy_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
      }
      xy_range_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
               " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
      }
      
      paste0(
        "hover: ", xy_str(input$plot_hover)
      )
    })
    
   # output$scatterplot <- renderPlotly({
    #output$scatterplot <- renderPlot({
                                              #print(mergedcountries()@data)
    #          ggplot(mergedcountries()@data, aes_string(x = firstindicatorcolname() , y = secondindicatorcolname(), label = "country")) + geom_point() 
                                                
                          #                 plot <- ggplot(mergedcountries()@data, aes_string(x = firstindicatorcolname() , y = secondindicatorcolname(),
                           #                                                                  label = "country")) + geom_point() 
                                           
                            #               plot %>% ggplotly() %>% layout(xaxis = list(title = firstindicatorlabel()), yaxis = list(title = secondindicatorlabel()))
                                                                       
                                                                          
                                          
                             #           })
      
    #display the data
    # first, select the columns to display
    dataframedisplay <- reactive(mergedcountries()@data %>% select(.,one_of(c(firstindicatorcolname(),
                                                                     secondindicatorcolname())),
                                                        country,ISO_A2,
                                                        ISO_A3,
                                                        ECONOMY,
                                                        INCOME_GRP,
                                                        CONTINENT,
                                                        REGION_WB))
    
    output$mytable <- DT::renderDataTable(dataframedisplay()) 
    
  })
  
  
  
  
  
  
