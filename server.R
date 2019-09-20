#The server.R script contains the instructions that your computer needs to build your app

library(shiny)
library(leaflet)
library(reshape)
library(rgdal)
library(shinyjs)
library(jsonlite,pos=100)
library(httr)
library(DT)
library(RColorBrewer)

library(tidyverse)
library(openair)
##### Begin Server Function ####

shinyServer(function(input,output){
  
#################### Time series planel ##############################################
###################### Create set of reactive selection boxes in UI  ####################
  ### select site based on park
  output$SiteResultsA <- renderUI({ 
    
    df_sub<-subset(df, LongName %in% input$park & Type %in% input$loc)    
    selectInput(inputId='site', label='Select Site',  choices = unique(df_sub$SiteName))
  })
  
  
  output$VarResults <- renderUI({ 
    # dynamically select parm based on location type. This will cause the UI to update 
    df_sub<-subset(df,  LongName %in% input$park & Type %in% input$loc & SiteName %in% input$site) # & Type %in% input$loc
  selectInput(inputId='parm', label='Select variable to plot', choices=unique(df_sub$DisplayName), selected = "Water Temperature")
  })
  

#######################################################   
### time series plot
   
 DataTot <- reactive({ 
       subset(df, LongName %in% input$park & Type %in% input$loc & SiteName %in% input$site & DisplayName %in% input$parm)
 })
      

DataSurf <- reactive({
  subset(DataTot(), SampleDepth == "epilimnion" | SampleDepth == "stream" | is.na(SampleDepth))  
  
})
      
  output$plot <- renderPlot({
      #check for data
             if(nrow(DataSurf())== 0){
          stop("Sorry, this variable has not been collected at this site.")
        }
      

      #make base plot:
    data.to.plot <- DataSurf()
    data.to.plot <- data.to.plot[order(data.to.plot$date),]
    unit <- unique(data.to.plot$Units)
    p <- plot(data.to.plot$Visit.Start.Date, data.to.plot$value,
              bty = "l", 
              type = "o", 
              pch = data.to.plot$plotting.symbol, 
              cex = 2,
              col = data.to.plot$plotting.color, 
              xlab = "Date", 
              ylab = unit,
              main = input$parm
              )

      #' calculate trend WITH deseason  
      #' out <- TheilSen(mydata = data, pollutant = "value", deseason = TRUE)
      
      #' #add trend line to plot:
      #' slope <- mean(out$data$main.data$slope, na.rm=T)
      #' int <- mean(out$data$main.data$intercept, na.rm=T)
      #' line.type <- ifelse(mean(out$data$main.data$p, na.rm=T) <= 0.05, 1, 2)
      #' #'openair' TheilSen assumes intercept is at "1970/1/1"
      #' ex <- seq(as.Date(min(data$Visit.Start.Date)), as.Date(max(data$Visit.Start.Date)), "years"#)
      #' why <- int + slope*(as.numeric(format(ex, '%Y'))-1970)
      #' p <- (p + points(ex, why, lty = line.type, type = "l", lwd = 2))
      #'     
      
      #output text
      # main = paste0("m = ",round(mean(out$data$main.data$slope, na.rm=T),2),"[",round(mean(out$data$main.data$lower, na.rm=T),2),",",round(mean(out$data$main.data$upper, na.rm=T),2),"], p = ",round(mean(out$data$main.data$p, na.rm=T),2))
      
      #' calculate trend WITHOUT deseason  
      #' out2 <- TheilSen(mydata = data, pollutant = "value", deseason = FALSE)
      
      #' #add trend line to plot:
      #' slope <- mean(out2$data$main.data$slope, na.rm=T)
      #' int <- mean(out2$data$main.data$intercept, na.rm=T)
      #' line.type <- ifelse(mean(out2$data$main.data$p, na.rm=T) <= 0.05, 1, 2)
      #' #'openair' TheilSen assumes intercept is at "1970/1/1"
      #' ex <- seq(as.Date(min(data$Visit.Start.Date)), as.Date(max(data$Visit.Start.Date)), "years")
      #' why <- int + slope*(as.numeric(format(ex, '%Y'))-1970)
      #' p <- (p + points(ex, why, lty = line.type, type = "l", lwd = 2))
      #'     
      
      #output text
      # main = paste0("m = ",round(mean(out2$data$main.data$slope, na.rm=T),2),"[",round(mean(out2$data$main.data$lower, na.rm=T),2),",",round(mean(out2$data$main.data$upper, na.rm=T),2),"], p = ",round(mean(out2$data$main.data$p, na.rm=T),2))
 
      
       print(p)}, height = 600, width = 800)
   

  
  ##################################### Sampling Effort plot panel ###################
  ### UI  steup for choosing parms per site
  
  ### select type
  output$ParmResultsB <- renderUI({ 
    
    df_sub<-subset(df,Type %in% input$locB)
    
    selectInput(inputId='parmB', label='Select Variable', choices= unique(df_sub$DisplayName), selected = "Dissolved Oxygen")
  })
  
  ###plot
  output$plot2 <- renderPlot({

      # select by site and parms
      data2<-subset(df, Type %in% input$locB & DisplayName %in% input$parmB)
    
      # Create color palette for heatmaps
      heat.pal.spectral <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
      #Calculate number of visits per year
      temp <- data2 %>%
        group_by(StationID, Year) %>%
        dplyr::summarize(n.samps = length(unique(Visit.Start.Date))) %>%
        ungroup()
      #Plot spatio-temporal sampling effort	
      p2 <- (ggplot(data = temp, aes(x = Year, y = StationID, fill = n.samps)) +
              geom_raster() +
              scale_fill_gradientn(colours = heat.pal.spectral(100), name = "Visits") +
              theme_bw() +
              xlab("Year") +
              ylab("Site") +
              theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90),  text = element_text(size = 9)))
  

      print(p2)
    
  }
  , height = 600, width = 800)
  
  
  
  
}) ## end shiny serverfunc    



