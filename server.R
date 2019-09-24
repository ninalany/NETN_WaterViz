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
library(magrittr)

library(tidyverse)
library(openair)
source("./functions/plot.depth.profile.R")

##### Begin Server Function ####

shinyServer(function(input,output){
  
#################### Time series panel ##############################################
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
  
  # year control
  
  output$yearControl<-renderUI({
    
    sliderInput(inputId="YearsShow", label= "Years to Display:", min=min(DataSurf()$Year, na.rm=T),  max=max(DataSurf()$Year, na.rm=T),
                value=c( min(DataSurf()$Year,na.rm=T), max=max(DataSurf()$Year,na.rm=T) ), sep=""
    )
  })

#######################################################   
### time series plot
   
 DataTot <- reactive({ 
       subset(df, LongName %in% input$park & Type %in% input$loc & SiteName %in% input$site & DisplayName %in% input$parm)
 })
      

DataSurf <- reactive({
  subset(DataTot(), SampleDepth == "epilimnion" | SampleDepth == "stream" | is.na(SampleDepth))  
  
})
      
DataGraph<-reactive({DataSurf()[DataSurf()$Year>=input$YearsShow[1] & DataSurf()$Year<=input$YearsShow[2],]})

output$plot <- renderPlot({
  #check for data
  if(nrow(DataGraph())== 0){
    stop("Sorry, this variable has not been collected at this site.")
  }
  
  
  #make base plot:
  data.to.plot <- DataGraph()
  data.to.plot <- data.to.plot[order(data.to.plot$date),]
  unit <- unique(data.to.plot$Units)
  p <- plot(data.to.plot$Visit.Start.Date, data.to.plot$value,
            bty = "c", 
            type = "o", 
            pch = data.to.plot$plotting.symbol, 
            cex = 2,
            col = data.to.plot$plotting.color, 
            xlab = "Date", 
            ylab = unit,
            main = input$parm
  )
  
  # calculate trend WITH deseason
  if(input$trendType == "Theil-Sen (deseasoned)"){
    
    out <- TheilSen(mydata = data.to.plot, pollutant = "value", deseason = TRUE, plot=FALSE)
    
    #add trend line to plot:
    slope <- mean(out$data$main.data$slope, na.rm=T)
    int <- mean(out$data$main.data$intercept, na.rm=T)
    line.type <- ifelse(mean(out$data$main.data$p, na.rm=T) <= 0.05, 1, 2)
    #' #'openair' TheilSen assumes intercept is at "1970/1/1"
    ex <- seq(as.Date(min(data.to.plot$Visit.Start.Date)), as.Date(max(data.to.plot$Visit.Start.Date)), "years")
    why <- int + slope*(as.numeric(format(ex, '%Y'))-1970)
    p <- (p + points(ex, why, lty = line.type, type = "l", lwd = 2))
    
    print(p)
    
  }

  # calculate trend WITHOUT deseason
  if(input$trendType == "Theil-Sen (NOT deseasoned)"){
    
    out <- TheilSen(mydata = data.to.plot, pollutant = "value", deseason = FALSE, plot=FALSE)
    
    #add trend line to plot:
    slope <- mean(out$data$main.data$slope, na.rm=T)
    int <- mean(out$data$main.data$intercept, na.rm=T)
    line.type <- ifelse(mean(out$data$main.data$p, na.rm=T) <= 0.05, 1, 2)
    #' #'openair' TheilSen assumes intercept is at "1970/1/1"
    ex <- seq(as.Date(min(data.to.plot$Visit.Start.Date)), as.Date(max(data.to.plot$Visit.Start.Date)), "years")
    why <- int + slope*(as.numeric(format(ex, '%Y'))-1970)
    p <- (p + points(ex, why, lty = line.type, type = "l", lwd = 2))
    
    print(p)
    
  }
  
  
  print(p)})

output$modelout <- renderTable({

  if(input$trendType == "Theil-Sen (NOT deseasoned)" ){
    
    #make base plot:
    data.to.plot <- DataGraph()
    data.to.plot <- data.to.plot[order(data.to.plot$date),]
    
    #Fit model
    
    out2 <- TheilSen(mydata = data.to.plot, pollutant = "value",
                     deseason = FALSE, plot=FALSE)
    
    #output text from model fit
    
    unitsLab<- ifelse(is.na(DataGraph()$Units[1]), "Slope (units/ year)",
                      paste0("Slope (",DataGraph()$Units[1],"/year)"))
    
    as.tibble(out2$data$main.data) %>%
      summarise(Slope =  round(mean(slope, na.rm=T),2),
                P = round(mean(p, na.rm=T),3),
                Intercept= round(mean(intercept, na.rm=T),2)) %>%
      add_column(Month = "Overall") %>%
      dplyr::select(Month, !!unitsLab := Slope, P, Intercept)
    
      # colnames(slopes)[which(colnames(slopes)=="Slope")]= unitsLab
   
    
  }
  
  else if(input$trendType == "Theil-Sen (deseasoned)"){

    #make base plot:
    data.to.plot <- DataGraph()
    data.to.plot <- data.to.plot[order(data.to.plot$date),]
    
    #Fit seasonal model
    
    out2 <- TheilSen(mydata = data.to.plot, pollutant = "value",
                     deseason = TRUE, plot=FALSE, type="month")

    #output text from model fit

      overall<-as.tibble(out2$data$main.data) %>% 
            summarise(Slope=  round(mean(slope, na.rm=T),2), 
            P = round(mean(p, na.rm=T),3),
              Intercept= round(mean(intercept, na.rm=T),2)) %>% 
            add_column(Month = "Overall")
    
    
    months<-as_tibble(out2$data$res2) %>% 
      select(Month= month, P = p, Slope = slope, Intercept= intercept) %>% 
      mutate(Slope =round(Slope, 2),P = round(P,3), Intercept =round(Intercept,2))
             
    unitsLab<- ifelse(is.na(DataGraph()$Units[1]), "Slope (units/ year)",
                      paste0("Slope (",DataGraph()$Units[1],"/year)"))
    
    bind_rows(overall,months) %>% 
      select(Month, !!unitsLab := Slope, P, Intercept)
  }
  
  
  }, caption = "Model results for selected trend test",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))


#################### Depth profile panel ##############################################
###################### Create set of reactive selection boxes in UI  ####################
### select site based on park
output$SiteResultsB <- renderUI({ 
  
  df_sub<-subset(df, LongName %in% input$parkB & Type %in% "Lake")    
  selectInput(inputId='siteA', label='Select Site',  choices = unique(df_sub$SiteName))
  
  
})

# year control

output$yearControlB<-renderUI({
  
  sliderInput(inputId="YearsShowB", label= "Years to Display:", min=min(DataTot()$Year, na.rm=T),  max=max(DataTot()$Year, na.rm=T),
              value=c(min(DataTot()$Year,na.rm=T), max=max(DataTot()$Year,na.rm=T) ), sep=""
  )
})

# month control

output$monthControlB<-renderUI({
  
  sliderInput(inputId="MonthsShowB", label= "Months to Display:", min=min(DataTot()$Month, na.rm=T),  max=max(DataTot()$Month, na.rm=T),
              value=c(min(DataTot()$Month,na.rm=T), max=max(DataTot()$Month,na.rm=T) ), sep=""
  )
})


#make plot
output$plot3<- renderPlot({
  
  DataTot <- reactive({ 
    subset(df, LongName %in% input$parkB) %>% 
      filter(!SampleDepth %in% "epilimnion" | !SampleDepth %in% "stream" | !is.na(SampleDepth))
  })

  selectedYears <- input$YearsShowB[1]:input$YearsShowB[2]
  selectedMonths <- input$MonthsShowB[1]:input$MonthsShowB[2]  
  p<-plot.depth.profile(data=DataTot() , location= input$siteA, variable = input$parmC, months=selectedMonths, years=selectedYears, add.legend = input$legend)
  
  print(p)
  
}
  
)




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
      data2<-subset(df, LongName %in% input$parkC & Type %in% input$locB & DisplayName %in% input$parmB)
    
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
              theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90),  text = element_text(size = 14)))
  

      print(p2)
    
  }
  , height = 600, width = 800)
  
  
  
  
}) ## end shiny serverfunc    



