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
library(NADA)
source("./functions/plot.depth.profile.R")
source("./functions/plot.sampling.effort.R")

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

   
###### time series plot ----
   
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
            bty = "l", 
            type = "o", 
            pch = data.to.plot$plotting.symbol, 
            cex = 2,
            col = data.to.plot$plotting.color, 
            xlab = "Date", 
            ylab = unit,
            main = input$parm
  )
  
  # calculate trend WITH deseason
  
  # calculate trend WITHOUT deseason
   if(input$trendType == "Theil-Sen (NOT deseasoned)"){
    
    NoSeasonTrend <- TheilSen(mydata = DataGraph(), pollutant = "value", deseason = FALSE, plot=FALSE)
    
    #add trend line to plot:
    slope <- mean(NoSeasonTrend$data$main.data$slope, na.rm=T)
    int <- mean(NoSeasonTrend$data$main.data$intercept, na.rm=T)
    line.type <- ifelse(mean(NoSeasonTrend$data$main.data$p, na.rm=T) <= 0.05, 1, 2)
    #' #'openair' TheilSen assumes intercept is at "1970/1/1"
    ex <- seq(as.Date(min(data.to.plot$Visit.Start.Date)), as.Date(max(data.to.plot$Visit.Start.Date)), "years")
    why <- int + slope*(as.numeric(format(ex, '%Y'))-1970)
    p <- (p + points(ex, why, lty = line.type, type = "l", lwd = 2))
    
    if(input$TheilSenPlot == "Theil-Sen plot"){
      
      plot(NoSeasonTrend)}
    else{
      
      print(p)
    }
    
  }
  
   else if(input$trendType == "Theil-Sen (deseasoned)"){
    
    SeasonalTrend <- TheilSen(mydata = DataGraph(), pollutant = "value", deseason = TRUE, plot=FALSE, type="month")
    
    #add trend line to plot:
    slope <- mean(SeasonalTrend$data$main.data$slope, na.rm=T)
    int <- mean(SeasonalTrend$data$main.data$intercept, na.rm=T)
    line.type <- ifelse(mean(SeasonalTrend$data$main.data$p, na.rm=T) <= 0.05, 1, 2)
    #' #'openair' TheilSen assumes intercept is at "1970/1/1"
    ex <- seq(as.Date(min(data.to.plot$Visit.Start.Date)), as.Date(max(data.to.plot$Visit.Start.Date)), "years")
    why <- int + slope*(as.numeric(format(ex, '%Y'))-1970)
    p <- (p + points(ex, why, lty = line.type, type = "l", lwd = 2))
    
    if(input$TheilSenPlot2 =="Plot trend by month"){
      
      plot(SeasonalTrend)}
    
    else{

    print(p)
    }
    
  }

  
  
  # calculate trend for censored data
  else if(input$trendType == "Akritas-Theil-Sen (for censored data)"){

    #create vector to indicate which observations are censored:
    data.to.plot <- data.to.plot %>%
      mutate(ycen = ifelse(data.to.plot$Result.Value.Text == "*Present <QL", TRUE, FALSE))
    #create vector of julian date (days since 1970-01-01) and divide by 365 (a very slight approximatin in leap years) so that the slope units are in units per year.
    data.to.plot$year.dec <- julian(data.to.plot$Visit.Start.Date)/365
    #run analysis
    out <- cenken(y = data.to.plot$value, ycen = data.to.plot$ycen, x = data.to.plot$year.dec)
    
    #add trend line to plot:
    p <- plot(data.to.plot$year.dec + 1970, data.to.plot$value,
              bty = "c", 
              type = "o", 
              pch = data.to.plot$plotting.symbol, 
              cex = 2,
              col = data.to.plot$plotting.color, 
              xlab = "Date", 
              ylab = unit,
              main = input$parm)
    line.type <- ifelse(out$p <= 0.05, 1, 2)
    ex <- seq(min(data.to.plot$year.dec), max(data.to.plot$year.dec), 0.1)
    why <- out$intercept + out$slope*(ex)
    p <- (p + points(ex+1970, why, lty = line.type, type = "l"))
    
    print(p)
    
  }
  
  
  print(p)})

#### Add model outputs in table ----
output$modelout <- renderTable({

  if(input$trendType == "Theil-Sen (NOT deseasoned)" ){
    
    #output text from model fit
    
    unitsLab<- ifelse(is.na(DataGraph()$Units[1]), "Slope (units/ year)",
    
                                        paste0("Slope (",DataGraph()$Units[1],"/year)"))
    
    NoSeasonTrend <- TheilSen(mydata = DataGraph(), pollutant = "value", deseason = FALSE, plot=FALSE)
    
    as.tibble(NoSeasonTrend$data$main.data) %>%
      summarise(Slope =  round(mean(slope, na.rm=T),2),
                P = round(mean(p, na.rm=T),3),
                Intercept= round(mean(intercept, na.rm=T),2)) %>%
      add_column(Month = "Overall") %>%
      dplyr::select(Month, !!unitsLab := Slope, P, Intercept)
   
    
  }
  
  else if(input$trendType == "Theil-Sen (deseasoned)"){

    #output text from model fit

      overall<-as.tibble(SeasonalTrend$data$res2) %>% ## acess contents of 
            summarise(Slope=  round(mean(slope, na.rm=T),2), 
            P = round(mean(p, na.rm=T),3),
              Intercept= round(mean(intercept, na.rm=T),2)) %>% 
            add_column(Month = "Overall")
    
    
    months<-as_tibble(SeasonalTrend$data$res2) %>% 
      select(Month= month, P = p, Slope = slope, Intercept= intercept) %>% 
      mutate(Slope =round(Slope, 2),P = round(P,3), Intercept =round(Intercept,2))
             
    unitsLab<- ifelse(is.na(DataGraph()$Units[1]), "Slope (units/ year)",
                      paste0("Slope (",DataGraph()$Units[1],"/year)"))
    
    bind_rows(overall,months) %>% 
      select(Month, !!unitsLab := Slope, P, Intercept)
  }
  
  else if(input$trendType == "Akritas-Theil-Sen (for censored data)" ){

    #data:
    data.to.plot <- DataGraph()
    data.to.plot <- data.to.plot[order(data.to.plot$date),]

    #Fit model
    data.to.plot <- data.to.plot %>%
      mutate(ycen = ifelse(data.to.plot$Result.Value.Text == "*Present <QL", TRUE, FALSE))
    #create vector of julian date (days since 1970-01-01) and divide by 365 (a very slight approximatin in leap years) so that the slope units are in units per year.
    data.to.plot$year.dec <- julian(data.to.plot$Visit.Start.Date)/365
    #run analysis
    out <- cenken(y = data.to.plot$value, ycen = data.to.plot$ycen, x = data.to.plot$year.dec)

    #output text from model fit
    unitsLab<- ifelse(is.na(DataGraph()$Units[1]), "Slope (units/ year)",
                      paste0("Slope (",DataGraph()$Units[1],"/year)"))

    table1 <- t(as.matrix(out)) 
    class(table1) <- "matrix"
    as.data.frame(table1) %>%
      rename(Slope =slope,
             P = p, 
             Intercept= intercept) %>%
      add_column(Month = "Overall") %>%
      dplyr::select(Month, !!unitsLab := Slope, P, Intercept)


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

    p2 <- plot.sampling.effort(data = df, park = input$parkC, type = input$locB, variable = input$parmB)

    print(p2)

}

  , height = 600, width = 800)
  
  
  
  
}) ## end shiny serverfunc    



