
# The user-interface (ui) script controls the layout and appearance of your app. 

library(shiny)
library(leaflet)
library(shinyjs)
library(DT)


shinyUI(navbarPage(title=HTML("<div> <a href='https://science.nature.nps.gov/im/units/netn/'> <img src='ah_small_black.gif',
          alt='WQ Visualizer'> </a> NETN Lakes, Ponds, and Streams Visualizer</div>"),position = "static-top", inverse=TRUE, collapsible = FALSE, fluid=TRUE, 
           windowTitle = "NETN Lakes, Ponds, and Streams Visualizer", id="MainNavBar",
           
######################################### Time Series Panel ####################################################################
           
tabPanel(title="Plot time series",
         #style="padding: 0",
                    useShinyjs(),
         div(class="outer",
             #tags$head(includeCSS("./www/mapstyles.css") ), # defines css file
             tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />'))
              #puts up icon on tab
            #, tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))#,
         ),
         
fluidPage(
  sidebarPanel(
    h1("Plot time series from each sampled site."),
    br(),
    #Park selection
    tags$div(title="Choose the park you want to work with",selectInput(inputId='park', label='Select Park', choices= ParkNameList, selectize = TRUE)),
    
    #Location Type selection
    tags$div(title="Choose site type",radioButtons(inputId='loc', label='Select Stream or Lake/Pond', choices= c("Stream","Lake"), selected = "Stream")),
    
    # Site selection
    uiOutput("SiteResultsA"),
    
    # Parameter selection
#    tags$div(title="Choose the variable you want to plot",selectInput(inputId='parm', label='Select variable to plot', choices=  DisplayVarList)),
    uiOutput("VarResults"),
    
  ##Add in options for calculating trend
  tags$div(title="Calculate Trend",selectInput(inputId='trendType', 
                                               label='Select trend analysis method',
choices=c("None", "Theil-Sen (NOT deseasoned)", "Theil-Sen (deseasoned)", "Akritas-Theil-Sen (for censored data)"), selected = "None")),

    br(),

    #downloadButton('downloadData', 'Download Data'),
    #img(src = "BMI_sampling.jpg", height = 140, width = 180),
    br(),
    br(),
    p("For further information about this sampling protocol, visit the ", 
    a("NETN protocol page.", href= "https://science.nature.nps.gov/im/units/netn/monitor/programs/lakesPonds/lakesPonds.cfm")),
    br()
    ),
    
    mainPanel(plotOutput("plot", height = 400, width = 800), 
              
              tableOutput("modelout")
    
              
                        )
    #dygraphOutput("dygraph")
    
  )
  ), #end navbarPage

######################################### Sampling Effort Panel ####################################################################

tabPanel(title="Sampling Effort",
         #style="padding: 0",
         useShinyjs(),
         div(class="outer",
             tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />'))
               ),
         
         fluidPage(
           sidebarPanel(
             h2("Visualize sampling effort."),
             br(),
 
            #Location Type selection
             tags$div(title="Choose site type",radioButtons(inputId='locB', label='Select Stream or Lake/Pond', choices= c("Stream","Lake"), selected = "Stream")),
             
            #Variable selection
            uiOutput("ParmResultsB"),
             
             br(),
            
             br(),
             p("For further information about this sampling protocol, visit the ", 
               a("NETN protocol page.", href= "https://science.nature.nps.gov/im/units/netn/monitor/programs/lakesPonds/lakesPonds.cfm")),
             br()
           ),
           
           mainPanel(plotOutput("plot2",  width = "100%")
                     
           )
  
         )
)#end navbarPage




)
)