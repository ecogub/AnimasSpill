#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load libraries
library(shiny)
library(leaflet)
library(dygraphs)
vars <-
    sort(c(
        'Arsenic', 'Turbidity', 'Dissolved oxygen (DO)', 'Mercury', 'Lead', 'Aluminum'
    )) 

# Define UI for application that draws a histogram
shinyUI(fluidPage(

# Application title
titlePanel("Gold King Mine Spill"),

# Sidebar with a slider input for number of bins
sidebarLayout(
sidebarPanel(
p("The wastewater of the Gold King Mine spilled into Cement Creek, just upstream of Silverton, on August 5th, 2015. Cement Creek is one of many tributaries to the greater Animas River. 
  In this app, you can explore the effects of the spill on water quality around Silverton. 
  Select an analyte (i.e. a target for analysis) to begin. 
  Discharge data is sourced from USGS gages 09358550 and 09359020. 
  Water quality data was collected by the EPA at sites A68 and A72, which are colocated to the USGS gages."),
leafletOutput('map',height=350) #Sets dimensions of leaflet map
),
mainPanel(
         fluidRow(column(4,
                         selectInput('analyte',label='Select a water quality analyte',
                                     choices=vars)),
                  column(4,
                         p('Look up drinking water thresholds',
                           
                           a('Primary',href='https://www.epa.gov/ground-water-and-drinking-water/national-primary-drinking-water-regulations'),
                           'or',
                           a('Secondary',href='https://www.epa.gov/dwstandardsregulations/secondary-drinking-water-standards-guidance-nuisance-chemicals')
                         )
                  ),
                  column(4,
                         numericInput('thresh',label='Enter a Regulation Threshold',value=NA))
         ),
         plotOutput('diff',height='160px'),
         h4('Analyte Concentration'),
         dygraphOutput('time.chem',height='200px'),
         h4('Daily Mean Discharge'),
         dygraphOutput('q',height='200px')
)
            )
        )
    )

