#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(xts)
library(dygraphs)
library(reshape2)
library(leaflet)
library(broom)
library(lubridate)
library(gridExtra)
library(grid)
library(sf)

#Load cleaned dataset, see "data_wrangle.R" for details on where data comes from
#and how it was cleaned. 
load('data/ShinyFullDataset.RData')

#Set a ggplot theme for aesthetic purposes
matt_theme <- theme_set(theme_bw())
matt_theme<- theme_update(axis.line = element_line(colour = "black"),
                          panel.grid.major=element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(),
                          text=element_text(family='sans','plain','black',16,0.5,0.5,0,0),
                          plot.margin=unit(c(6,20,6,2),'pt')
)

# 
shinyServer(function(input, output) {

# Generate leaflet (interactive map)
output$map <- renderLeaflet({
    leaflet() %>%
        addPolygons(data=catchment_us,color=catchment_us$color,popup=paste((catchment_us$id)),group='Catchments') %>%
        addPolygons(data=catchment_ds,color=catchment_ds$color,popup=paste((catchment_ds$id)),group='Catchments') %>%
        addMarkers(data=stations,popup=paste(stations$prettyname, "Sampling Site")) %>%
        addCircleMarkers(data=spill, popup=spill$name, color = "black") %>%
        addProviderTiles('Esri.WorldTopoMap',group='Topo') %>%
        addProviderTiles('Esri.WorldImagery',group='Imagery') %>%
        addLayersControl(baseGroups=c('Topo','Imagery'))
                         
    })

#Generate interactive and zoomable discharge plot for first tab of app
output$q <- renderDygraph({
    dygraph(q_xts, group='dy') %>%
        dyOptions(colors=c("blue","red"), strokeWidth=1.5) %>%
        dyAxis('y',label='Q (cms)', logscale = TRUE)
})

#Generate the plot with analyte concentration data. 
output$time.chem <- renderDygraph({
    d1 <- q_chem %>% 
        filter(variable == input$analyte) %>% 
        dcast(.,Datetime~prettyname,value.var='value',mean) 
    chem.dy <- xts(d1[,-1],order.by=d1$Datetime) %>%
        dygraph(.,group='dy')   %>%
        dyOptions(colors=c('red','blue'),strokeWidth=0, drawPoints=T,pointSize=4) %>%
        dyAxis('y',label='[Analyte] (mg/L)')
    if(!is.na(input$thresh)){
        chem.dy <- chem.dy %>% 
            dyLimit(limit=input$thresh,strokePattern='solid',color='green')
    }
    chem.dy
})

#Generate boxplot and probability density function of data. Dynamic by window set in dygraph group
#Add threshold line capacity
output$diff <- renderPlot({    dts <- numeric()
if(is.null(input$q_date_window)){
    dts <- c(min(q_chem$Datetime),max(q_chem$Datetime))
}else{
    dts[1] <- (as.Date(input$q_date_window[[1]]))
    dts[2] <- (as.Date(input$q_date_window[[2]]))
}
q.diff <- q_chem %>%
    filter(!is.na(value)) %>%
    filter(variable==input$analyte) #%>%
    #filter(Datetime > dts[1] & Datetime < dts[2])

box <- ggplot(q.diff,aes(x=prettyname,y=value,group=prettyname,fill=prettyname)) + 
    geom_boxplot(show.legend=F) +
    ylab('[Analyte] (mg/L)') +
    xlab(element_blank()) +
    scale_fill_manual(name='',values=c('red','blue')) 

if(!is.na(input$thresh)){
    box <- box + geom_hline(yintercept=input$thresh,col='green3',size=1.6)
}

dens <- ggplot(q.diff,aes(value,fill=site_no)) + 
    geom_density(alpha=.7,show.legend = F) + 
    ylab('Density') +
    xlab('[Analyte] (mg/L)') + 
    scale_fill_manual(name='',values=c('red','blue'))

if(!is.na(input$thresh)){
    dens <- dens + geom_vline(xintercept=input$thresh,col='green3',size=1.6)
}


grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2,widths=c(0.5,0.5))))
print(box, vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(dens, vp=viewport(layout.pos.row=1,layout.pos.col=2))

})



#end bracket below
})
