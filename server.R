

function(input, output, session) {
  
  
  US_reactive <- reactive({
  ks18%>%filter(region == "United States")%>%filter(goal<input$goal_range_ID, state==input$state_ID)
  })
  
  output$worldmap_plotID<-renderLeaflet({
    leaflet(bycountry) %>% 
      addProviderTiles("Esri.NatGeoWorldMap")%>%addCircleMarkers(~longitude,~latitude,
                                                                 radius=0.05*sqrt(bycountry$num),
                                    label=paste0(bycountry$region,' /n number of projects',bycountry$num),
                                   fillOpacity = 0.3)
  })
  
  
  output$US_plotID<- renderPlotly({
    plotdraft<-US_reactive()%>%
      ggplot(aes(x=goal))+geom_histogram(binwidth = input$binwidth_ID)
 ggplotly(plotdraft)})
  
  output$event<-renderPrint({
    d<-event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
}