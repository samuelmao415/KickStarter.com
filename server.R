

function(input, output, session) {
  
  
  US_reactive <- reactive({
  ks18%>%filter(region == "United States") 
  })
  
  output$worldmap_plotID<-renderLeaflet({
    leaflet(bycountry) %>% 
      addProviderTiles("Esri.NatGeoWorldMap")%>%addCircleMarkers(~longitude,~latitude,
                                                                 radius=0.05*sqrt(bycountry$num),
                                    label=paste0(bycountry$region,' /n number of projects',bycountry$num),
                                   fillOpacity = 0.3)
  })
  
  
  output$US_plotID<- renderPlot({
    US_reactive()%>%filter(goal<input$goal_range_ID, state==input$state_ID)%>%
      ggplot(aes(x=goal))+geom_histogram(binwidth = input$binwidth_ID)
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
}