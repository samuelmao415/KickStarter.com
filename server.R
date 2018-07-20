

function(input, output, session) {
  
  ####data for US tab with united stated selected
  US_reactive <- reactive({
  ks18%>%filter(region == "United States")})
  
  ####Leaflet output of world map
  output$worldmap_plotID<-renderLeaflet({
    leaflet(bycountry) %>% 
      addProviderTiles("Esri.NatGeoWorldMap")%>%addCircleMarkers(~longitude,~latitude,
                                                                 radius=0.05*sqrt(bycountry$num),
                                    label=paste0(bycountry$region,' /n number of projects',bycountry$num),
                                   fillOpacity = 0.3)
  })
  
  #ggplotly output of goal distribution
  output$US_goal_ID<- renderPlotly({
    plotdraft<-US_reactive()%>%
      #add a "no-filter" option to the filter using ifelse statement
      {if(input$state_ID!="All") filter(.,state==input$state_ID,
                                                                 goal<input$goal_range_ID) else 
                                                                   filter(.,goal<input$goal_range_ID)}%>%
      ggplot(aes(x=goal))+geom_histogram(binwidth = input$binwidth_ID)
 ggplotly(plotdraft)})

  #summary for goal
  output$summary_ID <- renderPrint({
    dataset <- US_reactive()%>%select(goal)
    summary(dataset)
  })
  

  ####category observations
  output$US_category_ID<- renderPlotly({
  plotdraft2<-US_reactive()%>%group_by(category)%>%summarize(num=n())%>%
    filter(num>input$category_observation_ID)%>%
    ggplot(aes(reorder(x=category,-num),y=num))+
    geom_bar(stat="identity")
  ggplotly(plotdraft2)
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$tableID <- DT::renderDataTable({
    DT::datatable(ks18)
  })
}