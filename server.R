

function(input, output, session) {
  
  ####data for US tab with united stated selected#########################################
  US_reactive <- reactive({
    isolate({
      withProgress({
        setProgress(message = "Processing my plot for you...")
      })})
    
  ks18%>%filter(region == "United States")})
  
  ####Leaflet output of world map#######################################################
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

  #summary for goal given state of the project
  output$summary_ID <- renderPrint({
    dataset <- US_reactive()%>%
      {if(input$state_ID!="All") filter(.,state==input$state_ID)else .}%>%
      select("Summary information for the selected filter"=goal)
    summary(dataset)
  })
  #selected data table given state of the project and the goal
  output$US_tableID <-DT::renderDataTable({
    datatable_goal<-US_reactive()%>% {if(input$state_ID!="All") filter(.,state==input$state_ID,
                        goal<input$goal_range_ID) else 
                       filter(.,goal<input$goal_range_ID)}%>%
      select(ID,name,category,state,goal)
    DT::datatable(datatable_goal)
  })
#####################################################

  ####category observations######################################################
  #plot histogram for each category given the state of the project and selected observation
  output$US_category_ID<- renderPlotly({
    
    
  plotdraft2<-US_reactive()%>%{if(input$category_state_ID=="All") . else filter(.,state==input$category_state_ID)}%>%
    group_by(main_category,category)%>%summarize(num=n())%>%
    filter(num>input$category_observation_ID)%>%
    ggplot(aes(x=reorder(main_category,-num),y=num, fill=category))+geom_bar(stat="identity")
  
  ggplotly(plotdraft2)
  
  })
  
  #selected data table given state of the project and the category
  output$US_category_tableID <-DT::renderDataTable({
    datatable_category<-US_reactive()%>%group_by(main_category,category)%>%mutate(num=n())%>%
      {if(input$category_state_ID!="All") filter(.,state==input$category_state_ID,
                                                                num>input$category_observation_ID) else  .}%>%
      select(ID,name,main_category,category,state,goal)
    
    DT::datatable(datatable_category)
  })
  
  #####Backers Analysis############################################################
  
  output$US_backers_ID<- renderPlotly({
    plotdraft3<-US_reactive()%>%
    {if(input$backers_state_ID=="All") . else filter(.,state==input$backers_state_ID)}%>%
      group_by(state)%>%
      summarize(number_of_backers=mean(backers))%>%
      ggplot(aes(reorder(x=state,-number_of_backers),y=number_of_backers))+
      geom_bar(stat="identity")
    ggplotly(plotdraft3)
  })
  
  
  ###almost made it project#########################################################
  output$US_almost_made_it_ID<-DT::renderDataTable({
  datatable_almost<-US_reactive()%>%mutate(prop=pledged/goal)%>%
    filter(prop<input$prop_max_ID, prop>input$prop_min_ID)%>%
    select(ID,name,category,state,goal,prop)
  
  DT::datatable(datatable_almost)
  })
  ###extremley successful project#########################################################
  output$US_successful_ID<-DT::renderDataTable({
    datatable_successful<-US_reactive()%>%mutate(prop=pledged/goal)%>%
      filter(prop>input$prop_success_max_ID)%>%
      select(ID,name,category,state,goal,prop)
    
    DT::datatable(datatable_successful)
  })
  
  #############word cloud################################################################
 
  output$word_cloud_plot_ID <- renderPlot({
    #showing a message
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
      })})
    
    category_filter<-US_reactive()%>%filter(main_category==input$wordcloud_category_ID)
    testcloud<-paste(category_filter$name[0:400000], collapse='')
    
    docs <- Corpus(VectorSource(testcloud))
    
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    # Text stemming
    # docs <- tm_map(docs, stemDocument)
    
    ###############
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)

    wordcloud(words = d$word, freq = d$freq, min.freq = input$freq_ID,
              max.words=input$max_ID, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  
  #####################################################################
  output$summary <- renderPrint({
    summary(cars)
  })
  
  ####full dataset table###############################################################
  output$tableID <- DT::renderDataTable({
    DT::datatable(ks18)
  })
}
