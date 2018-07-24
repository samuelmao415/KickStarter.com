

function(input, output, session) {
  
  ####data for US tab with united stated selected#########################################
  US_reactive <- reactive({
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
    withProgress({
      setProgress(message = "Processing corpus...")
    plotdraft<-US_reactive()%>%
      #add a "no-filter" option to the filter using ifelse statement
      {if(input$state_ID!="All") filter(.,state==input$state_ID,
                                                                 goal<input$goal_range_ID) else 
                                                                   filter(.,goal<input$goal_range_ID)}%>%
      ggplot(aes(x=goal))+geom_histogram(binwidth = input$binwidth_ID)
 ggplotly(plotdraft)})})

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

  
  ##########main category observation#############################
  
  #plot histogram for each category given the state of the project and selected observation
  output$main_US_category_ID<- renderPlotly({
    withProgress({
      setProgress(message = "Processing plots...")
      
      main_plotdraft2<-US_reactive()%>%group_by(main_category)%>%{if(input$main_category_state_ID=="All") . else filter(.,state==input$main_category_state_ID)}%>%
        summarize(num=n())%>%filter(num>input$main_category_observation_ID)%>%
        ggplot(aes(x=main_category,y=num))+geom_bar(stat="identity")
      
      ggplotly(main_plotdraft2)})
    
  })
  #selected data table given state of the project and the category
  output$main_US_category_tableID <-DT::renderDataTable({
    main_datatable_category<-US_reactive()%>%group_by(main_category)%>%mutate(num=n())%>%
    {if(input$main_category_state_ID!="All") filter(.,state==input$main_category_state_ID,
                                               num>input$category_observation_ID) else  .}%>%
      select(ID,name,main_category,category,state,goal)
    
    DT::datatable(main_datatable_category)
    
   
  })
  

  ####main category and subcategory observations######################################################
  #plot histogram for each category given the state of the project and selected observation
  
  US_category_reactive<-reactive({US_reactive()%>%group_by(main_category)%>%mutate(num=n())})
  
  output$US_category_ID<- renderPlotly({
    withProgress({
      setProgress(message = "Large dataset. Please wait...")
    
  plotdraft2<-US_category_reactive()%>%{if(input$category_state_ID=="All") . else filter(.,state==input$category_state_ID)}%>%
    filter(num>input$category_observation_ID)%>%
    ggplot(aes(x=main_category,y=num,fill=category))+geom_bar(stat="identity")
  
  ggplotly(plotdraft2)})
  
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
      withProgress({
        setProgress(message = "Processing corpus...")
      
    
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
              colors=brewer.pal(8, "Dark2"))})
  })
  
  
  ###########################################################################################
  #################Rest of the world##########################################################
  ####data for rest of the world tab with ###############################################
  Rest_US_reactive <- reactive({
    ks18%>%filter(region != "United States")})
  

  
  #ggplotly output of goal distribution
  output$Rest_US_goal_ID<- renderPlotly({
    withProgress({
      setProgress(message = "Processing plots...")
    Rest_plotdraft<-Rest_US_reactive()%>%
      #add a "no-filter" option to the filter using ifelse statement for the state of the project
    {if(input$Rest_state_ID!="All") filter(.,state==input$Rest_state_ID,
                                      usd_goal_real<input$Rest_goal_range_ID) else 
                                        filter(.,usd_goal_real<input$Rest_goal_range_ID)}%>%
      #add a filter option to let user select countries
         {if(input$Rest_region_ID!="All") filter(.,region==input$Rest_region_ID)
     else .}%>%
      ggplot(aes(x=usd_goal_real))+geom_histogram(binwidth = input$Rest_binwidth_ID)
    ggplotly(Rest_plotdraft)})})
  
  #summary for goal given state of the project
  output$Rest_summary_ID <- renderPrint({
    Rest_dataset <- Rest_US_reactive()%>%
    {if(input$Rest_state_ID!="All") filter(.,state==input$Rest_state_ID)else .}%>%
      select("Summary information for the selected filter"=goal)
    summary(Rest_dataset)
  })
  #selected data table given state of the project and the goal
  output$Rest_US_tableID <-DT::renderDataTable({
    Rest_datatable_goal<-Rest_US_reactive()%>% {if(input$Rest_state_ID!="All") filter(.,state==input$Rest_state_ID,
                                                                       usd_goal_real<input$goal_range_ID) else 
                                                                         filter(.,usd_goal_real<input$goal_range_ID)}%>%
      select(ID,name,category,state,usd_goal_real,region)
    DT::datatable(Rest_datatable_goal)
  })

  
  ##########main category observation#############################
  
  #plot histogram for each category given the state of the project and selected observation
  output$Rest_main_US_category_ID<- renderPlotly({
    withProgress({
      setProgress(message = "Processing plots...")
      
      Rest_main_plotdraft2<-Rest_US_reactive()%>%group_by(main_category)%>%{if(input$Rest_main_category_state_ID=="All") . else filter(.,state==input$Rest_main_category_state_ID)}%>%
        summarize(num=n())%>%filter(num>input$Rest_main_category_observation_ID)%>%
        ggplot(aes(x=main_category,y=num))+geom_bar(stat="identity")
      
      ggplotly(Rest_main_plotdraft2)})
    
  })
  #selected data table given state of the project and the category
  output$Rest_main_US_category_tableID <-DT::renderDataTable({
    Rest_main_datatable_category<-Rest_US_reactive()%>%group_by(main_category)%>%mutate(num=n())%>%
    {if(input$Rest_main_category_state_ID!="All") filter(.,state==input$Rest_main_category_state_ID,
                                                    num>input$Rest_category_observation_ID) else  .}%>%
      select(ID,name,main_category,category,state,goal)
    
    DT::datatable(Rest_main_datatable_category)
    
    
  })
  
  
  ####main category and subcategory observations######################################################
  #plot histogram for each category given the state of the project and selected observation
  
  Rest_US_category_reactive<-reactive({Rest_US_reactive()%>%group_by(main_category)%>%mutate(num=n())})
  
  output$Rest_US_category_ID<- renderPlotly({
    withProgress({
      setProgress(message = "Large dataset. Please wait...")
      
      Rest_plotdraft2<-Rest_US_category_reactive()%>%{if(input$Rest_category_state_ID=="All") . else filter(.,state==input$Rest_category_state_ID)}%>%
        filter(num>input$Rest_category_observation_ID)%>%
        ggplot(aes(x=main_category,y=num,fill=category))+geom_bar(stat="identity")
      
      ggplotly(Rest_plotdraft2)})
    
  })
  
  #selected data table given state of the project and the category
  output$Rest_US_category_tableID <-DT::renderDataTable({
    Rest_datatable_category<-US_reactive()%>%group_by(main_category,category)%>%mutate(num=n())%>%
    {if(input$Rest_category_state_ID!="All") filter(.,state==input$Rest_category_state_ID,
                                               num>input$Rest_category_observation_ID) else  .}%>%
      select(ID,name,main_category,category,state,goal)
    
    DT::datatable(Rest_datatable_category)
  })
  
  
  #####Backers Analysis############################################################
  
  output$Rest_US_backers_ID<- renderPlotly({
    Rest_plotdraft3<-Rest_US_reactive()%>%
    {if(input$Rest_backers_state_ID=="All") . else filter(.,state==input$Rest_backers_state_ID)}%>%
      group_by(state)%>%
      summarize(number_of_backers=mean(backers))%>%
      ggplot(aes(reorder(x=state,-number_of_backers),y=number_of_backers))+
      geom_bar(stat="identity")
    ggplotly(Rest_plotdraft3)
  })
  
  
  ###almost made it project#########################################################
  output$Rest_US_almost_made_it_ID<-DT::renderDataTable({
    Rest_datatable_almost<-Rest_US_reactive()%>%mutate(prop=usd_pledged_real/usd_goal_real)%>%
      filter(prop<input$Rest_prop_max_ID, prop>input$Rest_prop_min_ID)%>%
      select(ID,name,category,state,goal,prop)
    
    DT::datatable(Rest_datatable_almost)
  })
  ###extremley successful project#########################################################
  output$Rest_US_successful_ID<-DT::renderDataTable({
    Rest_datatable_successful<-Rest_US_reactive()%>%mutate(prop=usd_pledged_real/usd_goal_real)%>%
      filter(prop>input$Rest_prop_success_max_ID)%>%
      select(ID,name,category,state,goal,prop)
    
    DT::datatable(Rest_datatable_successful)
  })
  
  #############word cloud################################################################
  
  output$Rest_word_cloud_plot_ID <- renderPlot({
    #showing a message
    
      withProgress({
        setProgress(message = "Processing corpus...")
      
    
    Rest_category_filter<-Rest_US_reactive()%>%filter(main_category==input$Rest_wordcloud_category_ID)
    Rest_testcloud<-paste(Rest_category_filter$name[0:400000], collapse='')
    
    Rest_docs <- Corpus(VectorSource(Rest_testcloud))
    
    # Convert the text to lower case
    Rest_docs <- tm_map(Rest_docs, content_transformer(tolower))
    # Remove numbers
    Rest_docs <- tm_map(Rest_docs, removeNumbers)
    # Remove english common stopwords
    Rest_docs <- tm_map(Rest_docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    Rest_docs <- tm_map(Rest_docs, removeWords, c("blabla1", "blabla2")) 
    # Remove punctuations
    Rest_docs <- tm_map(Rest_docs, removePunctuation)
    # Eliminate extra white spaces
    Rest_docs <- tm_map(Rest_docs, stripWhitespace)
    # Text stemming
    # docs <- tm_map(docs, stemDocument)
    
    ###############
    Rest_dtm <- TermDocumentMatrix(Rest_docs)
    Rest_m <- as.matrix(Rest_dtm)
    Rest_v <- sort(rowSums(Rest_m),decreasing=TRUE)
    Rest_d <- data.frame(word = names(v),freq=v)
    
    wordcloud(words = Rest_d$word, freq = Rest_d$freq, min.freq = input$Rest_freq_ID,
              max.words=input$Rest_max_ID, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))})
  })
  
  
  ####full dataset table###############################################################
  output$tableID <- DT::renderDataTable({
    DT::datatable(ks18)})


####################################################################
output$summary <- renderPrint({
  summary(cars)
})

  }
