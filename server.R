

function(input, output, session) {
  
  
  
  ####Leaflet output of world map#######################################################
  output$worldmap_plotID<-renderLeaflet({
    leaflet(bycountry) %>% 
      addProviderTiles("Esri.NatGeoWorldMap")%>%addCircleMarkers(~longitude,~latitude,
                                                                 radius=0.075*sqrt(bycountry$num),
                                    label=paste0(bycountry$region,': ',bycountry$num, ' projects'),
                                   fillOpacity = 0.3, color = '#D55E00')
  })
  output$pie_plotID<-renderPlotly({
    plot_ly(piedata2, labels = ~region, values = ~observation, type = 'pie', marker=list(colors = c('rgb(114,147,203)', 'rgb(211,94,96)'))) %>%
      layout(title = 'United States vs Rest of the World',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
   
  })
  
  
  
  ####data for US tab with united stated selected#########################################
  US_reactive <- reactive({
    ks18%>%filter(region == "United States")})
  
  #ggplotly output of goal distribution
  output$US_goal_ID<- renderPlotly({
    withProgress({
      setProgress(message = "Processing plots...")
    plotdraft<-US_reactive()%>%
      #add a "no-filter" option to the filter using ifelse statement
      {if(input$state_ID!="All") filter(.,state==input$state_ID,
                     goal<input$goal_range_ID,goal>input$min_goal_range_ID) else 
                        filter(.,goal<input$goal_range_ID,goal>input$min_goal_range_ID)}%>%
#filter category
   {if(input$category_under_goal_ID!="All") filter(.,main_category==input$category_under_goal_ID)
         else 
                (.)}%>%
      ggplot(aes(x=goal))+geom_histogram(binwidth = input$binwidth_ID)+
      labs(x="Project goal",y="Number of project",title="Number of projects of desired funding target")
 ggplotly(plotdraft)})})

  #summary for goal given state of the project
  output$summary_ID <- renderPrint({
    dataset <- US_reactive()%>%
      {if(input$state_ID!="All") filter(.,state==input$state_ID)else .}%>%
      {if(input$category_under_goal_ID!="All") filter(.,main_category==input$category_under_goal_ID)
        else 
          (.)}%>%
      select("Summary of project funding"=goal)
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

  
  ####main category and subcategory observations############
  #plot histogram for each category given the state of the project and selected observation
  
  
  output$US_category_ID<- renderPlotly({
    withProgress({
      setProgress(message = "Large dataset. Please wait...")
    
  plotdraft2<-US_reactive()%>%{if(input$category_state_ID=="All") . else filter(.,state==input$category_state_ID)}%>%
    group_by(main_category)%>%mutate(totalprojects=n())%>%
    ggplot(aes(x=main_category,fill=category))+
    geom_bar(aes(text=paste("Subcategory:", category, totalprojects)))+
    theme(legend.position="none")+
    labs(x="Project Category",y="Number of project")
  
  ggplotly(plotdraft2,tooltip="text")})
  
  })
  
  #selected data table given state of the project and the category
  output$US_category_tableID <-DT::renderDataTable({
    datatable_category<-US_reactive()%>%
      {if(input$category_state_ID!="All") filter(.,state==input$category_state_ID) else  .}%>%
      select(ID,name,main_category,category,state,goal)
    
    DT::datatable(datatable_category)
  })
  
  ###########sucessful rate analysis by category#################################
  output$US_success_rate_ID<-renderPlotly({
    successrate_plot<-withoutlive%>%
      ggplot(aes(reorder(x=main_category,-rate),y=rate))+
      geom_col(aes(text=paste("Successful rate", rate)))+labs(title="Successful project vs All project",
    subtitle="Points on the left indicate a category with an above 0.5 succesful rate",x="Category",y="Succesful rate")+
    geom_hline(aes(yintercept = mean(withoutlive$rate)))
    ggplotly(successrate_plot,tooltip="text")
  })
  
  ###success analysis with scatter plot######################################
  output$US_scatterplot_ID<-renderPlotly({
    scatter_plot<-successful_scatter%>%
      ggplot(aes(x=projects,y=successful_projects))+
      geom_point(aes(fill=main_category),size=3)+geom_abline(slope=0.5)+
      labs(x="Total Number of projects",y="Number of successful projects")
    ggplotly(scatter_plot)
  })
    
  
  #####Backers Analysis############################################################
  
  output$US_backers_ID<- renderPlotly({
    plotdraft3<-US_reactive()%>%
      group_by(state)%>%
      summarize(average_number_of_backers=mean(backers))%>%
      ggplot(aes(reorder(x=state,-average_number_of_backers),y=average_number_of_backers))+
      geom_bar(stat="identity", aes(text=paste("Backers:\n", average_number_of_backers)))+labs(x="Average number of backers",y="Outcome of the project")
    ggplotly(plotdraft3,tooltip="text")
  })
  
  output$US_backers_by_category_ID<- renderPlotly({
    plotdraft4<-US_reactive()%>%
      group_by(main_category)%>%filter(state == "successful")%>%
      summarize(average_number_of_backers=mean(backers))%>%
      ggplot(aes(reorder(x=main_category,-average_number_of_backers),y=average_number_of_backers))+
      geom_bar(stat="identity",aes(text=paste("Backers:\n", average_number_of_backers)))+
      labs(y="Average number of backers",x="Outcome of the project")
  
    ggplotly(plotdraft4,tooltip="text")
  })
  
  
  
  ###almost made it project#########################################################
  output$US_almost_made_it_ID<-DT::renderDataTable({
  datatable_almost<-US_reactive()%>%mutate(percent_funded=pledged/goal*100)%>%
    filter(percent_funded<input$prop_max_ID, percent_funded>input$prop_min_ID)%>%
    select(ID,name,category,state,goal,percent_funded)
  
  DT::datatable(datatable_almost)
  })
  ###extremley successful project#########################################################
  output$US_successful_ID<-DT::renderDataTable({
    datatable_successful<-US_reactive()%>%mutate(over_funded=pledged/goal*100)%>%
      filter(over_funded>input$prop_success_max_ID)%>%
      {if(input$sucessful_showone_ID!="Show all") filter(.,goal>100)
        else .}%>%filter(state=="successful")%>%
      select(ID,name,category,state,goal,over_funded)
    
    DT::datatable(datatable_successful)
  })
  
  #############word cloud################################################################
 
  output$word_cloud_plot_ID <- renderPlot({
    #showing a message
      withProgress({
        setProgress(message = "Processing corpus...")
      
    
    category_filter<-US_reactive()%>%filter(main_category==input$wordcloud_category_ID)%>%
    {if(input$wordcloud_state_ID!="All") filter(.,state==input$wordcloud_state_ID) else  .}
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
    set.seed(1)

    wordcloud(words = d$word, freq = d$freq, min.freq = input$freq_ID,
              max.words=input$max_ID, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"),font=2,family = "serif", height=900)})
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
                                      usd_goal_real<input$Rest_goal_range_ID,usd_goal_real>input$Rest_min_goal_range_ID) else 
                                        filter(.,usd_goal_real<input$Rest_goal_range_ID,usd_goal_real>input$Rest_min_goal_range_ID)}%>%
      #add a filter option to let user select countries
         {if(input$Rest_region_ID!="All") filter(.,region==input$Rest_region_ID)
     else .}%>%
      #filter category
     {if(input$Rest_category_under_goal_ID!="All") filter(.,main_category==input$Rest_category_under_goal_ID)
       else 
         (.)}%>%
      ggplot(aes(x=usd_goal_real))+geom_histogram(binwidth = input$Rest_binwidth_ID)+
    labs(x="Project goal",y="Number of project",title="Number of projects of desired funding target")
    ggplotly(Rest_plotdraft)})})
  
  #summary for goal given state of the project
  output$Rest_summary_ID <- renderPrint({
    Rest_dataset <- Rest_US_reactive()%>%
    {if(input$Rest_state_ID!="All") filter(.,state==input$Rest_state_ID)else .}%>%
    {if(input$Rest_region_ID!="All") filter(.,region==input$Rest_region_ID) else .}%>%
      select("Summary of project funding"=usd_goal_real)
    summary(Rest_dataset)
  })
  #selected data table given state of the project and the goal
  output$Rest_US_tableID <-DT::renderDataTable({
    Rest_datatable_goal<-Rest_US_reactive()%>% {if(input$Rest_state_ID!="All") filter(.,state==input$Rest_state_ID,
                                                                       usd_goal_real<input$Rest_goal_range_ID, usd_goal_real>input$Rest_min_goal_range_ID) else 
                                                                         filter(.,usd_goal_real<input$Rest_goal_range_ID,usd_goal_real>input$Rest_min_goal_range_ID)}%>%
    {if(input$Rest_region_ID!="All") filter(.,region==input$Rest_region_ID) else .}%>%
    {if(input$Rest_category_under_goal_ID!="All") filter(.,main_category==input$Rest_category_under_goal_ID)
      else 
        (.)}%>%
      select(ID,name,category,state,usd_goal_real,region)
    DT::datatable(Rest_datatable_goal)
  })

  
  ##########main category observation#############################
  
  #plot histogram for each category given the state of the project and selected observation
  output$Rest_main_US_category_ID<- renderPlotly({
    withProgress({
      setProgress(message = "Processing plots...")
      
      Rest_main_plotdraft2<-Rest_US_reactive()%>%{if(input$Rest_main_category_state_ID=="All") . else filter(.,state==input$Rest_main_category_state_ID)}%>%
      {if(input$Rest_main_category_region_ID!="All") filter(.,region==input$Rest_main_category_region_ID)
        else .}%>%group_by(main_category,category)%>%
        ggplot(aes(x=main_category,fill=category))+
        geom_bar(aes(text=paste("Category:", main_category, "\nSubcategory:", category)))+
        theme(legend.position="none")+
        labs(x="Project Category",y="Number of project")
      
      ggplotly(Rest_main_plotdraft2,tooltip="text")})
    
  })
  #selected data table given state of the project and the category
  output$Rest_main_US_category_tableID <-DT::renderDataTable({
    Rest_main_datatable_category<-Rest_US_reactive()%>%
    {if(input$Rest_main_category_state_ID!="All") filter(.,state==input$Rest_main_category_state_ID) else  .}%>%
      {if(input$Rest_main_category_region_ID!="All") filter(.,region==input$Rest_main_category_region_ID)
                                                      else .}%>%
      select(ID,name,main_category,category,state,usd_goal_real,region)
    
    DT::datatable(Rest_main_datatable_category)
    
  })
  
  
  ###########sucessful rate analysis by category#################################
  output$Rest_US_success_rate_ID<-renderPlotly({
    Rest_successrate<-Rest_US_reactive()%>%filter(state!="live")%>%
    {if(input$Rest_success_rate_ID!="All") filter(.,region==input$Rest_success_rate_ID)
      else .}%>%
      group_by(main_category)%>%summarize(rate=sum(state=="successful")/n())
    Rest_successrate_plot<-Rest_successrate%>%ggplot(aes(reorder(x=main_category,-rate),y=rate))+
      geom_col(aes(text=paste("Successful rate", rate)))+labs(title="Successful project vs All project",
                                                              subtitle="Points on the left indicate a category with an above 0.5 succesful rate",x="Category",y="Succesful rate")+
      geom_hline(yintercept = mean(Rest_successrate$rate))
    ggplotly(Rest_successrate_plot,tooltip="text")
  })
  
  ###success analysis with scatter plot######################################
  output$Rest_US_scatterplot_ID<-renderPlotly({
    Rest_successful_scatter<-Rest_US_reactive()%>%filter(state!="live")%>%
    {if(input$Rest_scatter_ID!="All") filter(.,region==input$Rest_scatter_ID)
      else .}%>%
      group_by(main_category)%>%summarize(projects=n(),successful_projects=sum(state=="successful"))

     Rest_scatter_plot<-Rest_successful_scatter%>%
      ggplot(aes(x=projects,y=successful_projects))+
      geom_point(aes(fill=main_category),size=3)+geom_abline(slope=0.5)+
      labs(x="Total Number of projects",y="Number of successful projects")
    ggplotly(Rest_scatter_plot)
  })
  
  

  #####Backers Analysis############################################################
  
  output$Rest_US_backers_ID<- renderPlotly({
    Rest_plotdraft3<-Rest_US_reactive()%>%
      group_by(state)%>%
      {if(input$Rest_backers_region_ID!="All") filter(.,region==input$Rest_backers_region_ID)
        else .}%>%
      summarize(average_number_of_backers=mean(backers))%>%
      ggplot(aes(reorder(x=state,-average_number_of_backers),y=average_number_of_backers))+
      geom_bar(stat="identity", aes(text=paste("Backers:\n", average_number_of_backers)))+
      labs(y="Average number of backers",x="Outcome of the project")
    ggplotly(Rest_plotdraft3,tooltip="text")
  })

  output$Rest_backers_by_category_ID<- renderPlotly({
    Rest_plotdraft4<-Rest_US_reactive()%>% group_by(main_category)%>%
    {if(input$Rest_backers_category_region_ID!="All") filter(.,region==input$Rest_backers_category_region_ID)
      else .}%>%
  filter(state == "successful")%>%
      summarize(average_number_of_backers=mean(backers))%>%
      ggplot(aes(reorder(x=main_category,-average_number_of_backers),y=average_number_of_backers))+
      geom_bar(stat="identity",aes(text=paste("Backers:\n", average_number_of_backers)))+
      labs(y="Average number of backers",x="Outcome of the project")
    
    
    ggplotly(Rest_plotdraft4,tooltip="text")
  })
  
  
  
  
  
  ###almost made it project#########################################################
  output$Rest_US_almost_made_it_ID<-DT::renderDataTable({
    Rest_datatable_almost<-Rest_US_reactive()%>%mutate(percent_funded=usd_pledged_real/usd_goal_real*100)%>%
      filter(percent_funded<input$Rest_prop_max_ID, percent_funded>input$Rest_prop_min_ID)%>%
      select(ID,name,category,state,usd_goal_real,region,percent_funded)
    
    DT::datatable(Rest_datatable_almost)
  })
  ###extremley successful project#########################################################
  
  output$Rest_US_successful_ID<-DT::renderDataTable({
    Rest_datatable_successful<-Rest_US_reactive()%>%mutate(over_funded=usd_pledged_real/usd_goal_real*100)%>%
      filter(over_funded>input$Rest_prop_success_max_ID)%>%
      {if(input$Rest_sucessful_showone_ID!="Show all") filter(.,usd_goal_real>100)
        else .}%>%filter(state=="successful")%>%
      select(ID,name,category,state,usd_goal_real,region,over_funded)
    
    DT::datatable(Rest_datatable_successful)
  })
  
  
  #############word cloud################################################################
  
  output$Rest_word_cloud_plot_ID <- renderPlot({
    #showing a message
    
      withProgress({
        setProgress(message = "Processing corpus...")
      
    
    Rest_category_filter<-Rest_US_reactive()%>%filter(main_category==input$Rest_wordcloud_category_ID)%>%
      {if(input$Rest_word_cloud_region_ID!="All") filter(.,region==input$Rest_word_cloud_region_ID)
        else .}%>% {if(input$Rest_wordcloud_state_ID!="All") filter(.,state==input$Rest_wordcloud_state_ID) else  .}
      
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
    
    ###############
    Rest_dtm <- TermDocumentMatrix(Rest_docs)
    Rest_m <- as.matrix(Rest_dtm)
    Rest_v <- sort(rowSums(Rest_m),decreasing=TRUE)
    Rest_d <- data.frame(word = names(Rest_v),freq=Rest_v)
    set.seed(1)
    
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
  
  getPage<-function() {
    return(includeHTML("Untitleddocument.html"))
  }
  output$inc<-renderUI({getPage()})

  }
