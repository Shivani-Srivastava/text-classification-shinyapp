shinyServer(function(input,output,session){
  set.seed=12345
  dataset <- eventReactive(input$load,{
    if(is.null(input$file)){
      
      df = read.csv(paste0("data\\",input$sample,".csv") ,header=TRUE, sep = ",", stringsAsFactors = F)
      return(df)
      
      }
    else{
      df = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
      
      return(df)
    }
  })
  
  # dataset <- eventReactive(input$load,{
  #   if(is.null(input$file)){
  # 
  #   
  #     }
  # }
  # 
  # )
  
  cols <- reactive({colnames(dataset())})
  
  output$inp_var <- renderUI({
    selectInput("x","Select X",choices = cols())
  })
  
  output$tar_var <- renderUI({
    
    x <- match(input$x,cols())
    y_cols <- cols()[-x]
    
    cols_y <- 
    selectInput("y","Select Y",choices = y_cols)
  })
  
  
  output$sample_data <- renderDataTable({
    head(dataset(),5)
  })
  
  

 wc <-  eventReactive(input$plotwc,{
    
      if(is.null(input$file)){return(NULL)}
      else{
        dfm <- corpus(dataset()[,input$x]) %>% 
          dfm(remove = c(stopwords('english'),unlist(strsplit(input$stopw,","))), remove_punct = input$rem_punct) %>%
          dfm_trim(min_termfreq = input$minword, verbose = FALSE)
          textplot_wordcloud(dfm,color = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))
      }
  
    
  })          

  output$wordcloud <- renderPlot({wc()})
               
                  
        
   
  train_flag <-observeEvent(input$apply, {
     ask_confirmation(
      inputId = "myconfirmation1",
      type = "info",
      title = "Do you want to train ?",
      text = "Make sure correct variables are selected"
    )
  })
  
  
  list0 <- eventReactive(input$myconfirmation1, {
    if (isTRUE(input$myconfirmation1) & !is.null(input$file)){
      #list0 = reactive({
      textclassif_nb(dataset(), # input file with text and Y colms
                     y_n0=input$y,     # position of Y colm in the input DF 
                     x_n0=input$x,     # position of X or text colm
                     trg_propn = input$tr_per/100,   # default and slider for user input
                     n00 = 100,
                     user_stpw = unlist(strsplit(input$stopw,",")),
                     rem_punct = input$rem_punct
                     
                     #model=input$algo
      )   # num_term coeffs to display for each class  
    }else{
      NULL
    }
    
    
    #})
    
  })
  
  output$cf <- renderPrint({
    if(!isTRUE(input$myconfirmation1)){return(NULL)}
    else{
      print(list0()[[1]])
    }
  })
  
  output$cf_matrix <- renderPlot({
    if(input$apply==0){return(NULL)}
    else{
      # print(input$apply)
      # print(list0()[[1]])
      
      # Save confusion matrix as data frame
     # print(list0()[[1]][1])
     # print(list0()[[1]][['table']])
      confusion.data <- as.data.frame(list0()[[1]][['table']])
      ggplot(confusion.data, aes(x=Var2, y=Var1, fill=Freq)) +
        geom_tile() + theme_bw() + coord_equal() +
        scale_fill_distiller(palette="Greens", direction=1) +
        guides(fill=F) + # removing legend for `fill`
        labs(title = "Confusion Matrix") + # using a title instead
        geom_text(aes(label=Freq), color="black")+ylab("Actual")+xlab("Predicted") # printing values
      
      
      
    }
   })
  
  output$tokens <- renderUI({selectInput("y_token","select class for top tokens",choices = names(list0()[[2]]))})
  
  output$token_wc <- renderPlot({
    t <- list0()[[2]][[input$y_token]]
    names(t) <- c("tokens","probability")
    wordcloud(words = t$tokens, freq = t$probability, 
              max.words=input$maxword, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
  })
  
  
  
  output$token_table <- renderDataTable({
    print(input$y_token)
        t <- list0()[[2]][[input$y_token]]
        names(t) <- c("tokens","probability")
        t
        },options = list(
          autoWidth = TRUE,
          columnDefs = list(list(width = '200px', targets = "_all"))
        ))
  
  
  
  output$downloadData1 <- downloadHandler(
    filename = function() { paste0(input$downloads,".csv") },
    content = function(file) {
      writeLines(readLines(paste0("data\\",input$downloads,".csv")), file)
    }
  )
  # output$wordcloud <- renderPlot({
  #   if(is.null(input$file)){return(NULL)}
  #   else{
  #     dfm <- corpus(dataset()[,input$x]) %>% 
  #       dfm(remove = stopwords('english'), remove_punct = TRUE) %>%
  #       dfm_trim(min_termfreq = input$minword, verbose = FALSE)
  #     
  #     textplot_wordcloud(dfm,color = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))
  #   }
  #   
  # })
  
  
 
   
    # output$token_table <- renderDataTable({
    #   table_num <- as.numeric(input$y_token)
    #   list0()[[2]][]
    #   
    #   
    #   })
    # 
    
    
    
})