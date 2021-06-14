shinyServer(function(input,output,session){
  set.seed=12345
  
  dataset <- eventReactive(input$load,{
    if(is.null(input$file)){
      
      df = read.csv(paste0("data\\",input$sample,".csv") ,header=TRUE, sep = ",", stringsAsFactors = F)
      return(df)
      
      }
    else{
      df = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
      #df <- df %>%drop_na()
      return(df)
    }
  })
  
  output$dim <- renderText({
    paste0("Uploaded data has ",nrow(dataset())," observations and ",ncol(dataset())," columns ")
  })

  
  cols <- reactive({colnames(dataset())})
  
  output$inp_var <- renderUI({
    
    pickerInput(
      inputId = "x",
      label = "Select X", 
      choices = cols(),
      options = list(
        `live-search` = FALSE,style = "btn-primary")
    )
    
    #selectInput("x","Select X",choices = cols())
  })
  
  output$tar_var <- renderUI({
    x <- match(input$x,cols())
    y_cols <- cols()[-x]
    #cols_y <- 
    pickerInput(
      inputId = "y",
      label = "Select Y", 
      choices = y_cols,
      options = list(
        `live-search` = FALSE,style = "btn-primary")
    )
   # selectInput("y","Select Y",choices = y_cols)
  })
  
  
  output$sampleData <- renderDataTable({
    head(dataset(),4)
  })
  
  output$y_dis <- renderPrint({
    req(dataset())
    prop.table(table(dataset()[,input$y]))
  })
  

 wc <- eventReactive(input$plotwc,{
    
     # if(is.null(input$file)){return(NULL)}
     # else{
          dfm <- build_dfm(dataset()[,input$x],
                           user_list = unlist(strsplit(input$stopw,",")),
                           remove_punct = input$remove_punct,
                           remove_numbers = input$remove_numbers,
                           remove_symbols = input$remove_symbols,
                           split_hyphens = input$split_hyphens,stem = input$stem,
                           lemma=input$lemma)%>%dfm_trim(input$minword)
          quanteda::textplot_wordcloud(dfm,color = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))
    #  }
  
    
  })          

  output$wordcloud <- renderPlot({wc()})
               
                  
  values <- reactiveValues(train_size=NULL)
  values <- reactiveValues(test_size=NULL)
   
  train_flag <-observeEvent(input$apply, {
     ask_confirmation(
      inputId = "myconfirmation1",
      type = "info",
      title = "Do you want to train ?",
      text = "Make sure correct variables are selected"
    )
  })
  
  
  list0 <- eventReactive(input$myconfirmation1, {
    if (isTRUE(input$myconfirmation1)){
      #df <- dataset()
      updateProgressBar(session = session, id = "pb6", value = 10)
      tab <- table(dataset()[,input$y])
      df <- dataset()[dataset()[,input$y] %in% names(tab)[tab>5],]
      
      # train test split
      train_test <- train_test_split(df,
                                     x_n0 = input$x,
                                     y_n0 = input$y,
                                     trg_propn = input$tr_per/100)
      
      train <- train_test[[1]]
      test <- train_test[[2]]
      
      
      
      updateProgressBar(session = session, id = "pb6", value = 30)
      
      # build dfm
      train.dfm <- build_dfm(train[,input$x],
                             user_list = unlist(strsplit(input$stopw,",")),
                             remove_punct = input$remove_punct,
                             remove_numbers = input$remove_numbers,
                             remove_symbols = input$remove_symbols,
                             split_hyphens = input$split_hyphens,stem = input$stem,
                             lemma=input$lemma)%>%dfm_trim(min_termfreq = 5)
      
      test.dfm <- build_dfm(test[,input$x],
                            user_list = unlist(strsplit(input$stopw,",")),
                            remove_punct = input$remove_punct,
                            remove_numbers = input$remove_numbers,
                            remove_symbols = input$remove_symbols,
                            split_hyphens = input$split_hyphens,stem = input$stem,
                            lemma=input$lemma)%>%dfm_trim(min_termfreq = 5)
      
      values[['train_size']] <- dim(train.dfm)
      values[['test_size']] <- dim(test.dfm)
      
      updateProgressBar(session = session, id = "pb6", value = 60)
      # train & evaluate
      l = train_and_evaluate_model(train.dfm,train[,input$y],test.dfm,test[,input$y],model='nb',n00=20)
      
      updateProgressBar(session = session, id = "pb6", value = 100)
      return(l)
      #list0 = reactive({
      # textclassif_nb(dataset(), # input file with text and Y colms
      #                y_n0=input$y,     # position of Y colm in the input DF 
      #                x_n0=input$x,     # position of X or text colm
      #                trg_propn = input$tr_per/100,   # default and slider for user input
      #                n00 = 100,
      #                stopw_list = unlist(strsplit(input$stopw,",")),
                    
                     
                     #model=input$algo
        # num_term coeffs to display for each class  
    }else{
      NULL
    }
    
    
    #})
    
  })
  
  
  output$cf_text <- renderText({
    paste0("Model is trained on ",values[['train_size']][1],' records and ',values[['train_size']][2], ' features.\n','Following results are based on testing  ',values[['test_size']][1],' unseen records')
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
      ggplot(confusion.data, aes(x=predicted_class, y=actual_class, fill=Freq)) +
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
  wordcloud:: wordcloud(words = t$tokens, freq = t$probability, 
              max.words=input$maxword, random.order=FALSE, rot.per=0.35, 
              colors= RColorBrewer::brewer.pal(8, "Dark2"))
    
  })
  
  
  
  output$token_table <- renderDataTable({
    print(input$y_token)
        t <- list0()[[2]][[input$y_token]]
        names(t) <- c("tokens","probability")
        t
        },options = list(
          pageLength=10,
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