shinyServer(function(input,output,session){
  set.seed=12345
  dataset <- reactive({
    if(is.null(input$file)){return(NULL)}
    else{
      df = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
    }
  })
  
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
  
  

             
  output$wordcloud <- renderPlot({
    if(is.null(input$file)){return(NULL)}
    else{
      dfm <- corpus(dataset()[,input$x]) %>% 
        dfm(remove = stopwords('english'), remove_punct = TRUE) %>%
        dfm_trim(min_termfreq = input$minword, verbose = FALSE)
      
      textplot_wordcloud(dfm,color = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))
    }
    
  })
  
               
                  
        
   

  
  
  list0 <- eventReactive(input$apply, {
    #list0 = reactive({
    textclassif_nb(dataset(), # input file with text and Y colms
                   y_n0=input$y,     # position of Y colm in the input DF 
                   x_n0=input$x,     # position of X or text colm
                   trg_propn = input$tr_per/100,   # default and slider for user input
                   n00 = 100
                   #model=input$algo
    )   # num_term coeffs to display for each class  
    
    #})
    
  })
  
  output$cf_matrix <- renderPrint({print(list0()[[1]])})
  
  output$tokens <- renderUI({selectInput("y_token","select class for top 20 tokens",choices = as.character(sort(unique(dataset()[,input$y]))))})
  
  output$token_table <- renderDataTable({
    print(as.numeric(input$y_token))
        list0()[[2]][[as.numeric(input$y_token)]]})
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