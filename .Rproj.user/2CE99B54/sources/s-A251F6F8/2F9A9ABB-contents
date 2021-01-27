library(quanteda)
library(magrittr)
library(caret)
library(quanteda.textmodels)
library(shinyWidgets)
library(DT)

shinyUI(fluidPage(
  
  titlePanel(title=div(img(src="logo.png",align='right'),"Text Classification")),
  
  # Input in sidepanel:
  sidebarPanel(
    selectInput("sample","Load Sample Data",choices = sub('\\.csv$', '',list.files('data'))),
    p("OR"),
    fileInput("file", "Upload Data"),
    actionButton("load","Load Data"),
    
    
   # h5("select X"),
   hr(),
    uiOutput('inp_var'),
    
   # h5("select Y"),
   uiOutput('tar_var'),
    
   textInput("stopw", ("Enter stop words separated by comma(,)"), value = "will,can"),
   checkboxInput("rem_punct","Remove Punctuation",value = TRUE),
   # h5("select training data (in percentage)"),
    sliderInput("tr_per",label = "select training data (in percentage)",min = 0,max = 100,value = 70,step = 1),
   
    hr(),
    #h5("select classification algorithm"),
    selectInput("algo",label = "select algorithm",choices = c("Naive Bayes"="nb","Logistic Regression"="lr")),
    actionButton('plotwc',"Plot WordCloud"),
    actionButton("apply","Train Model")
  
  ),
  
  # Main Panel:
  mainPanel( 
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview & Example Dataset",h4(p("How to use this App")),
                         
                         p("To use this app you need a document corpus in csv file format. Make sure each document is separated from another document with a new line character.
                          Dataset should contain atleast two columns text (used as input) and targert (used for predictions).
                           To do basic Text classfication on your text corpus, click on Browse in left-sidebar panel and upload the file. Once the file is uploaded it will do the computations in 
                            back-end with default inputs and accordingly results will be displayed in various tabs.", align = "justify"),
                         p("If you wish to change the input, modify the input in left side-bar panel and click on Apply changes. Accordingly results in other tab will be refreshed
                           ", align = "Justify"),
                         h5("Note"),
                         p("You might observe no change in the outputs after clicking 'Apply Changes'. Wait for few seconds. As soon as all the computations
                           are over in back-end results will be refreshed",
                           align = "justify"),
                         #, height = 280, width = 400
                         br(),
                         h4(p("Download Sample file")),
                         selectInput("downloads","Select Sample file to download",choices = sub('\\.csv$', '',list.files('data'))),
                         downloadButton('downloadData1', 'Download'),br(),br(),
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         img(src = "example1.png")
                )
                ,
               
                tabPanel("Data",
                         h4("Review uploaded data"),
                         DT::dataTableOutput("sample_data"),br(), 
                         h4("Word Cloud"),
                         dropdownButton(
                           
                           tags$h3("List of Inputs"),
                           
                          sliderInput(inputId = 'minword',
                                       label = 'Min Term Frequency',
                                       value = 10,
                                       min = 1,
                                       max = 50),
                           
                           circle = TRUE, status = "danger",
                           icon = icon("gear"), width = "300px",
                           
                           tooltip = tooltipOptions(title = "Click to see inputs !")
                         ),
                         
                         plotOutput("wordcloud",height = 700, width = 700),br(),
                         #textInput("in",label = "text"),
                         
                        ),
                        tabPanel("Training Report",
                                 h4("confusion matrix"),
                                 plotOutput("cf_matrix"),
                                 hr(),
                                 verbatimTextOutput('cf'),
                                 hr(),
                                 uiOutput("tokens"),
                                 
                                 dropdownButton(
                                   
                                   tags$h3("List of Inputs"),
                                   
                                   sliderInput(inputId = 'maxword',
                                               label = 'Max words to display',
                                               value = 20,
                                               min = 1,
                                               max = 100),
                                   
                                   circle = TRUE, status = "danger",
                                   icon = icon("gear"), width = "300px",
                                   
                                   tooltip = tooltipOptions(title = "Click to see inputs !")
                                 ),
                                 
                                 plotOutput("token_wc",height = 700, width = 700),br(),
                                 
                                 
                                  dataTableOutput("token_table")
                                 
                                )
    ),
    
    )
  )
  )



