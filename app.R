#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(markdown)
library(shinythemes)
library(ggplot2)
library(shiny)
library(dplyr)
library(shinydashboardPlus)
library(shinydashboard)
library(psych)
library(dashboardthemes)
library(ggplot2)
library(devtools)
library(rsconnect)
library(data.table)
library(psych)
library(lmtest)
library(car)
library(corrplot)
library(olsrr)
rsconnect::setAccountInfo(name='arindacintalestari', token='CDFDAB4723EBB8FFAB54D23B529C9D50', 
                          secret='0z8AWDvg7aizV6j7cQdbIQs6iE/b1E3DdUqNl+jC')
ui <- 
  dashboardPage(
    dashboardHeader(title = "APSTAT APP",
                    titleWidth = 220),
    dashboardSidebar(
      div(img(src = "logo.png", width=75,style="display: block; margin-left: auto; margin-right: auto;")),
      sidebarMenu(
        menuItem("Beranda", tabName = "home", icon = icon("home")),
        menuItem("Input Data",tabName="data",icon=icon("upload")),
        menuItem("Statistika Deskriptif",tabName="summary",icon=icon("list-alt")),
        menuItem("Penyajian Data",tabName = "visualisation",icon=icon("bar-chart-o")),
        menuItem("Uji Normalitas", tabName="normal",icon=icon("chart-line")),
        menuItem("Uji Banding",tabName="ttest",icon=icon("laptop-code")),
        menuItem("Uji Korelasi",tabName="korelasi",icon=icon("project-diagram")),
        menuItem("Uji Regresi",tabName="regresi",icon=icon("calculator")),
        menuItem("Help", tabName = "Bantuan", icon = icon("question-circle"))
      )
    ),
    dashboardBody(
      shinyDashboardThemes(theme = "grey_dark"),
      tabItems(
        tabItem(tabName = "home",
                h1(strong("Welcome To Statistical Analysis APP"), align="center", style = "font-family: 'Helvetica', serif;
                 font-weight: normal; font-thickness: 150%; font-size: 150%; text-shadow: 0px 0px 0px #aaa; line-height: 0.4;
                 color: dark grey"),
                
                h6(strong("This statistical application is open source, and can be used easily. 
                   With this application, it is hoped that it can help complete your data processing with very simple features.
                   Good luck and enjoy the features!!"), align="center"),
                
                h6(strong("developer : arinda cinta lestari"),align="right"),
                
                
                tabItem(tabName = "Help",
                        h1(strong("Bantuan ?"), align="center", style = "font-family: 'Helvetica', serif;
                 font-weight: normal; font-thickness: 150%; font-size: 150%; text-shadow: 0px 0px 0px #aaa; line-height: 0.4;
                 color: dark grey"),
                        
                        
                        
                        h6(strong("*input dataset terlebih dahulu. kemudian, pilih menu setiap uji yang dibutuhkan.
                                  Selanjutnya pilih variabel pada dataset yang akan diuji statistik"), align="center"),
                        
                        h6(strong("Hasil output uji yang diinginkan akan tampil pada menu yang dipilih"),align="center")
                ),
        ),
        tabItem(tabName = "konsep",
                h1(strong("Pengantar"), align="center", style = "font-family: 'Helvetica', serif;
                 font-weight: normal; font-thickness: 150%; font-size: 150%; text-shadow: 0px 0px 0px #aaa; line-height: 0.4;
                 color: dark grey"),
                h6(strong("....................."), align = "light")
        ),
        tabItem(tabName = "data",
                sidebarLayout(
                  sidebarPanel(
                    # Input: Select a file ----
                    fileInput("file1", "Pilih File CSV ",
                              multiple = FALSE,
                              accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                    # Horizontal line ----
                    tags$hr(),
                    # Input: Checkbox if file has header ----
                    checkboxInput("header", "Header", TRUE),
                    # Input: Select separator ----
                    radioButtons("sep", "Separator",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t"),
                                 selected = ","),
                    # Input: Select quotes ----
                    radioButtons("quote", "Quote",
                                 choices = c(None = "",
                                             "Double Quote" = '"',
                                             "Single Quote" = "'"),
                                 selected = '"'),
                    # Horizontal line ----
                    tags$hr(),
                    # Input: Select number of rows to display ----
                    radioButtons("disp", "Display",
                                 choices = c(Head = "head",
                                             All = "all"),
                                 selected = "head")
                  ),
                  mainPanel(
                    tableOutput("contents")
                  )
                )
        ),
        tabItem(tabName = "summary",
                verbatimTextOutput("sum"),
                h5(strong("Keterangan:")),
                tags$h5(withMathJax("\\(vars\\) = Urutan Variabel")),
                tags$h5(withMathJax("\\(n\\) = Banyaknya Data")),
                tags$h5(withMathJax("\\(sd\\) = Standar Deviasi")),
                tags$h5(withMathJax("\\(median\\) = Median")),
                tags$h5(withMathJax("\\(trimmed\\) = banyaknya data")),
                tags$h5(withMathJax("\\(mad\\) = Mean Absolute Deviation")),
                tags$h5(withMathJax("\\(min\\) = Nilai Minimum")),
                tags$h5(withMathJax("\\(max\\) = Nilai Maksimum")),
                tags$h5(withMathJax("\\(range\\) = Jangkaun data")),
                tags$h5(withMathJax("\\(skew\\) = Nilai Skewness")),
                tags$h5(withMathJax("\\(kurtosis\\) = Nilai Kutosis")),
                tags$h5(withMathJax("\\(se\\) = Standar Error ")),
                verbatimTextOutput("summ")
                
        ),
        tabItem(tabName = "visualisation",
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      inputId="visual",
                      label="Penyajian Data",
                      choices = c("Scatterplot","Box Plot","Bar Plot","Histogram"),
                      multiple = FALSE,
                      selected = ""),
                    conditionalPanel(
                      condition = "input.visual == 'Scatterplot'",
                      selectInput(inputId="x", label="Pilih Variabel X:", choices= c()),
                      selectInput(inputId="y", label="Pilih Variabel Y:", choices= c())
                    ),
                    conditionalPanel(
                      condition = "input.visual == 'Box Plot'",
                      selectInput(inputId="x1", label="Pilih Variabel:", choices= c()),
                      selectInput(inputId="y1", label="Pilih Variabel:", choices= c())
                    ),
                    conditionalPanel(
                      condition = "input.visual == 'Bar Plot'",
                      selectInput(inputId="x2", label="Pilih Variabel:", choices= c()),
                      selectInput(inputId="y2", label="Pilih Variabel:", choices= c())
                    ),
                    conditionalPanel(
                      condition = "input.visual == 'Histogram'",
                      selectInput(inputId="x3", label="Pilih Variabel:", choices= c()),
                      selectInput(inputId="y3", label="Pilih Variabel:", choices= c())
                    )),
                  mainPanel(
                    conditionalPanel(
                      condition = "input.visual == 'Scatterplot'",
                      box(title = "Scatterplot", 
                          closable = FALSE, 
                          width = 12,
                          height = "500px",
                          solidHeader = TRUE, 
                          collapsible = FALSE,
                          plotOutput("scatterplot")
                      )),
                    conditionalPanel(
                      condition = "input.visual == 'Box Plot'",
                      box(title = "Box Plot", 
                          closable = FALSE, 
                          width = 12,
                          height = "500px",
                          solidHeader = TRUE, 
                          collapsible = FALSE,
                          plotOutput("boxplot")
                      )),
                    conditionalPanel(
                      condition = "input.visual == 'Bar Plot'",
                      box(title = "Bar Plot", 
                          closable = FALSE, 
                          width = 12,
                          height = "500px",
                          solidHeader = TRUE, 
                          collapsible = FALSE,
                          plotOutput("barplot")
                      )),
                    conditionalPanel(
                      condition = "input.visual == 'Histogram'",
                      fluidRow(
                        box(title = "Histogram Variabel 1", 
                            closable = FALSE, 
                            width = 6,
                            height = "500px",
                            solidHeader = TRUE, 
                            collapsible = FALSE,
                            plotOutput("Distribuction1")),
                        box(title = "Histogram Variabel 2", 
                            closable = FALSE, 
                            width = 6,
                            height = "500px",
                            solidHeader = TRUE, 
                            collapsible = FALSE,
                            plotOutput("Distribuction2")))
                    )
                  ))),
        tabItem(tabName = "normal",
                sidebarLayout(sidebarPanel(
                  selectInput(
                    inputId="ujinorm",
                    label="Uji Normalitas yang digunakan :",
                    choices = c("Kolmogorov Smirnov","Shapiro Wilk"),
                    multiple = FALSE,
                    selected = ""),
                  conditionalPanel(
                    condition = "input.ujinorm == 'Kolmogorov Smirnov'",
                    selectInput(inputId="norm1", label="Pilih Variabel :", choices= c())
                  ),
                  conditionalPanel(
                    condition = "input.ujinorm == 'Shapiro Wilk'",
                    selectInput(inputId="norm2", label="Pilih Variabel :", choices= c())
                  )),
                  mainPanel(
                    conditionalPanel(
                      condition = "input.ujinorm == 'Kolmogorov Smirnov'",
                      verbatimTextOutput("ks_test"),
                      span(textOutput("ksnote"), style="color:black")),
                    conditionalPanel(
                      condition = "input.ujinorm == 'Shapiro Wilk'",
                      verbatimTextOutput("shapiro_test"),
                      span(textOutput("shapironote"), style="color:black"))
                  ))),
        
        tabItem(tabName = "ttest",
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      inputId="ujit",
                      label="Uji T Untuk :",
                      choices = c("satu sampel","dua sampel"),
                      multiple = FALSE,
                      selected = ""),
                    conditionalPanel(
                      condition = "input.ujit == 'satu sampel'",
                      textInput("ttest1", "Masukan Variabel"),
                      numericInput("mu", "Apakah rata rata sama dengan",NA,min=1,max=NA)
                    ),
                    hr(),
                    conditionalPanel(
                      condition = "input.ujit == 'dua sampel'",
                      textInput("ts1", "Masukan Variabel 1"),
                      textInput("ts2", "Masukan Variabel 2"),
                      checkboxInput("Paired", "Apakah kedua sampel dependent", FALSE))),
                  mainPanel(
                    conditionalPanel(
                      condition = "input.ujit == 'satu sampel'",
                      verbatimTextOutput("hasil_satu")),
                    conditionalPanel(
                      condition="input.ujit=='dua sampel'",
                      verbatimTextOutput("paired")))
                )),
        tabItem(tabName = "regresi",
                navbarPage(title = span( "Regresi"),
                           tabPanel("Regresi Linier Sederhana",
                                    br(), 
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("regsed1", "Variabel Bebas (X)",choices = c()),
                                        selectInput("regsed2", "Variabel Terikat (Y)",choices = c())),
                                      mainPanel(
                                        tabsetPanel(type="tabs",
                                                    tabPanel("Uji Linearitas",
                                                             br(),
                                                             p(strong("Hipotesis")),
                                                             p("H",tags$sub("0"),": Hubungan linear"),
                                                             p("H",tags$sub("1"),": Hubungan tidak linear"),
                                                             p(strong("Kriteria Uji")),
                                                             p("Jika p-value > 0,05 maka H",tags$sub("0"),"diterima"),
                                                             verbatimTextOutput("linear")),
                                                    tabPanel("Model Regresi Sederhana", 
                                                             br(),
                                                             verbatimTextOutput("reg"))
                                        ))
                                    )),
                           tabPanel("Regresi Linier Berganda",
                                    br(),
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput(
                                          "varx",
                                          label = "Variabel bebas (X):",
                                          choices = c(""),
                                          multiple = TRUE,
                                          selected = NA
                                        ),
                                        selectInput(
                                          "vary",
                                          label="Variabel terikat (Y):",
                                          choices = c(""),
                                          multiple = FALSE,
                                          selected=NA
                                        )),
                                      mainPanel(
                                        tabsetPanel(type="tabs",
                                                    tabPanel("Uji Multikolinearitas",
                                                             verbatimTextOutput("multiko")),
                                                    tabPanel("Uji Autokorelasi",
                                                             verbatimTextOutput("autokor")),
                                                    tabPanel("Uji Heterokedastisitas",
                                                             verbatimTextOutput("hetero")),
                                                    tabPanel("Model Regresi Berganda",
                                                             verbatimTextOutput("regganda"))
                                                    
                                        ) 
                                      ))),
                           inverse=F)),
        tabItem(tabName = "korelasi",
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "krlvar1", label = "Pilih Variabel 1", choices = c()),
                    selectInput(inputId = "krlvar2", label ="Pilih Variabel 2",choices = c()),
                    hr(),
                    selectInput(
                      inputId="jeniskorelasi",
                      label="Pilih Jenis Korelasi",
                      choices = c("pearson","kendall","spearman"),
                      multiple = FALSE,
                      selected = "")
                  ),
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Corrplot", 
                                         br(),
                                         p(strong("Corrplot seluruh variabel dalam dataset"),
                                           plotOutput("kore"))),
                                tabPanel("Korelasi", print(strong("Koefisien Korelasi :")),verbatimTextOutput("jkorelasi"), 
                                         print("Korelasi untuk dua variabel bertipe numerik / integer")))
                  )
                )
        )
      )     )    )


server <- function(session,input,output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  #Tab Summary
  output$summ<-renderPrint({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    summary(df)
  })
  #tab visualisasi
  observeEvent(input$file1, {
    df<-read.csv(input$file1$datapath)
    updateSelectInput(session,"x",label = "Pilih Variabel X:",choices = colnames(df))
    updateSelectInput(session,"y",label = "Pilih Variabel Y:",choices = colnames(df))
    updateSelectInput(session,"x1",label = "Pilih Variabel:",choices = colnames(df))
    updateSelectInput(session,"y1",label = "Pilih Variabel Kategori:",choices = colnames(df))
    updateSelectInput(session,"x2",label = "Pilih Variabel:",choices = colnames(df))
    updateSelectInput(session,"y2",label = "Pilih Variabel Kategori:",choices = colnames(df))
    updateSelectInput(session,"x3",label = "Pilih Variabel 1:",choices = colnames(df))
    updateSelectInput(session,"y3",label = "Pilih Variabel 2:",choices = colnames(df))
  })   
  
  output$scatterplot <- renderPlot({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch({
      plot(df[,input$x], df[,input$y],
           xlab = input$x, ylab = input$y, pch = 19)
      abline(lm(df[, input$y] ~ df[,input$x]), col = "grey")
      lines(lowess(df[, input$y] ~ df[,input$x]), col = "black")
    },
    error = function(e){
      print("")
    }
    )
  })
  output$boxplot <- renderPlot({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch({
      boxplot(df[,input$x1]~df[,input$y1], xlab = input$y1,ylab=input$x1)
    },
    error = function(e){
      print("")
    }
    )
  })
  output$barplot <- renderPlot({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch({
      barplot(df[,input$x2],df[,input$y2], xlab = input$x2,ylab=input$y2)
    },
    error = function(e){
      print("")
    }
    )
  })
  
  # histogram output var 1
  output$Distribuction1 <-renderPlot({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch(
      {
        hist(df[, input$x3], main = "", xlab = input$x3)
      },
      error = function(e){
        print("")
      }
    )
  })
  
  # histogram output var 2
  output$Distribuction2 <-renderPlot({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch(
      {
        hist(df[, input$y3], main = "", xlab = input$y3)
      },
      error = function(e){
        print("")
      }
    )
  })
  #Tab Normalitas
  observeEvent(input$file1, {
    df<-read.csv(input$file1$datapath)
    updateSelectInput(session,"norm1",label = "Pilih Variabel :",choices = colnames(df))
    updateSelectInput(session,"norm2",label = "Pilih Variabel :",choices = colnames(df))
  })
  output$ks_test <-renderPrint({
    
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch(
      {
        ks.test(df[, input$norm1],ecdf(df[,input$norm1]))
      },
      error = function(e){
        print("silahkan inputkan variabel yang ingin diuji normalitasnya")
      }
    )
  })
  output$ksnote <- renderText({"Data berdistribusi normal jika p-value > 0,05"})
  output$shapiro_test <-renderPrint({
    
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch(
      {
        shapiro.test(df[, input$norm2])
      },
      error = function(e){
        print("silahkan input variabel yang ingin diuji normalitasnya")
      }
    )
  })
  output$shapironote <- renderText({"Data berdistribusi normal jika p-value > 0,05"})
  
  #Tab Uji T
  output$paired <- renderPrint({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch({
      t.test(df[,input$ts1],df[,input$ts2], alternative="two.sided", paired=input$Paired)
    },
    error = function(e){
      #return a safeError if a parsing error occur
    }
    )
  })
  
  #Regresi
  
  
  output$hasil_satu <- renderPrint({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch({
      t.test(df[,input$ttest1], alternative="two.sided", mu = input$mu , conf.level = 0.95)
    },
    error = function(e){
      #return a safeError if a parsing error occur
    }
    )
  })
  observeEvent(input$file1, {
    df<-read.csv(input$file1$datapath)
    updateSelectInput(session,"varx",label = "Variabel Bebas (X)",choices = colnames(df))
    updateSelectInput(session,"vary",label = "Variabel Terikat (Y)",choices = colnames(df))
  })
  output$regganda <- renderPrint({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch({
      model1<-reactive({
        vars<-as.matrix(df[,input$varx])
        lm(df[,input$vary]~vars,data=df)})
      summary(model1())
      
    },
    error = function(e){
      #return a safeError if a parsing error occur
    }
    )
  })
  
  output$multiko <- renderPrint({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch({
      model1<-reactive({
        vars<-as.matrix(df[,input$varx])
        lm(df[,input$vary]~vars,data=df)})
      ols_vif_tol(model1())
      
    },
    error = function(e){
      #return a safeError if a parsing error occur
    }
    )
  })
  output$autokor <- renderPrint({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch({
      model1<-reactive({
        vars<-as.matrix(df[,input$varx])
        lm(df[,input$vary]~vars,data=df)})
      dwtest(model1())
      
    },
    error = function(e){
      #return a safeError if a parsing error occur
    }
    )
  })
  output$hetero <- renderPrint({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch({
      model1<-reactive({
        vars<-as.matrix(df[,input$varx])
        lm(df[,input$vary]~vars,data=df)})
      bptest(model1())
      
    },
    error = function(e){
      #return a safeError if a parsing error occur
    }
    )
  })
  #----------------------------Tab Korelasi--------------------------------------------------------#
  observeEvent(input$file1,{
    df <- read.csv(input$file1$datapath)
    updateSelectInput(session,"krlvar1",label ="Pilih Variabel 1",choices = colnames(df))
    updateSelectInput(session,"krlvar2",label ="Pilih Variabel 2",choices = colnames(df))
  })
  
  # struktur data
  output$strdat <- renderPrint({
    req(input$file1)
    
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        df <- na.omit(df)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        
      }
    )
    tryCatch({
      str(df)
      summary(df)
    },
    error = function(e){
      #return a safeError if a parsing error occur
      print("Input data untuk mengetahui struktur data")
    }
    )
  })
  
  # korelasi
  output$kore <- renderPlot({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        df <- na.omit(df)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    tryCatch({
      df1 <- sapply(df[,], as.numeric)
      M = cor(df1, method="pearson")
      corrplot(M, method = 'number')
    },
    error= function(e){
      #return a safeError if a parsing error occur
      print("")
    }
    )
  })
  
  # Jenis Korelasi
  output$jkorelasi <- renderPrint({
    req(input$file1)
    
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        df <- na.omit(df)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        
      }
    )
    tryCatch({
      cor(df[,input$krlvar1],df[,input$krlvar2], method=input$jeniskorelasi)
    },
    error = function(e){
      #return a safeError if a parsing error occur
      print("Pilih Variabel yang akan di analisis korelasi")
      print("koefisien korelasi tidak muncul jika variabel bertipe karakter/string")
    }
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

