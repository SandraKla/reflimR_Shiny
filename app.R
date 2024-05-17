####################################### WELCOME TO THE SHINY APP ##################################
####################################### from Sandra K. (2023) #####################################
###################################################################################################

# Get template for the dataset
# data <- reflimR::livertests
# 
# write.csv(data, "reflim_csv.csv", row.names = FALSE)
# write.csv2(data, "reflim_csv2.csv", row.names = FALSE)
# write_xlsx(data, "reflim_excel.xlsx")
# 
# dataset_original1 <- read.csv("reflim_csv.csv")
# dataset_original2 <- read.csv2("reflim_csv2.csv")
# dataset_original3 <- read_excel("reflim_excel.xlsx")
# 
# write.csv2(dataset_original1, "reflim_data1.csv", row.names = FALSE)
# write.csv2(dataset_original2, "reflim_data2.csv", row.names = FALSE)
# write.csv2(dataset_original3, "reflim_data3.csv", row.names = FALSE)

####################################### Load Script and Example-Dataset ###########################

source("zlog.R")

####################################### Libraries #################################################

if("DT" %in% rownames(installed.packages())){
  library(DT)} else{
    install.packages("DT")
    library(DT)}

if("reflimR" %in% rownames(installed.packages())){
  library(reflimR)} else{
    install.packages("reflimR")
    library(reflimR)}

if("shinydashboard" %in% rownames(installed.packages())){
  library(shinydashboard)} else{
    install.packages("shinydashboard")
    library(shinydashboard)}

dataset_original <- reflimR::livertests

####################################### User Interface ############################################

ui <- dashboardPage(
  dashboardHeader(title = "reflimR_Shiny", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebarid",
      
      div(
        style = "text-align:center",
        br(),
        "Estimation of reference limits", br(),
        "from routine laboratory results", hr()
      ),
      
      uiOutput("dataset_file"),
      uiOutput("parameters"),
      actionButton('reset', 'Reset Input', icon = icon("trash")),
      
      selectInput(
        "sex",
        "Select the sex:",
        choices = c("Female (F) & Male (M)" = "t", "Female (F)" = "f", "Male (M)" = "m")
      ),
      
      sliderInput(
        "age_end",
        "Select age-range:",
        min = 0,
        max = 100,
        value = c(0, 100)),
      
      numericInput(
        "nmin",
        "Select n.min:",
        200,
        min = 40,
        max = 1000
      ),
      
      checkboxInput("check_plot.all", "View all plots"),
      
      hr(),
      
      checkboxInput("check_targetvalues", "Load preinstalled target values", value = FALSE),
      checkboxInput("check_target", "Own target values", value = FALSE),
      
      conditionalPanel(
        condition = "input.check_target == true",
        
          numericInput(
            "target_low",
            "Lower value:",
            10,
            min = 0,
            max = 10000
          ),
      
          numericInput(
            "target_upper",
            "Upper value:",
            15,
            min = 0,
            max = 10000
          )
      )
    )
  ),
  
  dashboardBody(
    fluidRow(
      
      tabsetPanel( 
        tabPanel("reflimR", 
          icon = icon("chart-line"), 
      
          box(
            title = "",
            width = 7,
            solidHeader = TRUE,
            status = "info",
        
            p("This Shiny App is based on the package", a("reflimR", href = "https://cran.r-project.org/web/packages/reflimR/index.html"), "for the estimation of reference limits from routine laboratory results:"),
            plotOutput("plot", height = "700px")
          )
        ),
       
        tabPanel("Data", 
          icon = icon("table"),
                  
          box(
            title = "",
            status = "info",
            width = 7,
            solidHeader = TRUE,
                    
            p("This Shiny App is based on the package", a("reflimR", href = "https://cran.r-project.org/web/packages/reflimR/index.html"), "for the estimation of reference limits from routine laboratory results:"),
            DT::dataTableOutput("table", height = "700px")
          )
        ),
      
        tabPanel("Scatterplot", 
          icon = icon("chart-line"),
                 
          box(
            title = "",
            status = "info",
            width = 7,
            solidHeader = TRUE,
                   
            p("This Shiny App is based on the package", a("reflimR", href = "https://cran.r-project.org/web/packages/reflimR/index.html"), "for the estimation of reference limits from routine laboratory results:"),
            plotOutput("scatterplot", height = "700px")
          )
        ),
        
        tabPanel( "Statistics", 
          icon = icon("chart-bar"),
          
          box(
            title = "",
            status = "info",
            width = 7,
            solidHeader = TRUE,
      
            p("This Shiny App is based on the package", a("reflimR", href = "https://cran.r-project.org/web/packages/reflimR/index.html"), "for the estimation of reference limits from routine laboratory results:"),
            plotOutput("plot_statistics", height = "700px")
          )
        ),
        
        tabPanel( "zlog", 
                  icon = icon("table"),
                  
                  box(
                    title = "",
                    status = "info",
                    width = 7,
                    solidHeader = TRUE,
                    
                    p("This Shiny App is based on the package", a("reflimR", href = "https://cran.r-project.org/web/packages/reflimR/index.html"), "for the estimation of reference limits from routine laboratory results:"),
                    DT::dataTableOutput("table_zlog")
                  )
        )
      ),
      
      box(
        title = tagList(shiny::icon("table"), "Report"),
        status = "info",
        width = 5,
        solidHeader = TRUE,
        
        DT::dataTableOutput("table_report")
      ),
      
      # box(
      #   title = tagList(shiny::icon("chart-bar"), "Console"),
      #   status = "info",
      #   width = 5,
      #   solidHeader = TRUE,
      #   collapsible = TRUE,
      #   collapsed = TRUE,
      # 
      #   verbatimTextOutput("console")
      # )
    )
  )
)

####################################### Server ####################################################

server <- function(input, output, session) {
  
  options(shiny.sanitize.errors = TRUE)
  options(warn = -1)
  
  values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$dataset_file1, {
    values$upload_state <- 'uploaded'
  })
  
  observeEvent(input$reset, {
    values$upload_state <- 'reset'
  })
  
  dataset_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$dataset_file1)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  output$dataset_file <- renderUI({
    input$reset ## Create a dependency with the reset button
    fileInput('dataset_file1', label = NULL,  multiple = FALSE)
  })
  
  output$parameters <- renderUI({
    if(is.null(dataset_input())){ 
      choices <- colnames(dataset_original)[4:length(colnames(dataset_original))]
      } 
    else{
      validate(need(endsWith(dataset_input()[["datapath"]], ".csv"), "Check if you have used the correct template!"))
      choices <- colnames(read.csv2(dataset_input()[["datapath"]]))[4:length(colnames(read.csv2(dataset_input()[["datapath"]])))]
    }
    selectInput("parameter","Select preinstalled dataset:", choices = choices, selected = TRUE)
  })      
  
  # Create a reactive values to track the state of the checkboxes
  reactive_values <- reactiveValues(
    check_targetvalues = FALSE,
    check_target = FALSE
  )
  
  # Observe changes in check_targetvalues and update the reactive value
  observeEvent(input$check_targetvalues, {
    if (input$check_targetvalues) {
      reactive_values$check_targetvalues <- TRUE
      reactive_values$check_target <- FALSE
    } else {
      reactive_values$check_targetvalues <- FALSE
    }
  })
  
  # Observe changes in check_target and update the reactive value
  observeEvent(input$check_target, {
    if (input$check_target) {
      reactive_values$check_target <- TRUE
      reactive_values$check_targetvalues <- FALSE
    } else {
      reactive_values$check_target <- FALSE
    }
  })
  
  # Update the checkboxes based on the reactive value
  observe({
    updateCheckboxInput(session, "check_targetvalues", value = reactive_values$check_targetvalues)
    updateCheckboxInput(session, "check_target", value = reactive_values$check_target)
  })
  
  ##################################### Reactive Expressions ######################################
  
  get_data_file <- reactive({
    
    input$nmin
    input$sex
    input$parameter
    input$check_plot.all
    input$check_targetvalues
    input$check_target
    input$dataset_file
    input$age_end
    
    if(is.null(dataset_input())){
      dataset_original <- livertests
      
      column_number <- which(names(dataset_original) == input$parameter)
      dataset_original <- dataset_original[c(1,2,3,column_number)]
    } else{
      
      validate(need(endsWith(dataset_input()[["datapath"]], ".csv"), "Check if you have used the correct template!"))
      dataset_original <- read.csv2(dataset_input()[["datapath"]])
      
      validate(need(nrow(dataset_original) >= 0, "Check if you have used the correct template!"))

      column_number <- which(names(dataset_original) == input$parameter)
      dataset_original <- dataset_original[c(1,2,3,column_number)]
      dataset_original[,4] <- as.numeric(dataset_original[,4])
    }

    return(dataset_original)
  })
  
  # Create the table with the zlog values as reactive expression 
  reflim_data <- reactive({
    
    dat <- get_data_file()

    validate(need(ncol(dat) == 4, "Check if you have used the correct template!"))
    
    dat <- subset(dat,dat$Age >= input$age_end[1] & dat$Age <= input$age_end[2])
    
    datm <- subset(dat,dat$Sex== "m")
    datf <- subset(dat,dat$Sex== "f")
    
    if(input$sex == "m"){dat <- datm}
    if(input$sex == "f"){dat <- datf}
    
    return(dat)
  })

  get_data_report <- reactive({
    
    dat <- reflim_data()
    validate(need(nrow(dat) > 39,
                  "(reflim) n = 0. The absolute minimum for reference limit estimation is 40."))
    
    if(input$check_target == FALSE && input$check_targetvalues == FALSE){
      reflim_text <- reflim(dat[,4], n.min = input$nmin, plot.all = FALSE)
    }
    
    if(input$check_target){
      validate(need(input$target_low < input$target_upper,
                    "(reflim) the upper target limit must be greater than the lower target limit."))
      
      validate(need(input$target_low > 0, 
                    "(reflim) the lower target limit must be greater than 0."))
      
      validate(need(input$target_upper > 0, 
                    "(reflim) the upper target limit must be greater than 0."))
      
      validate(need(input$target_low > 0 && input$target_upper > 0, 
                    "(reflim) the lower and upper target limit must be greater than 0."))
      
      reflim_text <- reflim(dat[,4], targets = c(input$target_low, input$target_upper), n.min = input$nmin, plot.all = FALSE)
    }
    
    if(input$check_targetvalues){
      
      validate(need(input$sex != "t", 
                    "(reflim) The reference intervals are sex-specific. Please select a sex."))
      
      targets <- reflimR::targetvalues
      targets_values <- targets[targets$analyte == input$parameter, ]
      
      if (input$sex == "m") {
        targetvalues_low <-  targets_values[, 5]
        targetvalues_upper <- targets_values[, 6]
      }
      if (input$sex == "f") {
        targetvalues_low <- targets_values[, 3]
        targetvalues_upper <- targets_values[, 4]
      }
      
      validate(need(nrow(targets_values) > 0, 
                    "(reflim) There are no preloaded target values for this parameter!"))
      
      reflim_text <- reflim(dat[,4], targets = c(targetvalues_low, targetvalues_upper), n.min = input$nmin, plot.all = FALSE)
    }
    report <- reflim_text
    
    return(report)
  })
  
  ##################################### Output ####################################################
  
  output$plot <- renderPlot({
    
    dat <- reflim_data()
    validate(need(nrow(dat) > 39,
                  "(reflim) n = 0. The absolute minimum for reference limit estimation is 40."))

    reflimR.plot.all <- FALSE
    
    if (input$check_plot.all) {
      reflimR.plot.all <- TRUE
    }
    
    if (input$check_target) {
      validate(need(input$target_low < input$target_upper,
                    "(reflim) the upper target limit must be greater than the lower target limit."))
      
      validate(need(input$target_low > 0, 
                    "(reflim) the lower target limit must be greater than 0."))

      validate(need(input$target_upper > 0, 
                      "(reflim) the upper target limit must be greater than 0."))

      validate(need(input$target_low > 0 && input$target_upper > 0, 
                      "(reflim) the lower and upper target limit must be greater than 0."))

      reflim_result <- reflim(dat[, 4], targets = c(input$target_low, input$target_upper), n.min = input$nmin, plot.all = reflimR.plot.all)
    }
    
    if (input$check_targetvalues) {
      validate(need(input$sex != "t",
                    "(reflim) The reference intervals are sex-specific. Please select a sex."))
      
      targets <- reflimR::targetvalues
      targets_values <- targets[targets$analyte == input$parameter,]
      
      if (input$sex == "m") {
        targetvalues_low <-  targets_values[, 5]
        targetvalues_upper <- targets_values[, 6]
      }
      if (input$sex == "f") {
        targetvalues_low <- targets_values[, 3]
        targetvalues_upper <- targets_values[, 4]
      }
      
      validate(need(nrow(targets_values) > 0, 
                    "(reflim) There are no preloaded target values for this parameter!"))
      
      reflim_result <- reflim(dat[, 4], targets = c(targetvalues_low, targetvalues_upper), n.min = input$nmin, plot.all = reflimR.plot.all)
    }
    
    if (input$check_target == FALSE && input$check_targetvalues == FALSE) {
      reflim_result <- reflim(dat[, 4], n.min = input$nmin, plot.all = reflimR.plot.all)
    }
    
    reflim_result
  })
  
  output$scatterplot <- renderPlot({
    
    dat <- reflim_data()
    
    plot(dat[,4] ~ dat[,2], pch = 20, cex = 1, col = factor(dat[,3]), xlab = "Age", ylab = colnames(dat)[4])
    legend("topright", legend = levels(factor(dat[,3])), pch = 19, col = factor(levels(factor(dat[,3]))))
  })
  
  output$table <- DT::renderDataTable({
    
    DT::datatable(reflim_data(), extensions = 'Buttons', caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Dataset'),
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')))
  })
  
  output$plot_statistics <- renderPlot({
    
    par(mfrow=c(2,1))
    
    dat <- reflim_data()
    ylab_ <- colnames(dat)[4]
    
    if(!(nrow(dat)) == 0){
      hist_data_w <- subset(dat, Sex == "f", select = Age)
      hist_data_m <- subset(dat, Sex == "m", select = Age)
      
      hist_w <- hist(hist_data_w$Age, breaks=seq(min(dat[,2])-1,max(dat[,2]),by=1))$counts
      hist_m <- hist(hist_data_m$Age, breaks=seq(min(dat[,2])-1,max(dat[,2]),by=1))$counts
      
      barplot(rbind(hist_m,hist_w), col = c("cornflowerblue","indianred"),
              names.arg=seq(min(dat[,2]), max(dat[,2]), by=1), xlab = "Age", las = 1, beside = TRUE, ylab = "Number of data")
      abline(h=0)
      legend("topright", legend = c(paste0("Men: ", nrow(hist_data_m)), paste0("Female: ", nrow(hist_data_w))), col = c("cornflowerblue","indianred"), pch = c(17, 20))
      
      par(new = TRUE)
      boxplot(dat[,2], horizontal = TRUE, axes = FALSE, col = rgb(0, 0, 0, alpha = 0.15))
    }
    
    if(!(nrow(dat)) == 0){

      if (input$sex == "m") {
        boxplot(dat[,4]~interaction(dat[,3], dat[,2]), xlab = "Age", 
                ylab = ylab_, col = "cornflowerblue", las = 2)
      }
      else if (input$sex == "f") {
        boxplot(dat[,4]~interaction(dat[,3], dat[,2]), xlab = "Age", 
                ylab = ylab_, col = "indianred", las = 2)
      } else{
        boxplot(dat[,4]~interaction(dat[,3], dat[,2]), xlab = "Age", 
                ylab = ylab_, col = c("indianred", "cornflowerblue"), las = 2)
      }
    }
  })
  
  output$table_report <- DT::renderDataTable({
    
    report <- get_data_report()
    
    if(!is.null(report$limits)){
      table_report <- t(data.frame("Mean:" = report$stats[1],
                                "Standard deviation:" = report$stats[2],
                                "Lognormal Distribution:" = report$lognormal,
                                "Lower limit:" = report$limits[1],
                                "Upper limit:" = report$limits[2],
                                "Lower tolerance intervals:" = paste0(report$limits[3], " - " , report$limits[4]),
                                "Upper tolerance intervals:" = paste0(report$limits[5], " - " , report$limits[6]),
                                "Lower target Limit:" = report$targets[1],
                                "Upper target Limit:" = report$targets[2],
                                "Lower target tolerance intervals:" = paste0(report$targets[3], " - " , report$targets[4]),
                                "Upper target tolerance intervals:" = paste0(report$targets[5], " - " , report$targets[6]),
                                "Lower confidence intervals:" = paste0(report$confidence.int[1], " - " , report$confidence.int[2]),
                                "Upper confidence intervals:" = paste0(report$confidence.int[3], " - " , report$confidence.int[4]),
                                "Interpretation of the lower limit:" = report$interpretation[1],
                                "Interpretation of the upper limit:" = report$interpretation[2],
                                check.names = FALSE))
      colnames(table_report) <- c("Report")
    
      DT::datatable(table_report, extensions = 'Buttons',
                    options = list(dom = 'Bt', pageLength = 16, buttons = c('copy', 'csv', 'pdf', 'print')))
    }
  })
  
  output$table_zlog <- DT::renderDataTable({
    
    dat <- reflim_data()
    report <- get_data_report()
    
    zlog_results <- numeric(nrow(dat))
    for (i in 1:nrow(dat)) {
      zlog_results[i] <- round_df(zlog(dat[i, 4], report$limits[1], report$limits[2]), 2)
    }
  
    reflim_data <- cbind(dat, "RI" = paste0(report$limits[1], " - " , report$limits[2]), "zlog" = zlog_results)

    options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
    
    DT::datatable(reflim_data, rownames= FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')),
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;',
                                                    'Table: Dataset with the zlog values')) %>%
    DT::formatStyle(columns = "zlog", 
                    color = styleEqual(reflim_data[,6], highzlogvalues(c(reflim_data[,6]))),
                    backgroundColor = styleEqual(reflim_data[,6], zlogcolor(c(reflim_data[,6])))) %>%
    DT::formatStyle(columns = colnames(reflim_data)[4], 
                    color = styleEqual(reflim_data[,4], highzlogvalues(c(reflim_data[,6]))),
                    backgroundColor = styleEqual(reflim_data[,4], zlogcolor(c(reflim_data[,6]))))
  })
}
####################################### Run the application #######################################
shinyApp(ui = ui, server = server)
