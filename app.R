####################################### WELCOME TO THE SHINY APP ##################################
####################################### from Sandra K. (2025) #####################################
###################################################################################################

# # Get template for the dataset
# library(writexl)
# library(readxl)
# 
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
source("reflimR_loop.R")
source("mclust.R")

####################################### Libraries #################################################

if ("DT" %in% rownames(installed.packages())) {
  library(DT)} else{
    install.packages("DT")
    library(DT)}

if ("mclust" %in% rownames(installed.packages())) {
  library(mclust)} else{
    install.packages("mclust")
    library(mclust)}

if ("refineR" %in% rownames(installed.packages())) {
  library(refineR)} else{
    install.packages("refineR")
    library(refineR)}

if ("reflimR" %in% rownames(installed.packages())) {
  library(reflimR)} else{
    install.packages("reflimR")
    library(reflimR)}

if ("shinydashboard" %in% rownames(installed.packages())) {
  library(shinydashboard)} else{
    install.packages("shinydashboard")
    library(shinydashboard)}

dataset_original <- reflimR::livertests

upload_text <- HTML(paste0(
  "This Shiny App is based on the package ", a("reflimR", href = "https://cran.r-project.org/web/packages/reflimR/index.html"), 
  " for the estimation of reference limits from routine laboratory results:", br(), br(), 
  "These columns should be used for new data: Category: Name of the category to filter the data, Age: Age in years, Sex: m for male and f for female,
  Value: Column name is the analyte name, values are the laboratory measures. The data from livertests serves as a template. 
  To load new data, the data should be in CSV format with values separated by semicolons (;), and decimal numbers should use a comma (,) as the decimal separator. 
  The first row should contain column headers.", br(), br(),
  "On the left side, the sidebar allows you to select the laboratory parameter, category, age and gender group. 
  In the “Target Values” section, you can load target values from targetvalues, load reference intervals estimated with refineR, or manually enter custom values."
))

reflimR_text <- HTML(paste0(
  "These tab displays the corresponding plot and the outputs of the reflim() function, providing an estimation of new reference intervals or a verification of the selected target values. 
  By clicking “Visualization of all plots across every process step”, all plots generated throughout the workflow can be displayed."
))

scatterplot_text <- HTML(paste0(
  "The scatterplot shows the relationship between age and the laboratory value."
))

statistics_text <- HTML(paste0(
  "The two figures show the distribution of sex across age and the laboratory value."
))

zlog_text <- HTML(paste0(
  "zlog values are calculated from the dataset and the calculated reference intervals. The lower reference limits (LL) and upper reference limits (UL) can transform any result x into a zlog value using the following equation:
  zlog(x) = (log(x)–(log(LL)+ log(UL))/2)*3.92/(log(UL)–log(LL)). Values ranging from –1.96 to 1.96 are considered normal, while values below –5 and above 5 indicate pathological conditions."
))

refineR_text <- HTML(paste0(
  "If, during the verification with reflimR and its target values or own target values, a yellow or red bar appears, 
  a follow-up analysis using refineR is recommended. The resulting reference intervals from refineR can be used as new target values
  and re-verified with reflimR. If all indicators turn green, this suggests that the manufacturer’s target values are likely incorrect. 
  If one or more indicators remain yellow or red, the data are considered too challenging for indirect methods. This assumption can be further 
  evaluated in the “mclust” tab using a Gaussian mixture model (mclust)."
))

mclust_text <- HTML(paste0(
  "Gaussian mixture modelling for the verification of reference intervals."
))

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
        "Estimation and Verification of reference limits", br(),
        "from routine laboratory results", hr()
      ),
      
      uiOutput("parameters"),
      uiOutput("category"),
      
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
      
      hr(),
      
      checkboxInput("check_targetvalues", "Load preinstalled target values", value = FALSE),
      checkboxInput("check_target", "Load own target values", value = FALSE),
      
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
      ), 
      uiOutput("refineR_checkbox_ui"),
      hr()
    )
  ),
  
  dashboardBody(
    fluidRow(
      
      tabsetPanel( 
        tabPanel("Upload", 
          icon = icon("upload"),
                 
          box(
            title = "",
            status = "info",
            width = 7,
            solidHeader = TRUE,
                   
            p(upload_text),
            
            uiOutput("dataset_file"),
            actionButton('reset', 'Reset Input', icon = icon("trash")), hr(),
            
            DT::dataTableOutput("table")
            
            # fluidRow(
            #   column(6, checkboxInput("show_table", "Show Editable Table for Upload", value = FALSE)),
            #   column(6, actionButton("submit", "Submit"))
            # ),
            # conditionalPanel(
            #   condition = "input.show_table == true",
            #   rHandsontableOutput("editable_table")
            # )
          )
        ),
        
        tabPanel("reflimR", 
          icon = icon("chart-line"), 
      
          box(
            title = "",
            width = 7,
            solidHeader = TRUE,
            status = "info",
        
            p(reflimR_text),
            checkboxInput("check_plot.all", "Visualization of all plots across every process step"),
            plotOutput("plot", height = "700px")
          )
        ),
      
        tabPanel("Scatterplot", 
          icon = icon("chart-line"),
                 
          box(
            title = "",
            status = "info",
            width = 7,
            solidHeader = TRUE,
                   
            p(scatterplot_text),
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
      
            p(statistics_text),
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
                    
            p(zlog_text),
            DT::dataTableOutput("table_zlog",  height = "700px")
          )
        ),
        
        tabPanel( "refineR", 
          icon = icon("table"),
                  
          box(
            title = "",
            status = "info",
            width = 7,
            solidHeader = TRUE,
                    
            p(refineR_text),
            #actionButton('calculate', 'Calculate RI with refineR', icon = icon("calculator")),
            plotOutput("plotrefineR", height = "700px"),
            DT::dataTableOutput("table_report_refineR")
          )
        ),
        
        tabPanel( "mclust", 
          icon = icon("table"),
                  
          box(
            title = "",
            status = "info",
            width = 7,
            solidHeader = TRUE,
                    
            p(mclust_text),
            plotOutput("plotmclust", height = "700px"),
          )
        )
      ),
      
      box(
        title = tagList(shiny::icon("table"), "Report for reflimR:"),
        status = "info",
        width = 5,
        solidHeader = TRUE,
        
        DT::dataTableOutput("table_report"), br(), hr(),
        downloadButton("download_ritable", "Download all Reference Intervals"),
        downloadButton("download_zlogtable", "Download all zlog values"),
      )
      
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
  
  ##################################### Observe Events ############################################
  
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
    if (is.null(dataset_input())) { 
      choices <- colnames(dataset_original)[4:length(colnames(dataset_original))]
      } 
    else{
      validate(need(endsWith(dataset_input()[["datapath"]], ".csv"), "Check if you have used the correct template! It must be an CSV file!"))
      choices <- colnames(read.csv2(dataset_input()[["datapath"]]))[4:length(colnames(read.csv2(dataset_input()[["datapath"]])))]
    }
    selectInput("parameter","Select laboratory value:", choices = choices, selected = TRUE)
  })   
  
  output$category <- renderUI({
    if (is.null(dataset_input())) { 
      choices <- unique(dataset_original[1])[[1]]
    } 
    else{
      validate(need(endsWith(dataset_input()[["datapath"]], ".csv"), "Check if you have used the correct template! It must be an CSV file!"))
      choices <- unique(read.csv2(dataset_input()[["datapath"]])[1])[[1]]
    }
    choices <- c("Not selected", choices)
    selectInput("category", "Select category:", choices = choices, selected = "Not selected")
  })
  
  # Create a reactive values to track the state of the checkboxes
  reactive_values <- reactiveValues(
    check_targetvalues = FALSE,
    check_target = FALSE,
    check_refineR = FALSE
  )
  
  # Observe changes in check_targetvalues and update the reactive value
  observeEvent(input$check_targetvalues, {
    if (input$check_targetvalues) {
      reactive_values$check_targetvalues <- TRUE
      reactive_values$check_target <- FALSE
      reactive_values$check_refineR <- FALSE
    } else {
      reactive_values$check_targetvalues <- FALSE
    }
  })
  
  # Observe changes in check_target and update the reactive value
  observeEvent(input$check_target, {
    if (input$check_target) {
      reactive_values$check_target <- TRUE
      reactive_values$check_targetvalues <- FALSE
      reactive_values$check_refineR <- FALSE
    } else {
      reactive_values$check_target <- FALSE
    }
  })
  
  # Observe changes in check_refineR and update the reactive value
  observeEvent(input$check_refineR, {
    if (input$check_refineR) {
      reactive_values$check_refineR <- TRUE
      reactive_values$check_targetvalues <- FALSE
      reactive_values$check_target <- FALSE
    } else {
      reactive_values$check_refineR <- FALSE
    }
  })
  
  # Update the checkboxes based on the reactive value
  observe({
    updateCheckboxInput(session, "check_targetvalues", value = reactive_values$check_targetvalues)
    updateCheckboxInput(session, "check_target", value = reactive_values$check_target)
    updateCheckboxInput(session, "check_refineR", value = reactive_values$check_refineR)
  })
  
  # observeEvent(input$submit, {
  #   if (!is.null(input$editable_table)) { data_store(hot_to_r(input$editable_table))}
  # })
  
  # initial_data <- data.frame(
  #   Category = character(50),
  #   Age = numeric(50),
  #   Sex = character(50),
  #   Analyte = numeric(50),
  #   stringsAsFactors = FALSE
  # )
  # 
  # data_store <- reactiveVal(initial_data)

  # observeEvent(input$show_table, {
  #   if (input$show_table) {
  #     output$editable_table <- renderRHandsontable({
  #       rhandsontable(data_store(), rowHeaders = NULL, colHeaders = colnames(data_store()))
  #     })
  #   }
  # }, ignoreNULL = FALSE)
  
  ##################################### Reactive Expressions ######################################
  
  # Create the table with the dataset as reactive expression 
  reflim_data <- reactive({
    
    input$nmin
    input$sex
    input$parameter
    input$category
    input$check_plot.all
    input$check_targetvalues
    input$check_refineR
    input$check_target
    input$dataset_file
    input$age_end
    
    # if(input$show_table && input$submit) {
    #   dataset_original <- data_store()
    # } else{
    if (is.null(dataset_input())) {
      dataset <- dataset_original
    } else {
      validate(need(
        endsWith(dataset_input()[["datapath"]], ".csv"),
        "Check if you have used the correct template! It must be a CSV file!"
      ))
      
      dataset <- read.csv2(dataset_input()[["datapath"]])
      
      validate(need(
        nrow(dataset) > 0,
        "Check if you have used the correct template! The dataset is empty!"
      ))
    }
    
    column_number <- which(names(dataset) == input$parameter)
    dataset <- dataset[c(1, 2, 3, column_number)]
    
    if (!is.null(input$category) && input$category != "Not selected") {
      dataset <- subset(dataset, dataset[[1]] == input$category)
    }
    #}
    
    validate(need(
      ncol(dataset) == 4, 
      "Check if you have used the correct template! You need 4 columns (Category, Age, Sex, Value)!"
    ))
    dataset[, 4] <- as.numeric(dataset[, 4])
    
    dataset <- subset(dataset, Age >= input$age_end[1] & Age <= input$age_end[2])
    
    if (input$sex %in% c("m", "f")) {
      dataset <- subset(dataset, Sex == input$sex)
    }
    
    return(dataset)
  })
  
  get_alldata_file <- reactive({
    
    input$nmin
    input$sex
    input$parameter
    input$category
    input$check_plot.all
    input$check_targetvalues
    input$check_refineR
    input$check_target
    input$dataset_file
    input$age_end
    
    # if(input$show_table && input$submit) {
    #   dataset_original <- data_store()
    # } else{
    if (is.null(dataset_input())) {
      dataset <- dataset_original
    } else {
      validate(need(
        endsWith(dataset_input()[["datapath"]], ".csv"),
        "Check if you have used the correct template! It must be a CSV file!"
      ))
      
      dataset <- read.csv2(dataset_input()[["datapath"]])
      
      validate(need(
        nrow(dataset) > 0,
        "Check if you have used the correct template! The dataset is empty!"
      ))
    }
    
    if (!is.null(input$category) && input$category != "Not selected") {
      dataset <- subset(dataset, dataset[[1]] == input$category)
    }
    #}
    
    dataset <- subset(dataset, Age >= input$age_end[1] & Age <= input$age_end[2])
    
    if (input$sex %in% c("m", "f")) {
      dataset <- subset(dataset, Sex == input$sex)
    }
    
    return(dataset)
  })
  
  get_data_report <- reactive({
    
    dat <- reflim_data()
    validate(need(nrow(dat) > 39,
                  "(reflim) n = 0. The absolute minimum for reference limit estimation is 40."))
    
    if (input$check_target == FALSE && input$check_targetvalues == FALSE && input$check_refineR == FALSE) {
      reflim_text <- reflim(dat[,4], n.min = input$nmin, plot.all = FALSE)
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
      
      reflim_text <- reflim(dat[,4], targets = c(input$target_low, input$target_upper), n.min = input$nmin, plot.all = FALSE)
    }
    
    if (input$check_targetvalues) {
      
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
    
    if(input$check_refineR) {
      
      validate(need(refineR_done(),"(refineR) Please perform the refineR calculation first."))
  
      table_refineR <- getRI(fit_refineR())
      targetvalues_low <- table_refineR$PointEst[1]
      targetvalues_upper <- table_refineR$PointEst[2]
      
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
    
    if(input$check_refineR) {
      
      validate(need(refineR_done(), "(refineR) Please perform the refineR calculation first."))
      
      table_refineR <- getRI(fit_refineR())
      targetvalues_low <- table_refineR$PointEst[1]
      targetvalues_upper <- table_refineR$PointEst[2]
      
      reflim_result <- reflim(dat[,4], targets = c(targetvalues_low, targetvalues_upper), n.min = input$nmin, plot.all = reflimR.plot.all)
    }
    
    if (input$check_target == FALSE && input$check_targetvalues == FALSE && input$check_refineR == FALSE) {
      reflim_result <- reflim(dat[, 4], n.min = input$nmin, plot.all = reflimR.plot.all)
    }
    
    reflim_result
  })
  
  output$scatterplot <- renderPlot({
    
    dat <- reflim_data()
    colors <- ifelse(dat[, 3] == "f", "indianred", "cornflowerblue")
    pchs <- ifelse(dat[, 3] == "f", 17, 19)
    plot(dat[,4] ~ dat[,2], pch = pchs, cex = 1, col = colors, xlab = "Age", ylab = colnames(dat)[4])
    
    unique_levels <- levels(factor(dat[, 3]))
    legend("topright", legend = unique_levels, pch = c(17, 19)[1:length(unique_levels)], col = c("indianred", "cornflowerblue")[1:length(unique_levels)])
  })
  
  output$table <- DT::renderDataTable({
    
    DT::datatable(reflim_data(), caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Dataset'))
  })
  
  output$plot_statistics <- renderPlot({
    
    par(mfrow = c(2,1))
    
    dat <- reflim_data()
    ylab_ <- colnames(dat)[4]
    
    if (!(nrow(dat)) == 0) {
      hist_data_w <- subset(dat, Sex == "f", select = Age)
      hist_data_m <- subset(dat, Sex == "m", select = Age)
      
      hist_w <- hist(hist_data_w$Age, breaks = seq(min(dat[,2]) - 1,max(dat[,2]),by = 1))$counts
      hist_m <- hist(hist_data_m$Age, breaks = seq(min(dat[,2]) - 1,max(dat[,2]),by = 1))$counts
      
      barplot(rbind(hist_m,hist_w), col = c("cornflowerblue","indianred"),
              names.arg = seq(min(dat[,2]), max(dat[,2]), by = 1), xlab = "Age", las = 1, beside = TRUE, ylab = "Number of data")
      abline(h = 0)
      legend("topright", legend = c(paste0("m: ", nrow(hist_data_m)), paste0("f: ", nrow(hist_data_w))), col = c("cornflowerblue","indianred"), pch = c(19, 19))
      
      par(new = TRUE)
      boxplot(dat[,2], horizontal = TRUE, axes = FALSE, col = rgb(0, 0, 0, alpha = 0.15))
    }
    
    if (!(nrow(dat)) == 0) {

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
    if (!is.na(report$limits[1])) {
      converted_sex <- switch(input$sex,
                              "f" = "Female(F)",
                              "m" = "Male(M)",
                              "t" = "Female(F) & Male(M)")
      
      table_report <- t(data.frame(
                                "Sex and Age:" = paste0(converted_sex, " (", input$age_end[1], "-", input$age_end[2], ")"),
                                "Category:" = input$category,
                                "Mean:" = report$stats[1],
                                "Standard deviation:" = report$stats[2],
                                "Lognormal Distribution:" = report$lognormal,
                                "Reference limit:" =  paste0(report$limits[1] , " - " , report$limits[2]),
                                "Lower tolerance intervals:" = paste0(report$limits[3], " - " , report$limits[4]),
                                "Upper tolerance intervals:" = paste0(report$limits[5], " - " , report$limits[6]),
                                "Target Limits:" = paste0(report$targets[1], " - " , report$targets[2]),
                                "Lower target tolerance intervals:" = paste0(report$targets[3], " - " , report$targets[4]),
                                "Upper target tolerance intervals:" = paste0(report$targets[5], " - " , report$targets[6]),
                                "Lower confidence intervals:" = paste0(report$confidence.int[1], " - " , report$confidence.int[2]),
                                "Upper confidence intervals:" = paste0(report$confidence.int[3], " - " , report$confidence.int[4]),
                                "Interpretation of the lower limit:" = report$interpretation[1],
                                "Interpretation of the upper limit:" = report$interpretation[2],
                                check.names = FALSE))
      colnames(table_report) <- input$parameter
    
      DT::datatable(table_report, extensions = 'Buttons',
                    options = list(dom = 'Bt', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')))
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
    
    DT::datatable(reflim_data, rownames = FALSE, extensions = 'Buttons',
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
  
  refineR_done <- reactiveVal(FALSE)
  
  observeEvent(input$parameters, { 
    refineR_done(FALSE)
  })
  
  observeEvent(input$category, { 
    refineR_done(FALSE)
  })
  
  observeEvent(input$sex, { 
    refineR_done(FALSE)
  })
  
  observeEvent(input$age_end, { 
    refineR_done(FALSE)
  })
  
  observeEvent(input$nmin, { 
    refineR_done(FALSE)
  })
  
  observeEvent(input$reset, { 
    refineR_done(FALSE)
  })
  
  observeEvent(input$dataset_file1, { 
    refineR_done(FALSE)
  })
  
  
  fit_refineR <- reactive({
    
    input$parameters
    input$category
    input$sex
    input$age_end
    input$nmin
    input$reset
    input$dataset_file1
    
    withProgress(message = "RI calculation with refineR …", {
      
      dat <- reflim_data()
      fit <- findRI(Data = dat[, 4])
      refineR_done(TRUE)
      
      fit
    })
  })
  
  output$plotrefineR <- renderPlot({
    plot(fit_refineR())
  })
  
  output$refineR_checkbox_ui <- renderUI({
    checkboxInput(
      "check_refineR",
      "Load calculated refineR values",
      value = FALSE
    )
  })
  
  output$table_report_refineR <- DT::renderDataTable({
    
    table_refineR <- getRI(fit_refineR())
    
    converted_sex <- switch(input$sex,
                            "f" = "Female(F)",
                            "m" = "Male(M)",
                            "t" = "Female(F) & Male(M)")
      
    table_report <- t(data.frame(
        "Sex and Age:" = paste0(converted_sex, " (", input$age_end[1], "-", input$age_end[2], ")"),
        "Category:" = input$category,
        "Reference limit:" =  paste0(round(table_refineR$PointEst[1], 3) , " - " , round(table_refineR$PointEst[2], 3)),
        check.names = FALSE))
    colnames(table_report) <- input$parameter
      
    DT::datatable(table_report, extensions = 'Buttons',
                  caption = htmltools::tags$caption(
                  style = "caption-side: top; text-align: left; font-weight: bold; font-size: 16px;",
                  "Report for refineR"
                  ), options = list(dom = 'Bt', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')))
  })
  
  output$plotmclust <- renderPlot({
    
    withProgress(message = "mclust Calculation …", {
      dat <- reflim_data()
      lab_mclust(dat[, 4])
    })
  })
  
  output$download_ritable <- downloadHandler(
    filename = function() {
      paste("ReferenceIntervals_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      
      dat <- get_alldata_file()
      dataset <- dat[c(-1,-2,-3)]
      reflim.loop.results <- reflim.loop(dataset, plot.it = FALSE)
      
      tmpdir <- tempdir()
      csv_files <- c()
      
      for (col in names(reflim.loop.results)) {
        report <- reflim.loop.results[[col]]
        
        converted_sex <- switch(input$sex,
                                "f" = "Female(F)",
                                "m" = "Male(M)",
                                "t" = "Female(F) & Male(M)")
        
        df <- t(data.frame(
          "Sex and Age:" = paste0(converted_sex, " (", input$age_end[1], "-", input$age_end[2], ")"),
          "Category:" = input$category,
          "Mean:" = report$stats[1],
          "Standard deviation:" = report$stats[2],
          "Lognormal Distribution:" = report$lognormal,
          "Reference Interval:" = paste0(report$limits[1] , " - " , report$limits[2]),
          "Lower tolerance intervals:" = paste0(report$limits[3], " - " , report$limits[4]),
          "Upper tolerance intervals:" = paste0(report$limits[5], " - " , report$limits[6]),
          "Target Limits:" = paste0(report$targets[1], " - " , report$targets[2]),
          "Lower target tolerance intervals:" = paste0(report$targets[3], " - " , report$targets[4]),
          "Upper target tolerance intervals:" = paste0(report$targets[5], " - " , report$targets[6]),
          "Lower confidence intervals:" = paste0(report$confidence.int[1], " - " , report$confidence.int[2]),
          "Upper confidence intervals:" = paste0(report$confidence.int[3], " - " , report$confidence.int[4]),
          "Interpretation of the lower limit:" = report$interpretation[1],
          "Interpretation of the upper limit:" = report$interpretation[2],
          check.names = FALSE))
        
        csv_path <- file.path(tmpdir, paste0(col, ".csv"))
        write.csv(df, csv_path)
        csv_files <- c(csv_files, csv_path)
      }
      
      old_wd <- setwd(tmpdir)
      on.exit(setwd(old_wd))
      
      zip(zipfile = file, files = basename(csv_files), extras = "-j")
    }
  )
  
    output$download_ritable <- downloadHandler(
    filename = function() {
      paste("ReferenceIntervals_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      
      dat <- get_alldata_file()
      dataset <- dat[c(-1,-2,-3)]
      reflim.loop.results <- reflim.loop(dataset, plot.it = FALSE)
      
      tmpdir <- tempdir()
      csv_files <- c()
      
      for (col in names(reflim.loop.results)) {
        report <- reflim.loop.results[[col]]
        
        converted_sex <- switch(input$sex,
                                "f" = "Female(F)",
                                "m" = "Male(M)",
                                "t" = "Female(F) & Male(M)")
        
        df <- t(data.frame(
          "Sex and Age:" = paste0(converted_sex, " (", input$age_end[1], "-", input$age_end[2], ")"),
          "Category:" = input$category,
          "Mean:" = report$stats[1],
          "Standard deviation:" = report$stats[2],
          "Lognormal Distribution:" = report$lognormal,
          "Reference Interval:" = paste0(report$limits[1] , " - " , report$limits[2]),
          "Lower tolerance intervals:" = paste0(report$limits[3], " - " , report$limits[4]),
          "Upper tolerance intervals:" = paste0(report$limits[5], " - " , report$limits[6]),
          "Target Limits:" = paste0(report$targets[1], " - " , report$targets[2]),
          "Lower target tolerance intervals:" = paste0(report$targets[3], " - " , report$targets[4]),
          "Upper target tolerance intervals:" = paste0(report$targets[5], " - " , report$targets[6]),
          "Lower confidence intervals:" = paste0(report$confidence.int[1], " - " , report$confidence.int[2]),
          "Upper confidence intervals:" = paste0(report$confidence.int[3], " - " , report$confidence.int[4]),
          "Interpretation of the lower limit:" = report$interpretation[1],
          "Interpretation of the upper limit:" = report$interpretation[2],
          check.names = FALSE))
        
        csv_path <- file.path(tmpdir, paste0(col, ".csv"))
        write.csv(df, csv_path)
        csv_files <- c(csv_files, csv_path)
      }
      
      old_wd <- setwd(tmpdir)
      on.exit(setwd(old_wd))
      
      zip(zipfile = file, files = basename(csv_files), extras = "-j")
    }
  )
  
  output$download_zlogtable <- downloadHandler(
    filename = function() {
      paste("zlogValues_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      dat <- get_alldata_file()
      
      dataset <- dat[c(-1,-2,-3)]
      reflim.loop.results <- reflim.loop(dataset, plot.it = FALSE)
      zlog.loop.results <- zlog.loop(dataset, reflim.loop.results)
      
      result <- c(dat, zlog.loop.results)
      write.csv(result, file)
    }
  )
}
####################################### Run the application #######################################
shinyApp(ui = ui, server = server)
