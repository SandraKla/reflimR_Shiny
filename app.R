####################################### WELCOME TO THE SHINY APP ##################################
####################################### from Sandra K. (2023) #####################################
###################################################################################################

####################################### Libraries #################################################

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
  dashboardHeader(title = "reflimR", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebarid",
      
      div(
        style = "text-align:center",
        br(),
        "Estimation of reference limits", br(),
        "from routine laboratory results", hr(),
      ),
      
      selectInput(
        "parameter",
        "Select the lab parameter:",
        choices = colnames(dataset_original)[4:length(colnames(dataset_original))],
        selected = TRUE
      ),
      
      selectInput(
        "sex",
        "Select the sex:",
        choices = c("Female (F) & Male (M)" = "t", "Female (F)" = "f", "Male (M)" = "m")
      ),
      
      checkboxInput("check_plot.all", "View all plots"),
      
      hr(),
      
      div(
        style = "text-align:center",
        br(),
        "Target values"),
      
      checkboxInput("check_targetvalues", "Load target values from *targetvalues*", value = FALSE),
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
        ),
      
      hr(),
      
      div(
        style = "text-align:center",
        "For further information visit", br(), "our R-package",
        a("reflimR", href = "https://github.com/reflim/reflimR"), "!"
      )
    )
  ),
  
  dashboardBody(fluidRow(
    tabBox(
      title = "reflimR",
      id = "tabselected",
      width = 8,
      
      tabPanel(
        "Plot",
        icon = icon("chart-line"),
        
        p("This Shiny App is based on the package reflimR for the estimation of reference limits from routine laboratory results."),
        
        verbatimTextOutput("messageOutput"), plotOutput("plot", height = "700px")
      )
    ),
    
    box(
      title = tagList(shiny::icon("info"), "Reference Limits"),
      status = "info",
      width = 4,
      solidHeader = TRUE,

      verbatimTextOutput("console")
    )
  ))
)

####################################### Server ####################################################

server <- function(input, output, session) {
  
  options(shiny.plot.res=128)
  options(shiny.sanitize.errors = TRUE)
  options(warn = -1)
  
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
    
    dataset_original <- reflimR::livertests
        
    column_number <- which(names(dataset_original) == input$parameter)
    dataset_original <- dataset_original[c(1,2,3,column_number)]
        
    return(dataset_original)
  })
  
  # Create the table with the zlog values as reactive expression 
  reflim_data <- reactive({
    
    dat <- get_data_file()
    
    validate(need(ncol(dat) == 4, "Check if you have used the correct template!"))
    
    datm <- subset(dat,dat$Sex== "m")
    datf <- subset(dat,dat$Sex== "f")
    
    if(input$sex == "m"){dat <- datm}
    if(input$sex == "f"){dat <- datf}
    
    return(dat)
 })

  ##################################### Output ####################################################
  
  output$plot <- renderPlot({
    
    dat <- reflim_data()

    reflimR.plot.all <- FALSE
    
    if (input$check_plot.all) {
      reflimR.plot.all <- TRUE
    } 
    
    if (input$check_target) {
      validate(need(input$target_low < input$target_upper,
                    "(reflim) the upper target limit must be greater than the lower target limit."))
      
      reflim_result <- reflim(dat[, 4], targets = c(input$target_low, input$target_upper), plot.all = reflimR.plot.all)
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
      
      reflim_result <- reflim(dat[, 4], targets = c(targetvalues_low, targetvalues_upper), plot.all = reflimR.plot.all)
    }
    
    if (input$check_target == FALSE && input$check_targetvalues == FALSE) {
      reflim_result <- reflim(dat[, 4], plot.all = reflimR.plot.all)
    }
    
    reflim_result
    
    if(is.null(reflim_result$limits)){
      plot_is_empty <<- TRUE
    } else{
      plot_is_empty <<- FALSE
    }
  })
  
  output$console <- renderPrint({

    dat <- reflim_data()
    
    if(input$check_target == FALSE && input$check_targetvalues == FALSE){
      reflim_text <- reflim(dat[,4], plot.it = FALSE)
    }
    
    if(input$check_target){
      validate(need(input$target_low < input$target_upper, 
                    "(reflim) the upper target limit must be greater than the lower target limit."))
      
      reflim_text <- reflim(dat[,4], targets = c(input$target_low, input$target_upper), plot.it = FALSE)
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
      reflim_text <- reflim(dat[,4], targets = c(targetvalues_low, targetvalues_upper), plot.it = FALSE)
    }
    
    print(reflim_text)
  })
  
  output$messageOutput <- renderPrint({
    
    dat <- reflim_data()
    
    if (plot_is_empty) {
      "(reflim) n or n.trunc is too small where a minimum of 200 is required! Try another example!"
    }
  })
}
####################################### Run the application #######################################
shinyApp(ui = ui, server = server)
