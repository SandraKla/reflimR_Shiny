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

####################################### User Interface ############################################

ui <- dashboardPage(
  dashboardHeader(title = "reflimR", titleWidth = 350),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "sidebarid",
      
      div(
        style = "text-align:center",
        br(),
        "Estimation of reference limits", br(),
        "from routine laboratory results", hr(),
      ),
      
      fileInput(
        "data_table",
        "Upload CSV File:",
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      
      selectInput(
        "sex",
        "Select the sex:",
        choices = c("Female (F)" = "f", "Male (M)" = "m")
      ),
      
      hr(),
      
      checkboxInput("check_target", "Target values"),
      
      conditionalPanel(
        condition = "input.check_target == true",
        
          numericInput(
            "target_low",
            "Target lower value:",
            10,
            min = 0,
            max = 10000
          ),
      
          numericInput(
            "target_upper",
            "Target upper value:",
            15,
            min = 0,
            max = 10000
          )
        ),
      
      hr(),
      
      checkboxInput("check_plot.all", "View all plots"),
      
      hr(),
      
      div(
        style = "text-align:center",
        "For further information visit our R-package",
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
        
        p(
          "This Shiny App is based on the package reflimR for the estimation of reference limits from routine laboratory results."
        ),
        
        plotOutput("plot", height = "700px")
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
  
  ##################################### Reactive Expressions ######################################
  
  get_data_file <- reactive({

    saving <- 
      if(!is.null(input$data_table)){
        #dataset_original <- read.csv(input$data_table[["datapath"]],na.strings="", fileEncoding="latin1")
      }else{
        dataset_original <- reflimR::livertests
        dataset_original <- dataset_original[c(1,2,3,6)]
      }
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
    
    if (input$check_plot.all == FALSE) {
      if (input$check_target) {
        validate(
          need(
            input$target_low < input$target_upper,
            "(reflim) the upper target limit must be greater than the lower target limit."
          )
        )
        reflim(dat[, 4], target = c(input$target_low, input$target_upper))
      } else{
        reflim(dat[, 4])
      }
    } else{
      if (input$check_target) {
        validate(
          need(
            input$target_low < input$target_upper,
            "(reflim) the upper target limit must be greater than the lower target limit."
          )
        )
        reflim(dat[, 4], target = c(input$target_low, input$target_upper), plot.all = TRUE)
      } else{
        reflim(dat[, 4], plot.all = TRUE)
      }
    }
  })
  
  output$console <- renderPrint({

    dat <- reflim_data()
    
    if(input$check_target){
      validate(need(input$target_low < input$target_upper, 
                    "(reflim) the upper target limit must be greater than the lower target limit."))
      reflim(dat[,4], target = c(input$target_low, input$target_upper), plot.it = FALSE)
    } else{
      reflim(dat[,4], plot.it = FALSE)
    }
  })
}
####################################### Run the application #######################################
shinyApp(ui = ui, server = server)
