library('shinydashboard')
library('shinyBS')   # for tooltip
library('shinyjs')   # for hiding/showing objects
library('plotly')


model_names <- c("Naslund",
                 "Curtis",
                 "Michailoff",
                 "Meyer",
                 "Power",
                 "Naslund 2",
                 "Naslund 3",
                 "Naslund 4",
                 "Micment",
                 "Micment2",
                 "Wykoff",
                 "Prodan",
                 "Logistic",
                 "Richards",
                 "Weibull",
                 "Gomperz",
                 "Sibbesen",
                 "Korf",
                 "Ratkowsky",
                 "Hossfeld IV")

# https://stackoverflow.com/questions/44958495/coloring-the-checkboxgroupinput-choices?rq=1
my.colors <- c('black','black','black','black','black','black','black','black','black','black','black',
               'blue','blue','blue','blue','blue','blue','blue','blue','blue')

my.fun <- function() {
  res <- list()
  for (o in model_names) {
    res[[length(res)+1]] <- tags$span(o, 
        style = paste0('color: ', my.colors[which(model_names == o)],';'))
  }
  res
}

# https://stackoverflow.com/questions/29738975/how-to-align-a-group-of-checkboxgroupinput-in-r-shiny
# tweaks, a list object to set up multicols for checkboxGroupInput
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 150px;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ))

all_rows <- 1:20
names(all_rows) <- model_names 

# data control: the checkboxes will control the data values plotted
controls <-
  list( tags$strong("Select model (max. 3)"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'numSelector', 
                                   label    = "...", #Select models:", 
#                                   choices  = all_rows,
                                   choiceNames = my.fun(),
                                   choiceValues = 1:20, #my.colors,
                                   selected = NULL,
                                   inline   = FALSE,
                                   width    = "100%"))) 


ui <- function(request) {
  
  
  header <- dashboardHeader(
     title = "Tree DBH-Height Modelling Tool (v. 1.0)",
     titleWidth = 450
  )
  
  
  # https://stackoverflow.com/questions/43827728/aligning-checkboxes-in-shiny

  stext1 = "Give zero (0) to get maximum value from input data" 
  stext2 = paste0("* 0, then a model without random parameters is fitted. Results to a ",
                  "fixed-effects model, and argument <i>plot</i> is not used. <br/>",
                  "* 1, then parameter a of a nonlinear function or the first coefficient ",
                  "of the linear formula is assumed to vary among plots. <br/>",
                  "* 2, then a and b or the first two terms of the linear formula are ",
                  "assumed to vary among plots or <br/>",
                  "* 3, then a b, and c of a three-parameter nonlinear model or three ",
                  "first coefficients of a linear model are assumed to vary among plots")
  stext3 =  "CSV file should contain 4 fields: cluster, plot, tree_dbh, tree_height"

  stext4 =  paste0("In <i>lmfor</i> manual, this selection refers to function <i>ImputeHeights</i>, varf= TRUE/FALSE <br/>",  
                  "The parametric variance function may improve the model fit particularly for heteroscedastic data, read more in CJFR article (link on ABOUT sheet) "
                  )
  stext5 = paste0("Upper graph:  Plot calibration. <br/>",
                  "Middle graph: Cluster calibration. <br/>",
                  "Lower graph:  Fixed part model. <br/><br/>",
                  "s.e.= Standard error (in meters)"
                  )
  
  
  body <- dashboardBody(
    # bsTooltip texts start ****************
    bsTooltip(id = "d_max",    title = stext1, placement = "top", trigger = "hover"),
    bsTooltip(id = "h_max",    title = stext1, placement = "top", trigger = "hover"),
    bsTooltip(id = "sel_nrap", title = HTML(stext2), placement = "top", trigger = "hover", options = list(container = "body")),
    bsTooltip(id = "check_variance", title = HTML(stext4), placement = "top", trigger = "hover", options = list(container = "body")),
    
    bsTooltip(id = "image_residuals1", title = stext5, placement = "top", trigger = "hover"),

    
    # Tooltip texts end   ****************
    shinyjs::useShinyjs(),

    navbarPage(
      "", 

      tabPanel("Application",

               sidebarPanel(
                 tweaks,
                 fileInput("file1", "Upload CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                h6(stext3),

                fluidRow(column(width = 9, controls)),
                
#                tags$hr(style="border-color: black;"),
                tags$br(),
                fluidRow(
                   column(6,
                      selectInput("sel_nrap", "Random parameters", 
                        choices= c("0", "1", "2", "3"), selected="3")),
                   column(6,
                      checkboxInput("check_variance", "Variance function used", value = FALSE)),
                ),
                fluidRow(
                  column(6,
                      numericInput("d_max", "Max. DBH in graphs (cm)", 0 )),
                        #    verbatimTextOutput("txtout0")),
                  column(6,
                      numericInput("h_max", "Max. height in graphs (m)", 0 )),
#                      checkboxInput("check_variance", "Variance function used", value = FALSE)), 
                ),
                # Input: Specification of range within an interval ----
                # conditionalPanel(
                #   condition = "output$fileUploaded",
                #   sliderInput("range", "DBH range for analysis:",
                #     min = 1, max = 1000,
                #     value = c(0,500))),

                 actionButton("action_plot", "Run estimatation", class = "btn-primary"),
                
                width=5
               ),
               
               mainPanel(
                 tabsetPanel(
                    tabPanel("Data",
                            tags$br(),
                            radioButtons("data_color", "Colors by:",
                                         c("Plots"    = "plot_id",
                                           "Clusters" = "cluster")),
                            plotly::plotlyOutput("plot1"),
                            hr(),
                            verbatimTextOutput("summary"),
                            #   h4("Verbatim text output"),
#                            verbatimTextOutput("txtout1"),
#                            p(tags$h5("Relative standard error (RSE) curve non-linear model parametes:")),
#                            verbatimTextOutput("txtout2")
                            ),
                    tabPanel("Input data", 
                            tags$br(),
                            DT::dataTableOutput("contents")
                            ),
                    tabPanel("Residual plots",
                        fluidRow(
                             column(width=4, imageOutput("image_residuals1")),
                             column(width=4, imageOutput("image_residuals2")),
                             column(width=4, imageOutput("image_residuals3"))
                            )),   
                    tabPanel("Fitted curves", 
                            tags$br(),
                            imageOutput("image_curves")),
                       tabPanel("Predictions -1",
                                fluidRow(      # lower row
                                  column(width=12, plotOutput("plot_estimates4"))
                                ),
                                fluidRow(      # upper row
                                  column(width=12, plotOutput("plot_estimates1"))
                                ),
                                downloadButton('downloadData1', 'Download data')
                       ),
                       tabPanel("Predictions -2",
                                fluidRow(      # lower row
                                  column(width=12, plotOutput("plot_estimates5"))
                                ),
                                fluidRow(      # upper row
                                  column(width=12, plotOutput("plot_estimates2"))
                                ),
                                downloadButton('downloadData2', 'Download data')
                       ),
                       tabPanel("Predictions -3",
                                fluidRow(      # lower row
                                  column(width=12, plotOutput("plot_estimates6"))
                                ),
                                fluidRow(      # upper row
                                  column(width=12, plotOutput("plot_estimates3"))
                                ),
                                downloadButton('downloadData3', 'Download data')         
                    ),
                    id="tabs"), width = 7,   # tabsetPanel
               )# mainPanel
      ),
      tabPanel("About", 
               verbatimTextOutput("txtout3"),
               p(tags$h5("This application uses lmfor package created by Prof. Lauri Mehtatalo. ")),
               htmlOutput("html_link"),
               tags$br(),
               htmlOutput("CJFR_link"),
               tags$br(),
               htmlOutput("html_link2"),
               tags$br(),
               htmlOutput("html_link3"),
               tags$br(),tags$br(),
               verbatimTextOutput("txtout5"),
               p(tags$h6("Shiny application compiled by Lauri Vesa, Forestry Department, FAO" )),
      )
    ))
    dashboardPage(
      skin='green',
      header,
      dashboardSidebar(disable = TRUE),
      body
    )
}