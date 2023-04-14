#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(shinythemes);library(waiter)



fluidPage(shinyjs::useShinyjs(),autoWaiter(),theme = shinytheme("slate"),
          titlePanel("DIMPLE"),
          tabsetPanel(
            tabPanel("Visualize Multiplex Object",
                     sidebarLayout(
                       sidebarPanel(
                         fileInput('file1', 'Choose .RDS containing MltplxExperiment',
                                   accept=c('.RDS')),

                         "Or push button to load the lung cancer data from the VectraPolarisData R package:",
                         tags$br(),
                         actionButton("exampledata", "Use lung cancer data"),
                         tags$br(),
                         tags$br(),
                         "A summary of the data will appear below once the data is loaded:",
                         tags$br(),
                         #actionButton("exampledata1", "Use example CRC data"),
                         
                         #tableOutput('contents'),
                         span(tableOutput("contents"), style="color:green"),
                         tags$br(),
                         selectInput("slide_ids_to_plot","Select slide id to plot","",selected="",multiple=F),
                         selectInput("y_n_quantile_mask","Plot quantile mask?",c("Y","N"),selected="N",multiple=F),
                         tags$br(),
                         selectInput("cell_types_to_plot","Select cell types to plot intensities","",selected="",multiple=T),
                         tags$br(),
                         selectInput("dm_plot_mode","Select mode for distance matrix plot",c("heatmap","network"),selected="",multiple=F),
                         selectInput("y_n_qdist","Plot distances stratified by quantile?",c("Y","N"),selected="N",multiple=F)
                         ),
                       mainPanel(
                         plotOutput('ppplot'),downloadButton("save_pp", "download plot"),
                         plotOutput('intensity_plot'),downloadButton("save_int", "download plot"),
                         plotOutput('dm_plot'),downloadButton("save_dm", "download plot")
                         #plotOutput('quantile_mask')

                       )
                     )
            ),
            tabPanel("Visualize Multiplex Experiment",
                     sidebarLayout(
                       sidebarPanel( "Test each pairwise distance for association with a patient-level covatiate from the patient metadata",
                         selectInput("strat_qdist","Stratified by quantiles?",c("Y","N"),selected="N",multiple=F),
                         selectInput("which_qdist","Which quantile?","",multiple=F),
                    
                         selectInput("group_factor","Select covariate to test","",selected="",multiple=F),
                         selectInput("var_type","Select type of variable",c("continous","categorical"),multiple=F),
                         selectInput("covariates","Select covariates to adjust for","",selected="",multiple=T),
                         selectInput("agg","Select aggregating function",c("median","mean","max","min"),selected="",multiple=F),
                         selectInput("adjust_counts","Adjust for cell type counts",c("Yes","No"),selected="",multiple=F)
                       ),
                       mainPanel(
                         plotOutput('pairwise_group_heat'),downloadButton("save_heat", "download plot")
                       ),
                     ),
                     sidebarLayout(
                       sidebarPanel("Choose two cell types to investigate the distribution of their distance with respect to the grouping factor",
                         selectInput("cell_types1","Select cell type 1","",selected="",multiple=F),
                         selectInput("cell_types2","Select cell type 2","",selected="",multiple=F)
                       ),
                       mainPanel(
                        
                         #plotOutput('boxplot'),
                         plotOutput('group_boxplot_or_cont'),downloadButton("save_scatter", "download plot")
                         
                       )
                     )
            )

          )
)

