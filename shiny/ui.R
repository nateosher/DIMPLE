#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


fluidPage(shinyjs::useShinyjs(),
          titlePanel("DistDist Shiny App"),
          tabsetPanel(
            tabPanel("Visualize Multiplex Experiment",
                     sidebarLayout(
                       sidebarPanel(
                         fileInput('file1', 'Choose .RDS containing MltplxExperiment object',
                                   accept=c('.RDS')),
                         "Or load the lung cancer data from the VectraPolarisData R package",
                         tags$br(),
                         actionButton("exampledata", "Use lung cancer data"),
                         tags$br(),
                         #actionButton("exampledata1", "Use example CRC data"),
                         tableOutput('contents'),
                         tags$br(),
                         selectInput("slide_ids_to_plot","Select slide ids to plot","",selected="",multiple=F),
                         
                         selectInput("dm_plot_mode","Select mode for distance matrix plot",c("heatmap","network"),selected="",multiple=F)
                         
                       ),
                       mainPanel(
                         plotOutput('ppplot'),
                         plotOutput('dm_plot')
                       )
                     ),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("cell_types_to_plot","Select cell types to plot intensities","",selected="",multiple=T)
                       ),
                       mainPanel(
                         
                         plotOutput('intensity_plot')
                         
                       )
                     )
            ),
            tabPanel("Statistical Inference",
                     sidebarLayout(
                       sidebarPanel(
                         
                         selectInput("group_factor","Select grouping factor","",selected="",multiple=F),
                         selectInput("covariates","Select covariates to adjust for","",selected="",multiple=T),
                         selectInput("agg","Select aggregating function",c("median","mean","max","min"),selected="",multiple=F)
                       ),
                       mainPanel(
                         plotOutput('pairwise_group_heat')
                       ),
                     ),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("cell_types1","Select cell type 1","",selected="",multiple=F),
                         selectInput("cell_types2","Select cell type 2","",selected="",multiple=F)
                       ),
                       mainPanel(
                         
                         plotOutput('boxplot'),
                         plotOutput('group_boxplot')
                         
                       )
                     )
            )
            #,
            # tabPanel("Plot Intensities",
            #          sidebarLayout(
            #            sidebarPanel(
            #              selectInput("slide_ids_to_plot","Select slide ids to plot","",selected=""),
            #              selectInput("cell_types_to_plot","Select cell types to plot","",selected="")
            #              
            #            ),
            #            mainPanel(
            #              plotOutput('intensity_plot')
            #            )
            #          )
            # )
            #,
            #   tabPanel("Plot Point Patterns",
            #            pageWithSidebar(
            #              headerPanel(''),
            #              sidebarPanel(tags$head(tags$style("#pp_contents{height:70vh !important;}")),
            #                selectInput('image_to_plot', 'Select image to plot', "", selected = "")  
            #                
            #              ),
            #              mainPanel(
            #                plotOutput('pp_contents')
            #              )
            #            )
            #   ),
            #   tabPanel("Plot Intensities",
            #            pageWithSidebar(
            #              headerPanel(''),
            #              sidebarPanel(tags$head(tags$style("#intens_plot{height:70vh !important;}")),
            #                numericInput('eps', 'Select Pixel Size', value=10,min=1,max=10000),
            #                numericInput('bw', 'Select Smoothing Bandwidth', value=30,min=1,max=10000),
            #                selectInput('intens_to_plot', 'Select image to plot intensities', "", selected = "")
            #              ),
            #              mainPanel(
            #                plotOutput('intens_plot')
            #              )
            #            )
            #            
            #   ),
            #   tabPanel("Plot Intensity Quantiles",
            #            pageWithSidebar(
            #              headerPanel(''),
            #              sidebarPanel(tags$head(tags$style("#quantile_intens_plot{height:70vh !important;}")),
            #                selectInput('quant_cell_type', 'Select cell type for mask', "", selected = ""),
            #                textInput('quantiles_from', 'Input quantiles from', value="0,25,50,75"),
            #                textInput('quantiles_to', 'Input quantiles to', value="25,50,75,100"),
            #                selectInput('quantile_to_plot', 'Select image to plot quantiles of intensities', "", selected = "")
            #              ),
            #              mainPanel(
            #                plotOutput('quantile_intens_plot')
            #              )
            #            )
            #            
            #   ),
            #   tabPanel("Plot Distance Matrices",
            #            pageWithSidebar(
            #              headerPanel(''),
            #              sidebarPanel(tags$head(tags$style("#one_dm_plot{height:70vh !important;}")),
            #                selectInput('dm_to_plot', 'Select image ids to plot distance matrix', "", selected = ""),
            #                selectInput('dist_metric', 'Select distance metric', c("JSD","Correlation","KLD"), selected = "JSD"),
            #                selectInput('quantiles_or_none',"Separate Distance Matrices by Quantiles?",c("Y","N"),selected="N"),
            #                selectInput('dm_plot_type',"Select plot type",c("heatmap","network"))
            #              ),
            #              mainPanel(
            #                plotOutput('one_dm_plot')
            #              )
            #            ),
            #           
            #   tabPanel("Plot Boxplots",
            #            pageWithSidebar(
            #              headerPanel(''),
            #              sidebarPanel( 
            #                
            #                selectInput('t1', 'Select cell type 1', "", selected = "tumor cells"),
            #                selectInput('t2', 'Select cell type 2', "", selected = "CD4+ T cells"),
            #                selectInput('grouping_var', 'Select grouping variable', "", selecte
            #d = "Patient")
            #              ),
            #              mainPanel(
            #                #pre(id = "console"),
            #                plotOutput('patient_boxplot'),
            #                plotOutput('typewise_boxplot')
            #              )
            #            )
            #            
            #   ),
            #   tabPanel("Plot grouped heatmap",
            #            pageWithSidebar(
            #              headerPanel(''),
            #              sidebarPanel(   tags$head(tags$style("#heatmap{height:70vh !important;}")),
            #                selectInput('heatmap_grouping_var', 'Select grouping variable', "", selected = "Group"),
            #                selectInput('agg_function', 'Select function to aggregate distances over patients', c("median","mean","max","min"))
            #              ),
            #              mainPanel(
            #                #pre(id = "console"),
            #                plotOutput('heatmap')
            #              )
            #            )
            #            
            # )
          )
)

