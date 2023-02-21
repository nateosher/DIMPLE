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
            tabPanel("Upload Data",
                     titlePanel("Upload file and select variables"),
                     sidebarLayout(
                       sidebarPanel(
                         fileInput('file1', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         
                         
                         tags$br(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"'),
                         selectInput('xcol', 'X', ""),
                         selectInput('ycol', 'Y', "", selected = ""),
                         selectInput('image_id', 'Image ID', "", selected = ""),
                         selectInput('type_id', 'Cell Type', "", selected = "")
                         
                       ),
                       mainPanel(
                         tableOutput('contents')
                       )
                     )
            ),
            tabPanel("Upload Metadata",
                     titlePanel("Upload file and select variables (Optional)"),
                     sidebarLayout(
                       sidebarPanel(
                         fileInput('file2', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         
                         
                         tags$br(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"'),
                         selectInput('md_image_id', 'Image ID', ""),
                         selectInput('patient_id', 'Patient ID', "", selected = "")
                         #selectInput('surv_time', 'Survival Time', "", selected = ""),
                         #selectInput('surv_event', 'Survival Event', "", selected = "")
                       ),
                       mainPanel(
                         tableOutput('contents_md')
                       )
                     )
            ),
            tabPanel("Plot Point Patterns",
                     pageWithSidebar(
                       headerPanel(''),
                       sidebarPanel(tags$head(tags$style("#pp_contents{height:70vh !important;}")),
                         selectInput('image_to_plot', 'Select image to plot', "", selected = "")  
                         
                       ),
                       mainPanel(
                         plotOutput('pp_contents')
                       )
                     )
            ),
            tabPanel("Select Intensity Parameters",
                     pageWithSidebar(
                       headerPanel(''),
                       sidebarPanel(tags$head(tags$style("#intens_plot{height:70vh !important;}")),
                         numericInput('eps', 'Select Pixel Size', value=10,min=1,max=10000),
                         numericInput('bw', 'Select Smoothing Bandwidth', value=30,min=1,max=10000),
                         selectInput('intens_to_plot', 'Select image to plot intensities', "", selected = "")
                       ),
                       mainPanel(
                         plotOutput('intens_plot')
                       )
                     )
                     
            ),
            tabPanel("Select Intensity Quantiles",
                     pageWithSidebar(
                       headerPanel(''),
                       sidebarPanel(tags$head(tags$style("#quantile_intens_plot{height:70vh !important;}")),
                         selectInput('quant_cell_type', 'Select cell type for mask', "", selected = ""),
                         textInput('quantiles_from', 'Input quantiles from', value="0,25,50,75"),
                         textInput('quantiles_to', 'Input quantiles to', value="25,50,75,100"),
                         selectInput('quantile_to_plot', 'Select image to plot quantiles of intensities', "", selected = "")
                       ),
                       mainPanel(
                         plotOutput('quantile_intens_plot')
                       )
                     )
                     
            ),
            tabPanel("Select Distance",
                     pageWithSidebar(
                       headerPanel(''),
                       sidebarPanel(tags$head(tags$style("#one_dm_plot{height:70vh !important;}")),
                         selectInput('dm_to_plot', 'Select image ids to plot distance matrix', "", selected = ""),
                         selectInput('dist_metric', 'Select distance metric', c("JSD","Correlation","KLD"), selected = "JSD"),
                         selectInput('quantiles_or_none',"Separate Distance Matrices by Quantiles?",c("Y","N"),selected="N"),
                         selectInput('dm_plot_type',"Select plot type",c("heatmap","network"))
                       ),
                       mainPanel(
                         plotOutput('one_dm_plot')
                       )
                     )
                     
            ),
            tabPanel("Calculate Distance Matrices",
                     pageWithSidebar(
                       headerPanel(''),
                       sidebarPanel(
                         'Click "Submit" to begin calculation. May take several minutes.',
                                    actionButton("submit", label = "Submit")
                       ),
                       mainPanel(
                         #pre(id = "console"),
                         textOutput("text")
                       )
                     )
                     
            ),
            tabPanel("Plot Boxplots",
                     pageWithSidebar(
                       headerPanel(''),
                       sidebarPanel( 
                         
                         selectInput('t1', 'Select cell type 1', "", selected = "tumor cells"),
                         selectInput('t2', 'Select cell type 2', "", selected = "CD4+ T cells"),
                         selectInput('grouping_var', 'Select grouping variable', "", selected = "Patient")
                       ),
                       mainPanel(
                         #pre(id = "console"),
                         plotOutput('patient_boxplot'),
                         plotOutput('typewise_boxplot')
                       )
                     )
                     
            ),
            tabPanel("Plot grouped heatmap",
                     pageWithSidebar(
                       headerPanel(''),
                       sidebarPanel(   tags$head(tags$style("#heatmap{height:70vh !important;}")),
                         selectInput('heatmap_grouping_var', 'Select grouping variable', "", selected = "Group"),
                         selectInput('agg_function', 'Select function to aggregate distances over patients', c("median","mean","max","min"))
                       ),
                       mainPanel(
                         #pre(id = "console"),
                         plotOutput('heatmap')
                       )
                     )
                     
            ),
            tabPanel("Plot survival heatmap",
                     pageWithSidebar(
                       headerPanel(''),
                       sidebarPanel(  tags$head(tags$style("#surv_heatmap{height:70vh !important;}")),
                         selectInput('surv_time', 'Select survival time variable', "", selected = "Group"),
                         selectInput('surv_event', 'Select survival event variable', "", selected = "Group"),
                         selectInput('agg_function1', 'Select function to aggregate distances over patients', c("median","mean","max","min"))
                       ),
                       mainPanel(
                         #pre(id = "console"),
                         plotOutput('surv_heatmap')
                       )
                     )
                     
            )
          )
)

