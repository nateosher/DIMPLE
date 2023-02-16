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
                       sidebarPanel(
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
                       sidebarPanel(
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
                       sidebarPanel(
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
                       sidebarPanel(
                         selectInput('dm_to_plot', 'Select image ids to plot distance matrix', "", selected = ""),
                         selectInput('dist_metric', 'Select distance metric', c("JSD","JSDu","KLD"), selected = "JSD"),
                         selectInput('quantiles_or_none',"Separate Distance Matrices by Quantiles?",c("Y","N"),selected="N")
                       ),
                       mainPanel(
                         plotOutput('one_dm_plot')
                       )
                     )
                     
            ),
            tabPanel("Calculate Distance Matrices",
                     pageWithSidebar(
                       headerPanel(''),
                       sidebarPanel('Click "Submit" to begin calculation. May take several minutes.',
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
                       sidebarPanel(  
                         selectInput('heatmap_grouping_var', 'Select grouping variable', "", selected = "Group")
                       ),
                       mainPanel(
                         #pre(id = "console"),
                         plotOutput('heatmap')
                       )
                     )
                     
            )
          )
)

