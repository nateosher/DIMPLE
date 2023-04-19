#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(ggplot2);library(dplyr);library(here);library(spatstat);library(tidyr);library(purrr);library(fuzzyjoin)
library(devtools);library(ggpubr);library(viridis);library(waiter)
devtools::load_all()
source("helpers.R")


options(shiny.maxRequestSize = 100000*1024^2)


function(input, output, session) {
  
  experiment<-reactive({
     
    if(!is.null(input$file1)){
      inFile <- input$file1 
      exp <- readRDS(inFile$datapath)
    }else{
      req(input$exampledata)
      exp<-readRDS("lung_experiment_10_30_jsd_qdist.RDS")
    }
  
    updateSelectInput(session, inputId = 'slide_ids_to_plot', label = 'Select slide ids to plot', choices = exp$slide_ids, selected = "")
    updateSelectInput(session, inputId = 'cell_types1', label = 'Select first cell type', choices = unique(unlist(lapply(lapply(exp$mltplx_objects,'[[',3),'[[',2))), selected = "")
    updateSelectInput(session, inputId = 'cell_types2', label = 'Select second cell type', choices = unique(unlist(lapply(lapply(exp$mltplx_objects,'[[',3),'[[',2))), selected = "")
    if(!is.null(exp$mltplx_objects[[1]]$qdist)){
      updateSelectInput(session, inputId = 'which_qdist', label = 'Which quantile?', choices = unique(paste0(unlist(lapply(lapply(lapply(lapply(exp$mltplx_objects,'[[',5),'[',5),'[[',1),'[',3))%>%na.omit(),"-",unlist(lapply(lapply(lapply(lapply(exp$mltplx_objects,'[[',5),'[',5),'[[',1),'[',4))%>%na.omit())), selected = "")
      
    }
    
    if(!is.null(exp$metadata)){
      updateSelectInput(session, inputId = 'group_factor', label = 'Select covariate to test', choices = names(exp$metadata), selected = "")
      updateSelectInput(session, inputId = 'covariates', label = 'Select covariates to adjust for', choices = names(exp$metadata), selected = "")
    }
    return(exp)
  })
  
  # plot the message  
  output$contents <- renderPrint({ 
    experiment()
  })
  
  #point pattern plot 
  ppplot<-function(){
    req(experiment())
    req(input$slide_ids_to_plot)
    if(input$y_n_quantile_mask=="Y"){
      req(experiment1())
      req(input$slide_ids_to_plot)
      qdist1<-filter_mltplx_objects(experiment(),input$slide_ids_to_plot)[[1]]$quantile_dist
      p<-plot_quantile_mask(experiment(),qdist1$mask_type,cbind.data.frame(from=c(qdist1$quantiles[,3]),to=c(qdist1$quantiles[,4])),input$slide_ids_to_plot) 
    }else{
      p<-plot_ppp(experiment(),input$slide_ids_to_plot)
    }
    p
  }
  
  output$ppplot <- renderPlot({ 
    ppplot()
  })
  
  experiment1 <- reactive({ 
    req(experiment()) 
    req(input$slide_ids_to_plot)
    experiment<-experiment()
    exp1<-filter_mltplx_objects(experiment,input$slide_ids_to_plot)
    updateSelectInput(session, inputId = 'cell_types_to_plot', label = 'Select cell types to plot intensities', choices = unique(unlist(lapply(lapply(exp1,'[[',3),'[[',2))), selected = "")
    return(experiment)
  })
  
  
  #intensity plot  
  #only make available cell types that are in the selected image
  intensity_plot<-function(){req(experiment1())
    req(input$slide_ids_to_plot)
    req(input$cell_types_to_plot)
    plot_intensities(experiment(),types=input$cell_types_to_plot,slide_ids=input$slide_ids_to_plot)
  }
  
  output$intensity_plot <- renderPlot({ 
    intensity_plot()
 })
  
  dm_plot<-function(){
    req(experiment())
    req(input$slide_ids_to_plot)
    req(input$y_n_qdist)
    if(input$y_n_qdist=="Y"){
      plot_qdist(experiment(),input$slide_ids_to_plot,mode=input$dm_plot_mode)
    }else{
      plot_dist(experiment(),input$slide_ids_to_plot,mode=input$dm_plot_mode)
    }
  }
  
  output$dm_plot <- renderPlot({ 

    dm_plot()
  })
  
  
  output$save_pp <- downloadHandler(
    #filename="save.png",
    filename = "ppplot.pdf" , # variable with filename
    content = function(file) {
      #ggsave(ppplot(), filename = file)
    
     ggsave(file,plot=ppplot()) 
    
    })
  
  output$save_int <- downloadHandler(
    #filename="save.png",
    filename = "intensities.pdf" , # variable with filename
    content = function(file) {
      #ggsave(ppplot(), filename = file)
      ggsave(file,plot=intensity_plot())
    })
  
  output$save_dm <- downloadHandler(
    #filename="save.png",
    filename = "dm.pdf" , # variable with filename
    content = function(file) {
      #ggsave(ppplot(), filename = file)
      ggsave(file,plot=dm_plot())
    })

  agg_list<-list(mean,median,max,min)
  names(agg_list)<-c("mean","median","max","min")
  
  pairwise_group_heat<-function(){
    req(experiment())
    req(experiment()$metadata)
    req(input$var_type)
    req(input$strat_qdist)
    req(input$group_factor)
    adjust<-ifelse(input$adjust_counts=="Yes",TRUE,FALSE)
    if(input$strat_qdist=="Y"){
      req(input$which_qdist)

      lmdist<-lm_qdist(exp,input$group_factor,interval=input$which_qdist,agg_fun = agg_list[[input$agg]],covariates = input$covariates,adjust_counts = adjust)

    }else{
    
    lmdist<-lm_dist(experiment(),input$group_factor,agg_fun = agg_list[[input$agg]],covariates = input$covariates,adjust_counts = adjust)
    
    
      }
    
    plot_pairwise_group_heatmap(lmdist,p_val_col = "p.adj")
  }
  
  output$pairwise_group_heat <- renderPlot({ 
    pairwise_group_heat()
  })
  
  output$save_heat <- downloadHandler(
    #filename="save.png",
    filename = "heatmap.pdf" , # variable with filename
    content = function(file) {
      #ggsave(ppplot(), filename = file)
      #png(file)
      ggsave(file,plot=pairwise_group_heat())
      #dev.off()
    })

  
  # output$boxplot <- renderPlot({ 
  #   req(experiment())
  #   req(input$cell_types1)
  #   req(input$cell_types2)
  #   patient_boxplots(experiment(),input$cell_types1,input$cell_types2,grouping_var=input$group_factor)
  # })
  
  group_boxplot_or_cont<-function(){
    req(experiment())
    req(input$cell_types1)
    req(input$cell_types2)
    if(input$var_type=="categorical"){
      typewise_boxplots(experiment(),input$cell_types1,input$cell_types2,group_factor=input$group_factor)
    }else{
      plot_scatter_dist(experiment(),input$cell_types1,input$cell_types2,cont_var=input$group_factor,agg_fun=NULL,smooth="loess")
    }
    
  }
  
  output$group_boxplot_or_cont <- renderPlot({ 
    group_boxplot_or_cont()
  })
  
  
  output$save_scatter <- downloadHandler(
    #filename="save.png",
    filename = "scatter.pdf" , # variable with filename
    content = function(file) {
      #ggsave(ppplot(), filename = file)
      ggsave(file,plot=group_boxplot_or_cont())
    })

}








