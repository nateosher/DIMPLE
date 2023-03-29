#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(ggplot2);library(dplyr);library(here);library(spatstat);library(tidyr);library(purrr);library(fuzzyjoin)
library(devtools);library(ggpubr);library(viridis)
devtools::load_all()
source("helpers.R")


options(shiny.maxRequestSize = 100000*1024^2)


function(input, output, session) {
  
  # read in .rds file   
  experiment <- reactive({ 
    req(input$file1) 
    
    inFile <- input$file1 
    
    exp <- readRDS(inFile$datapath)
    
    updateSelectInput(session, inputId = 'slide_ids_to_plot', label = 'Select slide ids to plot', choices = exp$slide_ids, selected = "")
    updateSelectInput(session, inputId = 'cell_types1', label = 'Select first cell type', choices = unique(unlist(lapply(lapply(exp$mltplx_objects,'[[',3),'[[',2))), selected = "")
    updateSelectInput(session, inputId = 'cell_types2', label = 'Select second cell type', choices = unique(unlist(lapply(lapply(exp$mltplx_objects,'[[',3),'[[',2))), selected = "")
    
    if(!is.null(exp$metadata)){
      updateSelectInput(session, inputId = 'group_factor', label = 'Select grouping factor', choices = names(exp$metadata), selected = "")
      updateSelectInput(session, inputId = 'covariates', label = 'Select covariates to adjust for', choices = names(exp$metadata), selected = "")
    }
    
    
    return(exp)
  })
  
  experiment<-eventReactive(input$exampledata,{
    exp<-readRDS("lung_experiment_10_10_jsd.RDS")
    updateSelectInput(session, inputId = 'slide_ids_to_plot', label = 'Select slide ids to plot', choices = exp$slide_ids, selected = "")
    updateSelectInput(session, inputId = 'cell_types1', label = 'Select first cell type', choices = unique(unlist(lapply(lapply(exp$mltplx_objects,'[[',3),'[[',2))), selected = "")
    updateSelectInput(session, inputId = 'cell_types2', label = 'Select second cell type', choices = unique(unlist(lapply(lapply(exp$mltplx_objects,'[[',3),'[[',2))), selected = "")
    
    if(!is.null(exp$metadata)){
      updateSelectInput(session, inputId = 'group_factor', label = 'Select grouping factor', choices = names(exp$metadata), selected = "")
      updateSelectInput(session, inputId = 'covariates', label = 'Select covariates to adjust for', choices = names(exp$metadata), selected = "")
    }
    return(exp)
  })
  
  # experiment<-eventReactive(input$exampledata1,{
  #   exp<-readRDS("CRC_example_jsd_10_30.RDS")
  #   updateSelectInput(session, inputId = 'slide_ids_to_plot', label = 'Select slide ids to plot', choices = exp$slide_ids, selected = "")
  #   updateSelectInput(session, inputId = 'cell_types1', label = 'Select first cell type', choices = unique(unlist(lapply(lapply(exp$mltplx_objects,'[[',3),'[[',2))), selected = "")
  #   updateSelectInput(session, inputId = 'cell_types2', label = 'Select second cell type', choices = unique(unlist(lapply(lapply(exp$mltplx_objects,'[[',3),'[[',2))), selected = "")
  # 
  #   if(!is.null(exp$metadata)){
  #     updateSelectInput(session, inputId = 'group_factor', label = 'Select grouping factor', choices = names(exp$metadata), selected = "")
  #     updateSelectInput(session, inputId = 'covariates', label = 'Select covariates to adjust for', choices = names(exp$metadata), selected = "")
  #   }
  #   return(exp)
  # })
  
  # plot the message  
  output$contents <- renderPrint({ 
    experiment()
  })
  
  #point pattern plot 
  output$ppplot <- renderPlot({ 
    req(experiment())
    req(input$slide_ids_to_plot)
    p<-plot_ppp(experiment(),input$slide_ids_to_plot)
    print(p)
  })
  
  experiment1 <- reactive({ 
    req(experiment()) 
    req(input$slide_ids_to_plot)
    experiment<-experiment()
    exp1<-filter_mltplx_objects(experiment,input$slide_ids_to_plot)
    updateSelectInput(session, inputId = 'cell_types_to_plot', label = 'Select cell types', choices = unique(unlist(lapply(lapply(exp1,'[[',3),'[[',2))), selected = "")
    return(experiment)
  })
  
  #intensity plot  
  #only make available cell types that are in the selected image
  output$intensity_plot <- renderPlot({ 
    req(experiment1())
    req(input$slide_ids_to_plot)
    req(input$cell_types_to_plot)
    plot_intensities(experiment(),input$cell_types_to_plot,input$slide_ids_to_plot)
  })
  
  output$dm_plot <- renderPlot({ 
    req(experiment())
    req(input$slide_ids_to_plot)
    plot_dist(experiment(),input$slide_ids_to_plot,mode=input$dm_plot_mode)
  })
  
  agg_list<-list(mean,median,max,min)
  names(agg_list)<-c("mean","median","max","min")
  
  output$pairwise_group_heat <- renderPlot({ 
    req(experiment())
    req(experiment()$metadata)
    req(input$group_factor)
    lmdist<-lm_dist(experiment(),input$group_factor,agg_fun = agg_list[[input$agg]],covariates = input$covariates)
    
    plot_pairwise_group_heatmap(lmdist,p_val_col = "p.adj")
  })
  
  output$boxplot <- renderPlot({ 
    req(experiment())
    req(input$cell_types1)
    req(input$cell_types2)
    patient_boxplots(experiment(),input$cell_types1,input$cell_types2,grouping_var=input$group_factor)
  })
  
  output$group_boxplot <- renderPlot({ 
    req(experiment())
    req(input$cell_types1)
    req(input$cell_types2)
    typewise_boxplots(experiment(),input$cell_types1,input$cell_types2,group_factor=input$group_factor)
  })
  
  
  # metadata <- reactive({ 
  #   req(input$file2) 
  #   
  #   inFile <- input$file2 
  #   
  #   df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
  #                  quote = input$quote)
  #   
  #   updateSelectInput(session, inputId = 'md_image_id', label = 'Image ID',
  #                     choices = names(df), selected = "spots")
  #   updateSelectInput(session, inputId = 'patient_id', label = 'Patient ID',
  #                     choices = names(df), selected = "Patient")
  #   
  #   return(df)
  # })
  # 
  # output$contents_md <- renderTable({ 
  #   head(metadata())
  # })
  # 
  # 
  # data.frame<-reactive({
  #   
  #   df<-data()%>%select(.data[[input$xcol]],.data[[input$ycol]],.data[[input$type_id]],.data[[input$image_id]]) %>% group_by(.data[[input$image_id]])%>%
  #     mutate(x=as.numeric(.data[[input$xcol]]),y=as.numeric(.data[[input$ycol]]),cell_type=as.factor(.data[[input$type_id]]),image_id=as.factor(.data[[input$image_id]]))
  #   
  #   updateSelectInput(session, inputId = 'image_to_plot', label = 'Select image to plot',choices = unique(df$image_id))
  #   updateSelectInput(session, inputId = 'intens_to_plot', label = 'Select image to plot intensities',choices = unique(df$image_id))
  #   updateSelectInput(session, inputId = 'dm_to_plot', label = 'Select image id to plot distance matrix',choices = unique(df$image_id))
  #   updateSelectInput(session, inputId = 't1', label = 'Select cell type 1',choices = unique(df$cell_type))
  #   updateSelectInput(session, inputId = 't2', label = 'Select cell type 2',choices = unique(df$cell_type))
  #   updateSelectInput(session, inputId = 'quant_cell_type', label = 'Select cell type for mask',choices = unique(df$cell_type))
  #   updateSelectInput(session, inputId = 'quantile_to_plot', label = 'Select image to plot quantiles of intensities',choices =  unique(df$image_id))
  #   return(df)
  # })
  # 
  # md.data.frame<-reactive({
  #   
  #   df<-metadata()%>%mutate(slide_id=as.factor(.data[[input$md_image_id]]),patient_id=as.factor(.data[[input$patient_id]]))
  #   updateSelectInput(session, inputId = 'grouping_var', label = 'Select grouping variable',choices = names(df))
  #   updateSelectInput(session, inputId = 'heatmap_grouping_var', label = 'Select grouping variable',choices = names(df))
  #   updateSelectInput(session, inputId = 'surv_time', label = 'Select survival time variable',choices = names(df))
  #   updateSelectInput(session, inputId = 'surv_event', label = 'Select survival event variable',choices = names(df))
  #   return(df)
  # })
  # 
  # fake_experiment<-reactive({
  #   exp <- build_mltplx_exp(2000,seed=1,n_slides=20)
  #   exp <- add_mltplx_metadata(exp,n_patients=10)
  #   exp$metadata$survival_time<-rexp(dim(exp$metadata)[1],1)
  #   exp$metadata$survival_event<-sample(c(0,1),dim(exp$metadata)[1],replace=T)
  #   #exp <- update_intensity(exp,ps=2,bw=3)
  #   #exp <- update_dist(exp,cor)
  #   
  #   updateSelectInput(session, inputId = 'image_to_plot', label = 'Select image to plot',choices = exp$slide_ids, selected = "S1")
  #   updateSelectInput(session, inputId = 'intens_to_plot', label = 'Select image to plot intensities',choices = exp$slide_ids, selected = "S1")
  #   updateSelectInput(session, inputId = 'dm_to_plot', label = 'Select image id to plot distance matrix',choices =exp$slide_ids)
  #   updateSelectInput(session, inputId = 't1', label = 'Select cell type 1',choices = exp$mltplx_objects[[1]]$mltplx_image$cell_types,selected="X1")
  #   updateSelectInput(session, inputId = 't2', label = 'Select cell type 2',choices = exp$mltplx_objects[[1]]$mltplx_image$cell_types,selected="X2")
  #   updateSelectInput(session, inputId = 'quant_cell_type', label = 'Select cell type for mask',choices = exp$mltplx_objects[[1]]$mltplx_image$cell_types,selected="X1")
  #   updateSelectInput(session, inputId = 'quantile_to_plot', label = 'Select image to plot quantiles of intensities',choices =  exp$slide_ids, selected = "S1")
  #   
  #   updateSelectInput(session, inputId = 'grouping_var', label = 'Select grouping variable',choices = names(exp$metadata),selected="patient_id")
  #   updateSelectInput(session, inputId = 'heatmap_grouping_var', label = 'Select grouping variable',choices = names(exp$metadata),selected="group")
  #   updateSelectInput(session, inputId = 'surv_time', label = 'Select survival time variable',choices = names(exp$metadata))
  #   updateSelectInput(session, inputId = 'surv_event', label = 'Select survival event variable',choices = names(exp$metadata))
  #   return(exp)
  # })
  # 
  # 
  # 
  # output$pp_contents <- renderPlot({   
  #   if(!is.null(input$file1)){
  #     df<-data.frame()%>%filter(image_id==input$image_to_plot)
  #     req(input$image_to_plot%in%df$image_id)
  #     obj<-new_MltplxExperiment(
  #       x = df$x,
  #       y = df$y,
  #       marks = df$cell_type,
  #       slide_id = df$image_id)
  #   }else{
  #     req(input$image_to_plot%in%fake_experiment()$slide_ids)
  #     obj<-fake_experiment()
  #     
  #   }
  # 
  #   plot_ppp(obj,input$image_to_plot)
  # 
  #   
  #   
  # })
  # 
  # 
  # output$intens_plot <- renderPlot({
  #   if(!is.null(input$file1)){
  #   df<-data.frame()%>%filter(image_id==input$intens_to_plot)
  #   req(input$intens_to_plot%in%df$image_id)
  #   intens_data<-new_MltplxExperiment(
  #     x = df$x,
  #     y = df$y,
  #     marks = df$cell_type,
  #     slide_id = df$image_id,ps=input$eps,bw=input$bw )
  #   }else{
  #   req(input$image_to_plot%in%fake_experiment()$slide_ids)
  #   exp<-fake_experiment()
  #   
  #   intens_data <- update_intensity(exp,ps=input$eps,bw=input$bw)
  #   }
  #  
  #   plot_intensities(intens_data,types=unique(intens_data$mltplx_objects[[1]]$mltplx_image$cell_types),input$intens_to_plot)
  #   
  # })
  # 
  # func_list<-list(cor,jensen_shannon_dist,KL_div)
  # names(func_list)<-c("Correlation","JSD","KLD")
  # 
  # output$one_dm_plot<- renderPlot({
  #   if(input$quantiles_or_none=="N"){
  #     if(!is.null(input$file1)){
  #       df<-data.frame()%>%filter(image_id==input$dm_to_plot)
  #       df$cell_type<-factor(df$cell_type,levels=unique(df$cell_type))
  #       req(input$dm_to_plot%in%df$image_id)
  #       distance_data<-new_MltplxExperiment(
  #         x = df$x,
  #         y = df$y,
  #         marks = df$cell_type,
  #         slide_id = df$image_id,ps=input$eps,bw=input$bw, dist_metric=func_list[[input$dist_metric]])
  #       
  #       
  #     }else{
  #       exp<-fake_experiment()
  #       intens_data <- update_intensity(exp,ps=input$eps,bw=input$bw)
  #       distance_data<- update_dist(intens_data,dist_metric=func_list[[input$dist_metric]])
  #     }
  #     plot_dist(distance_data,input$dm_to_plot, mode = input$dm_plot_type)
  #     
  #   }else{
  #     if(!is.null(input$file1)){
  #     df<-data.frame()%>%filter(image_id==input$dm_to_plot)
  #     df$cell_type<-factor(df$cell_type,levels=unique(df$cell_type))
  #     req(input$dm_to_plot%in%df$image_id)
  #     distance_data<-new_MltplxExperiment(
  #       x = df$x,
  #       y = df$y,
  #       marks = df$cell_type,
  #       slide_id = df$image_id,ps=input$eps,bw=input$bw)
  #     from <- as.numeric(unlist(strsplit(input$quantiles_from,",")))
  #     to <- as.numeric(unlist(strsplit(input$quantiles_to,",")))
  #     q_probs<-cbind.data.frame(from,to)
  #     distance_data<-add_QuantileDist(distance_data, dist_metric=func_list[[input$dist_metric]],mask_type=input$quant_cell_type,q_probs)
  #     }else{
  #       exp<-fake_experiment()
  #       
  #       intens_data <- update_intensity(exp,ps=input$eps,bw=input$bw)
  #       distance_data<- update_dist(intens_data,dist_metric=func_list[[input$dist_metric]])
  #       distance_data<-filter_mltplx_objects(distance_data,input$dm_to_plot)[[1]]
  #       from <- as.numeric(unlist(strsplit(input$quantiles_from,",")))
  #       to <- as.numeric(unlist(strsplit(input$quantiles_to,",")))
  #       q_probs<-cbind.data.frame(from,to)
  #       
  #       distance_data<-add_QuantileDist(distance_data, dist_metric=func_list[[input$dist_metric]],mask_type=input$quant_cell_type,q_probs)
  #       
  #     }
  #     #plot_dist(distance_data,input$dm_to_plot, mode = "heatmap",plot_by_quantile=T)
  #     plot_qdist(distance_data,input$dm_to_plot, mode = input$dm_plot_type)
  #   }
  #   
  # })
  # 
  # output$quantile_intens_plot<-renderPlot({
  #   if(!is.null(input$file1)){
  #      df<-data.frame()%>%filter(image_id==input$quantile_to_plot)
  #      req(input$quantile_to_plot%in%df$image_id)
  #      intens_data<-new_MltplxExperiment(
  #        x = df$x,
  #        y = df$y,
  #        marks = df$cell_type,
  #        slide_id = df$image_id,ps=input$eps,bw=input$bw)
  #      from <- as.numeric(unlist(strsplit(input$quantiles_from,",")))
  #      to <- as.numeric(unlist(strsplit(input$quantiles_to,",")))
  #      q_probs<-cbind.data.frame(from,to)
  #     
  #     plot_quantile_mask(intens_data,input$quant_cell_type,q_probs,input$quantile_to_plot)
  #   }else{
  #     exp<-fake_experiment()
  #     intens_data <- update_intensity(exp,ps=input$eps,bw=input$bw)
  #     from <- as.numeric(unlist(strsplit(input$quantiles_from,",")))
  #     to <- as.numeric(unlist(strsplit(input$quantiles_to,",")))
  #     q_probs<-cbind.data.frame(from,to)
  # 
  #     plot_quantile_mask(intens_data,input$quant_cell_type,q_probs,input$quantile_to_plot)
  #   }
  # 
  # })
  # 
  # out <- reactive({
  #   
  #   shinyjs::disable("submit")
  #   if(!is.null(input$file1)){
  #     req(data.frame())
  #     df<-data.frame()
  #     if(!is.null(input$file2)){
  #       meta<-md.data.frame()
  #       
  #       if(input$quantiles_or_none=="N"){
  #         obj<-new_MltplxExperiment(
  #           x = df$x,
  #           y = df$y,
  #           marks = df$cell_type,
  #           slide_id = df$image_id,metadata=meta,ps=input$eps,bw=input$bw,func_list[[input$dist_metric]])
  #         
  #       }else{
  #         obj<-new_MltplxExperiment(
  #           x = df$x,
  #           y = df$y,
  #           marks = df$cell_type,
  #           slide_id = df$image_id,metadata=meta,ps=input$eps,bw=input$bw,dist_metric = func_list[[input$dist_metric]])
  #         from <- as.numeric(unlist(strsplit(input$quantiles_from,",")))
  #         to <- as.numeric(unlist(strsplit(input$quantiles_to,",")))
  #         q_probs<-cbind.data.frame(from,to)
  #         obj<-add_QuantileDist(obj, dist_metric=func_list[[input$dist_metric]],mask_type=input$quant_cell_type,q_probs)
  #       }
  #       
  #       
  #       
  #     }else{
  #       if(input$quantiles_or_none=="N"){
  #         obj<-new_MltplxExperiment(
  #           x = df$x,
  #           y = df$y,
  #           marks = df$cell_type,
  #           slide_id = df$image_id,ps=input$eps,bw=input$bw,dist_metric = func_list[[input$dist_metric]]) 
  #       }else{
  #         from <- as.numeric(unlist(strsplit(input$quantiles_from,",")))
  #         to <- as.numeric(unlist(strsplit(input$quantiles_to,",")))
  #         obj<-new_MltplxExperiment(
  #           x = df$x,
  #           y = df$y,
  #           marks = df$cell_type,
  #           slide_id = df$image_id,ps=input$eps,bw=input$bw,dist_metric = func_list[[input$dist_metric]])
  #         q_probs<-cbind.data.frame(from,to)
  #         obj<-add_QuantileDist(obj, dist_metric=func_list[[input$dist_metric]],mask_type=input$quant_cell_type,q_probs)
  #         
  #         
  #       }
  #     }
  #   }else{
  #     if(input$quantiles_or_none=="N"){
  #     exp<-fake_experiment()
  #     intens_data <- update_intensity(exp,ps=input$eps,bw=input$bw)
  #     obj<- update_dist(intens_data,dist_metric=func_list[[input$dist_metric]])
  #     }else{
  #       exp<-fake_experiment()
  #       intens_data <- update_intensity(exp,ps=input$eps,bw=input$bw)
  #       distance_data<- update_dist(intens_data,dist_metric=func_list[[input$dist_metric]])
  #       from <- as.numeric(unlist(strsplit(input$quantiles_from,",")))
  #       to <- as.numeric(unlist(strsplit(input$quantiles_to,",")))
  #       q_probs<-cbind.data.frame(from,to)
  #       obj<-add_QuantileDist(distance_data, dist_metric=func_list[[input$dist_metric]],mask_type=input$quant_cell_type,q_probs)
  #     }
  #   }
  #   
  #   
  #   
  #   
  #   return(obj)
  #   
  # })
  # 
  # 
  # observeEvent(input$submit, {
  #   withCallingHandlers({
  #     shinyjs::html("text", "")
  #     req(out())
  #   },
  #   message = function(m) {
  #     shinyjs::html(id = "text", html = paste0(m$message), add = FALSE)
  #   })
  #   shinyjs::enable("submit")
  # })
  # 
  # 
  # output$dm_plot <- renderPlot({
  #   req(out())
  #   plot_dist(out(), input$dm_to_plot, mode = "heatmap")
  # })
  # 
  # output$patient_boxplot<-renderPlot(({
  #   req(out())
  #   patient_boxplots(out(),input$t1,input$t2,grouping_var=input$grouping_var,label_spots=FALSE)
  #   
  # }))
  # 
  # output$typewise_boxplot<-renderPlot(({
  #    req(out())
  #    typewise_boxplots(out(),input$t1,input$t2,group_factor=input$grouping_var)
  # }))
  # 
  # agg_func_list<-list(median,mean,max,min)
  # names(agg_func_list)<-c("median","mean","max","min")
  # 
  # output$heatmap<-renderPlot(({
  #   req(out())
  #   df<-lm_dist(out(),group_factor = input$heatmap_grouping_var,agg_fun = agg_func_list[[input$agg_function]])
  #   plot_pairwise_group_heatmap(df)
  # }))
  # 
  # 
  
}








