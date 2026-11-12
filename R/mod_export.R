export_figure <- function(fig,path,format='png',width=12,height=6,dpi=300) {
 if(!format %in% c('png','svg','pdf')) fail('Unsupported figure format.')
 if(any(!is.finite(c(width,height,dpi))) || min(width,height,dpi)<=0) fail('Figure dimensions and resolution must be positive.')
 device <- switch(format,png='png',svg=svglite::svglite,pdf=grDevices::cairo_pdf)
 ggplot2::ggsave(path,fig$static,device=device,width=width,height=height,dpi=dpi,bg='white',limitsize=TRUE)
}
export_ui <- function(id) {
 ns <- shiny::NS(id)
 shiny::fluidRow(shiny::column(4,shiny::h3('Publication figures'),
 shiny::numericInput(ns('width'),'Width (inches)',12,min=1),shiny::numericInput(ns('height'),'Height (inches)',7,min=1),
 shiny::numericInput(ns('dpi'),'PNG resolution (DPI)',300,min=72),
 shiny::downloadButton(ns('png'),'PNG'),shiny::downloadButton(ns('svg'),'SVG'),shiny::downloadButton(ns('pdf'),'PDF'),
 shiny::helpText('Exports use the same cached panel objects as the comparison workspace, with the applied axes and graph settings.'),
 shiny::h3('Results & observations'),shiny::downloadButton(ns('csv'),'Results CSV'),shiny::downloadButton(ns('xlsx'),'Results XLSX'),shiny::downloadButton(ns('data'),'Canonical data CSV')),
 shiny::column(4,shiny::h3('Local projects'),
 shiny::downloadButton(ns('save'),'Save complete project (.json)'),shiny::fileInput(ns('load'),'Reopen a StatLab project',accept='.json'),
 shiny::helpText('Portable, versioned JSON. Data is processed locally; projects contain no executable R code.'),
 shiny::h3('Reusable graph / axis presets'),shiny::textInput(ns('preset_name'),'Preset name','My figure'),
 shiny::actionButton(ns('store'),'Save preset'),shiny::selectInput(ns('preset'),'Saved presets',choices=character()),
 shiny::actionButton(ns('restore'),'Apply preset')),
 shiny::column(4,shiny::h3('Reproducibility'),shiny::verbatimTextOutput(ns('session'))))
}
normalize_settings <- function(value,template) {
 if(is.null(value)) return(template)
 out <- template;out[names(value)] <- value
 for(nm in intersect(names(value),names(template))) {
  v <- value[[nm]];if(is.null(v)) {out[[nm]] <- template[[nm]];next}
  if(is.data.frame(template[[nm]])) out[[nm]] <- normalize_table(v,template[[nm]],if(nm=='annotations') annotation_row() else NULL)
  else if(is.list(template[[nm]])) out[[nm]] <- normalize_settings(v,template[[nm]])
  else {
   item <- if(!length(v)) template[[nm]][0] else unlist(v,use.names=FALSE)
   if(is.numeric(template[[nm]]) && (is.numeric(item) || (is.logical(item) && all(is.na(item))))) item <- as.numeric(item)
   out[[nm]] <- item
  }
 }
 out
}
project_with_settings <- function(state) {
 p <- state$project
 for(nm in c('axes','graph','view')) p$settings[[nm]] <- state[[nm]]
 p$settings$statistics <- state$stats_settings;p
}
export_server <- function(id,state,comparison) shiny::moduleServer(id,function(input,output,session) {
 for(fmt in c('png','svg','pdf')) local({format <- fmt;output[[format]] <- shiny::downloadHandler(
   filename=function() paste0('StatLab_',Sys.Date(),'.',format),content=function(path) export_figure(comparison$figure(),path,format,state$graph$export_width,state$graph$export_height,state$graph$export_dpi))})
 output$csv <- shiny::downloadHandler(filename=function() 'StatLab_results.csv',content=function(path) utils::write.csv(state$project$results,path,row.names=FALSE,na=''))
 output$xlsx <- shiny::downloadHandler(filename=function() 'StatLab_results.xlsx',content=function(path) {
   summaries <- tryCatch(summarise_groups(comparison$data()),error=function(e) data.frame(message=conditionMessage(e)))
   openxlsx::write.xlsx(list(Statistics=state$project$results,Descriptions=summaries,Data=state$project$data),path)
 })
 output$data <- shiny::downloadHandler(filename=function() 'StatLab_data.csv',content=function(path) utils::write.csv(state$project$data,path,row.names=FALSE))
 output$save <- shiny::downloadHandler(filename=function() paste0('StatLab_',Sys.Date(),'.json'),content=function(path) {
   save_project(project_with_settings(state),path)
 })
 shiny::observeEvent(input$load,{
   tryCatch({
    p <- load_project(input$load$datapath)
    a <- normalize_settings(p$settings$axes %||% list(),axis_settings())
    g <- normalize_settings(p$settings$graph %||% list(),graph_defaults())
    validate_axis(a$x);validate_axis(a$y);validate_intervals(a$intervals,log=a$x$scale=='log10');validate_graph_settings(g)
    state$project <- p;commit_edit(state,g,a,record=FALSE);state$undo <- list();state$redo <- list()
    state$view <- p$settings$view %||% list();state$view$endpoint <- unlist(state$view$endpoint);state$view$filters <- lapply(state$view$filters,unlist)
    state$stats_settings <- p$settings$statistics %||% list();state$restore <- state$restore+1
    shiny::showNotification('Project reopened locally.',type='message')
    if(length(p$extensions$color_migration_notice)) shiny::showNotification(p$extensions$color_migration_notice,type='warning',duration=12)
   },error=function(e) shiny::showNotification(conditionMessage(e),type='error',duration=12))
 })
 shiny::observeEvent(input$store,{
  if(!nzchar(input$preset_name)) return()
  p <- state$project;p$presets[[input$preset_name]] <- list(axes=state$axes,graph=state$graph);state$project <- p
 })
 shiny::observe({shiny::updateSelectInput(session,'preset',choices=names(state$project$presets))})
 shiny::observeEvent(input$restore,{
  preset <- state$project$presets[[input$preset]];shiny::req(preset)
  g <- normalize_settings(preset$graph,graph_defaults());if(nzchar(g$colors)) g <- migrate_positional_colors(g,comparison$data(),legacy_order=TRUE)
  if(nzchar(g$shapes) || nzchar(g$line_types)) g <- migrate_legacy_symbols(g,comparison$data())
  commit_edit(state,g,normalize_settings(preset$axes,axis_settings()));state$restore <- state$restore+1
 })
 shiny::observeEvent(list(state$graph$export_width,state$graph$export_height,state$graph$export_dpi),{
  for(nm in c('width','height','dpi')) shiny::updateNumericInput(session,nm,value=state$graph[[paste0('export_',nm)]])
 })
 shiny::observeEvent(list(input$width,input$height,input$dpi),{
  g <- state$graph
  for(nm in c('width','height','dpi')) if(!is.null(input[[nm]])) g[[paste0('export_',nm)]] <- input[[nm]]
  tryCatch({validate_graph_settings(g);commit_edit(state,graph=g)},error=function(e) shiny::showNotification(conditionMessage(e),type='error'))
 },ignoreInit=TRUE)
 output$session <- shiny::renderPrint({print(utils::sessionInfo());cat('\nProject format: StatLab JSON v2 (v1 compatible). No telemetry or remote processing.\n')})
})
