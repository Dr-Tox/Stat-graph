statistics_ui <- function(id) {
 ns <- shiny::NS(id)
 shiny::sidebarLayout(shiny::sidebarPanel(width=3,
  shiny::helpText('Inference is scoped to one study, endpoint and experimental stratum. Independent observations are assumed; repeated measures require a different model.'),
  shiny::selectInput(ns('dataset'),'Study',choices=character()),shiny::selectInput(ns('endpoint'),'Endpoint',choices=character()),
  shiny::uiOutput(ns('strata')),shiny::selectizeInput(ns('groups'),'Experimental treatment groups',choices=character(),multiple=TRUE),
  shiny::selectInput(ns('control'),'Control (required for Dunnett)',choices=character()),
  shiny::selectInput(ns('method'),'Method',stat_methods),shiny::numericInput(ns('alpha'),'Significance threshold',.05,min=.001,max=.5),
  shiny::selectInput(ns('adjust'),'Dunn p-value adjustment',stats::p.adjust.methods,selected='holm'),
  shiny::actionButton(ns('run'),'Run explicit comparison',class='btn-primary')),
  shiny::mainPanel(width=9,shiny::h3('Statistical inference'),shiny::verbatimTextOutput(ns('message')),
  DT::DTOutput(ns('result')),shiny::h4('Selected-group descriptions (confidence level = 1 - alpha)'),DT::DTOutput(ns('descriptive')),
  shiny::h4('Raw-data diagnostics'),shiny::plotOutput(ns('qq'),height='280px'),shiny::verbatimTextOutput(ns('diagnostics'))))
}
statistics_server <- function(id,state) shiny::moduleServer(id,function(input,output,session) {
 pending_restore <- shiny::reactiveVal(NULL)
 message <- shiny::reactiveVal('Select groups explicitly, then run a method. Supplied summary statistics are never expanded into individuals.')
 shiny::observe({p <- state$project;shiny::updateSelectInput(session,'dataset',choices=stats::setNames(names(p$datasets),vapply(p$datasets,function(x) paste(x$name,x$id),character(1))),selected=if(length(input$dataset) && input$dataset %in% names(p$datasets)) input$dataset else names(p$datasets)[1])})
 shiny::observeEvent(list(input$dataset,state$project$data),{
  d <- state$project$data;shiny::updateSelectInput(session,'endpoint',choices=unique(d$endpoint[d$dataset_id==input$dataset]))
 })
 base <- shiny::reactive({d <- state$project$data;d[d$dataset_id==input$dataset & d$endpoint==input$endpoint,,drop=FALSE]})
 output$strata <- shiny::renderUI({d <- base();shiny::tagList(lapply(factors,function(nm) shiny::selectInput(session$ns(nm),gsub('_',' ',nm),unique(d[[nm]]),selected=shiny::isolate(if(identical(state$stats_settings$dataset,input$dataset)) state$stats_settings$strata[[nm]] %||% input[[nm]] else input[[nm]]))))})
 scoped <- shiny::reactive({d <- base();for(nm in factors) {shiny::req(input[[nm]]);d <- d[d[[nm]]==input[[nm]],,drop=FALSE]}; d})
 restore_ready <- function(s) !is.null(s) && identical(input$dataset,s$dataset) && identical(input$endpoint,s$endpoint) &&
   all(vapply(factors,function(nm) is.null(s$strata[[nm]]) || identical(input[[nm]],s$strata[[nm]]),logical(1)))
 shiny::observeEvent(list(scoped(),pending_restore()),{
  d <- scoped();choices <- unique(d$treatment);s <- pending_restore()
  selected <- if(restore_ready(s)) intersect(unlist(s$groups),choices) else intersect(input$groups,choices)
  shiny::updateSelectizeInput(session,'groups',choices=choices,selected=selected)
  control <- if(restore_ready(s)) s$control else input$control
  shiny::updateSelectInput(session,'control',choices=choices,selected=if(length(control) && control %in% choices) control else if('Control' %in% choices) 'Control' else choices[1])
 })
 shiny::observeEvent(list(input$groups,input$control),{
  s <- pending_restore()
  if(restore_ready(s) && setequal(input$groups,unlist(s$groups)) && identical(input$control,s$control)) pending_restore(NULL)
 },ignoreNULL=FALSE)
 selected <- shiny::reactive({d <- scoped();d[d$treatment %in% input$groups,,drop=FALSE]})
 shiny::observeEvent(input$run,{
  tryCatch({
   if(!length(input$groups)) fail('Explicitly select the experimental groups to analyze.')
   res <- run_analysis(selected(),input$method,control=input$control,alpha=input$alpha,adjust=input$adjust)
   settings <- list(dataset=input$dataset,endpoint=input$endpoint,groups=input$groups,control=input$control,method=input$method,alpha=input$alpha,adjust=input$adjust,strata=stats::setNames(lapply(factors,function(nm) input[[nm]]),factors))
   p <- state$project;p$results <- res;p$analysis_context <- analysis_context(selected(),settings,res);state$project <- p;state$stats_settings <- settings
   message(paste('Completed:',input$method,'·',stat_assumptions(input$method)))
  },error=function(e) {message(conditionMessage(e));shiny::showNotification(conditionMessage(e),type='error',duration=10)})
 })
 output$message <- shiny::renderText(message())
 output$result <- DT::renderDT(DT::datatable(state$project$results,options=list(scrollX=TRUE)))
 output$descriptive <- DT::renderDT({d <- selected();shiny::req(nrow(d));DT::datatable(summarise_groups(d,1-input$alpha),options=list(scrollX=TRUE))})
 residuals <- shiny::reactive({d <- selected();shiny::req(nrow(d),all(d$mode=='raw'));d$value-ave(d$value,d$treatment,FUN=mean)})
 output$qq <- shiny::renderPlot({r <- residuals();shiny::req(length(r)>2);stats::qqnorm(r,main='Q–Q plot of within-group residuals');stats::qqline(r,col='#0072B2')})
 output$diagnostics <- shiny::renderPrint({r <- residuals(); if(length(r)>=3 && length(r)<=5000 && stats::sd(r)>0) print(stats::shapiro.test(r));cat('\nResidual normality is descriptive; independence comes from the experimental design.\n');d <- selected();if(length(unique(d$treatment))>1) print(stats::fligner.test(value~treatment,data=d))})
 shiny::observeEvent(state$restore,{
  s <- state$stats_settings;if(!length(s)) {pending_restore(NULL);return()};pending_restore(s)
  for(nm in intersect(names(s),c('dataset','endpoint','control','method','adjust'))) shiny::updateSelectInput(session,nm,selected=s[[nm]])
  shiny::updateNumericInput(session,'alpha',value=s$alpha);shiny::updateSelectizeInput(session,'groups',selected=unlist(s$groups))
 },ignoreInit=TRUE)
})
