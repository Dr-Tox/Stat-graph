statistics_ui <- function(id) {
 ns <- shiny::NS(id)
 shiny::div(class='guided-workspace',shiny::h2('Compare treatments within a study'),
  shiny::p('For multi-study graphs, use Compare Datasets. Statistical tests here use one independent study, one endpoint and one experimental stratum.'),
  shiny::sidebarLayout(shiny::sidebarPanel(width=4,
   shiny::div(class='workflow-card',shiny::h3('1. Choose the study and measurements'),
    shiny::selectInput(ns('dataset'),'Study',choices=character()),shiny::selectInput(ns('endpoint'),'Endpoint',choices=character()),shiny::uiOutput(ns('strata'))),
   shiny::div(class='workflow-card',shiny::h3('2. Check the treatments to compare'),
    shiny::helpText('Tick two or more groups below. No Ctrl / Cmd key is needed. A t-test requires exactly two.'),
    shiny::checkboxGroupInput(ns('groups'),'Available treatments',choices=character()),
    shiny::actionButton(ns('all_groups'),'Select all treatments'),shiny::actionButton(ns('no_groups'),'Clear treatments')),
   shiny::div(class='workflow-card',shiny::h3('3. Choose a method'),
    shiny::selectInput(ns('method'),'Statistical method',stat_methods),shiny::uiOutput(ns('method_help')),
    shiny::conditionalPanel(paste0("input['",ns('method'),"'] == 'Dunnett'"),shiny::selectInput(ns('control'),'Control treatment',choices=character())),
    shiny::tags$details(shiny::tags$summary('Significance and multiple comparisons'),shiny::numericInput(ns('alpha'),'Significance threshold (alpha)',.05,min=.001,max=.5),
     shiny::conditionalPanel(paste0("input['",ns('method'),"'] == 'Dunn'"),shiny::selectInput(ns('adjust'),'Dunn p-value adjustment',stats::p.adjust.methods,selected='holm'))),
    shiny::actionButton(ns('run'),'Run comparison',class='btn-primary'))),
   shiny::mainPanel(width=8,shiny::uiOutput(ns('origin')),shiny::uiOutput(ns('selection_status')),
    shiny::div(class='workflow-card',shiny::h3('Your selected treatments'),DT::DTOutput(ns('descriptive'))),
    shiny::div(class='workflow-card',shiny::h3('Analysis result'),shiny::verbatimTextOutput(ns('message')),shiny::checkboxInput(ns('result_details'),'Show all result details',FALSE),DT::DTOutput(ns('result'))),
    shiny::tags$details(shiny::tags$summary('Raw-data diagnostics and assumptions'),shiny::plotOutput(ns('qq'),height='280px'),shiny::verbatimTextOutput(ns('diagnostics'))))))
}
statistics_server <- function(id,state) shiny::moduleServer(id,function(input,output,session) {
 pending_restore <- shiny::reactiveVal(NULL)
 message <- shiny::reactiveVal('Select groups explicitly, then run a method. Supplied summary statistics are never expanded into individuals.')
 shiny::observe({p <- state$project;shiny::updateSelectInput(session,'dataset',choices=stats::setNames(names(p$datasets),vapply(p$datasets,function(x) paste(x$name,x$id),character(1))),selected=if(length(input$dataset) && input$dataset %in% names(p$datasets)) input$dataset else names(p$datasets)[1])})
 shiny::observeEvent(list(input$dataset,state$project$data),{
  d <- state$project$data;shiny::updateSelectInput(session,'endpoint',choices=unique(d$endpoint[d$dataset_id==input$dataset]))
 })
 base <- shiny::reactive({d <- state$project$data;d[d$dataset_id==input$dataset & d$endpoint==input$endpoint,,drop=FALSE]})
 output$strata <- shiny::renderUI({
  d <- base();shiny::tagList(lapply(factors,function(nm) {
   choices <- unique(d[[nm]]);fixed <- length(choices)<=1
   shiny::div(style=if(fixed) 'display:none' else NULL,shiny::selectInput(session$ns(nm),tools::toTitleCase(gsub('_',' ',nm)),choices,selected=shiny::isolate(if(identical(state$stats_settings$dataset,input$dataset)) state$stats_settings$strata[[nm]] %||% input[[nm]] else input[[nm]])))
  }),shiny::helpText('Only factors with a choice of levels are shown. Fixed factors remain part of the study identity.'))
 })
 shiny::outputOptions(output,'strata',suspendWhenHidden=FALSE)
 scoped <- shiny::reactive({d <- base();for(nm in factors) {shiny::req(input[[nm]]);d <- d[d[[nm]]==input[[nm]],,drop=FALSE]}; d})
 restore_ready <- function(s) !is.null(s) && identical(input$dataset,s$dataset) && identical(input$endpoint,s$endpoint) &&
   all(vapply(factors,function(nm) is.null(s$strata[[nm]]) || identical(input[[nm]],s$strata[[nm]]),logical(1)))
 shiny::observeEvent(list(scoped(),pending_restore()),{
  d <- scoped();choices <- unique(d$treatment);s <- pending_restore()
  selected <- if(restore_ready(s)) intersect(unlist(s$groups),choices) else intersect(input$groups,choices)
  shiny::updateCheckboxGroupInput(session,'groups',choices=treatment_choices(d),selected=selected)
  control <- if(restore_ready(s)) s$control else input$control
  shiny::updateSelectInput(session,'control',choices=choices,selected=if(length(control) && control %in% choices) control else if('Control' %in% choices) 'Control' else choices[1])
 })
 shiny::observeEvent(list(input$groups,input$control),{
  s <- pending_restore()
  if(restore_ready(s) && setequal(input$groups,unlist(s$groups)) && identical(input$control,s$control)) pending_restore(NULL)
 },ignoreNULL=FALSE)
 shiny::observeEvent(input$all_groups,shiny::updateCheckboxGroupInput(session,'groups',selected=unique(scoped()$treatment)))
 shiny::observeEvent(input$no_groups,shiny::updateCheckboxGroupInput(session,'groups',selected=character()))
 output$origin <- shiny::renderUI({shiny::req(input$dataset);source_card(state$project$datasets[[input$dataset]],base())})
 output$selection_status <- shiny::renderUI({
  d <- scoped();groups <- intersect(input$groups,unique(d$treatment))
  shiny::div(class=if(length(groups)) 'source-card' else 'empty-guide',
   if(!length(groups)) 'Choose treatments in step 2. Their sample sizes and summaries will appear here before you run a test.' else paste(length(groups),'treatments selected ·',nrow(d[d$treatment %in% groups,,drop=FALSE]),'stored rows'),
   shiny::div(paste(vapply(factors[vapply(d[factors],function(x) any(x!='Unspecified'),logical(1))],function(nm) paste(tools::toTitleCase(gsub('_',' ',nm)),paste(unique(d[[nm]]),collapse=', '),sep=': '),character(1)),collapse=' · ')))
 })
 output$method_help <- shiny::renderUI({shiny::helpText(stat_assumptions(input$method))})
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
 output$result <- DT::renderDT({
  d <- state$project$results
  primary <- intersect(c('comparison','p','estimate','lower','upper','df','correction','sample_sizes','confidence'),names(d))
  shown <- if(isTRUE(input$result_details)) names(d) else primary
  tab <- DT::datatable(d[shown],rownames=FALSE,options=list(scrollX=TRUE))
  numeric <- shown[vapply(d[shown],is.numeric,logical(1))];if(length(numeric)) tab <- DT::formatSignif(tab,columns=numeric,digits=6)
  tab
 })
 output$descriptive <- DT::renderDT({d <- selected();shiny::validate(shiny::need(nrow(d)>0,'Tick treatment checkboxes in step 2 to preview their summaries.'));summary <- summarise_groups(d,1-input$alpha);cols <- c('treatment','dose','mean','sd','n','se','lower','upper');tab <- DT::datatable(summary[cols],rownames=FALSE,colnames=c('Treatment','Dose','Mean','SD','n','SEM','CI lower','CI upper'),options=list(scrollX=TRUE));DT::formatSignif(tab,columns=c('mean','sd','se','lower','upper'),digits=6)})
 residuals <- shiny::reactive({d <- selected();shiny::req(nrow(d),all(d$mode=='raw'));d$value-ave(d$value,d$treatment,FUN=mean)})
 output$qq <- shiny::renderPlot({r <- residuals();shiny::req(length(r)>2);stats::qqnorm(r,main='Q–Q plot of within-group residuals');stats::qqline(r,col='#0072B2')})
 output$diagnostics <- shiny::renderPrint({r <- residuals(); if(length(r)>=3 && length(r)<=5000 && stats::sd(r)>0) print(stats::shapiro.test(r));cat('\nResidual normality is descriptive; independence comes from the experimental design.\n');d <- selected();if(length(unique(d$treatment))>1) print(stats::fligner.test(value~treatment,data=d))})
 shiny::observeEvent(state$restore,{
  s <- state$stats_settings;if(!length(s)) {pending_restore(NULL);return()};pending_restore(s)
  for(nm in intersect(names(s),c('dataset','endpoint','control','method','adjust'))) shiny::updateSelectInput(session,nm,selected=s[[nm]])
  shiny::updateNumericInput(session,'alpha',value=s$alpha);shiny::updateCheckboxGroupInput(session,'groups',selected=unlist(s$groups))
 },ignoreInit=TRUE)
})
