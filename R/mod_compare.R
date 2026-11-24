compare_ui <- function(id) {
 ns <- shiny::NS(id)
 shiny::div(class='statlab-workspace',id=ns('workspace'),
  shiny::div(class='workspace-properties',
   shiny::div(class='properties-heading','COMPARISON PROPERTIES'),
   shiny::tags$details(open=TRUE,class='study-manager',shiny::tags$summary('Studies'),
    shiny::textOutput(ns('selected_count')),shiny::helpText('Use the checkboxes: Compare selects a study; Show controls visibility; Highlight emphasizes it. Changes never delete data.'),DT::DTOutput(ns('studies')),
    shiny::div(class='compact-actions',shiny::actionButton(ns('all'),'Select all'),shiny::actionButton(ns('none'),'Clear selection'),shiny::actionButton(ns('matched'),'Use search results')),
    shiny::tags$details(shiny::tags$summary('Selected-row actions'),
     shiny::actionButton(ns('only_rows'),'Compare selected rows'),shiny::actionButton(ns('activate'),'Activate rows'),shiny::actionButton(ns('deactivate'),'Deactivate rows'),
     shiny::actionButton(ns('hide'),'Hide rows'),shiny::actionButton(ns('unhide'),'Unhide rows'),
     shiny::actionButton(ns('highlight'),'Highlight rows'),shiny::actionButton(ns('clear_highlight'),'Clear highlighting'),
     shiny::actionButton(ns('up'),'Move row up'),shiny::actionButton(ns('down'),'Move row down'))),
   shiny::selectInput(ns('endpoint'),'Common endpoint',choices=character()),
   shiny::tags$details(shiny::tags$summary('Experimental filters'),shiny::uiOutput(ns('filters'))),
   shiny::tags$details(open=TRUE,class='graph-editor',shiny::tags$summary('Graph editor'),graph_ui('graphs'))),
  shiny::div(class='workspace-figure',
   shiny::div(class='figure-toolbar',shiny::actionButton(ns('toggle_editor'),'Show / hide properties',class='properties-toggle'),shiny::actionButton(ns('toggle_legend'),'Collapse / expand legend')),
   shiny::uiOutput(ns('summary')),plotly::plotlyOutput(ns('plot'),height='760px'),
   shiny::tags$details(shiny::tags$summary('Figure diagnostics'),shiny::verbatimTextOutput(ns('diagnostics'))),
   shiny::tags$details(shiny::tags$summary('Exact plotted coordinates'),DT::DTOutput(ns('coordinates')))))
}
compare_server <- function(id,state) shiny::moduleServer(id,function(input,output,session) {
 chosen <- shiny::reactiveVal(character())
 last_endpoints <- character()
 shiny::observeEvent(list(unique(state$project$data$endpoint),state$restore),{
  p <- state$project;ids <- names(p$datasets)
  endpoints <- unique(p$data$endpoint);selected <- state$view$endpoint %||% endpoints[1]
  if(!length(selected) || !selected %in% endpoints) selected <- endpoints[1]
  if(!identical(input$endpoint,selected) || !identical(last_endpoints,endpoints)) {
   shiny::freezeReactiveValue(input,'endpoint');shiny::updateSelectInput(session,'endpoint',choices=endpoints,selected=selected);last_endpoints <<- endpoints
  }
 })
 set_selection <- function(ids) {p <- state$project;p$selected <- names(p$datasets)[names(p$datasets) %in% ids];state$project <- p}
 shiny::observeEvent(input$all,{v <- state$view;v$hidden <- character();state$view <- v;set_selection(names(state$project$datasets))})
 shiny::observeEvent(input$none,set_selection(character()))
 shiny::observeEvent(input$endpoint,{v <- state$view;v$endpoint <- input$endpoint;state$view <- v})
 registry_table <- shiny::reactiveVal(data.frame())
 registry_layout <- shiny::reactiveVal(data.frame())
 shiny::observeEvent(list(state$project$datasets,state$project$selected,state$view$hidden,state$graph$dataset_styles,state$graph$highlight),{
  p <- state$project;ids <- names(p$datasets);hidden <- state$view$hidden %||% character();g <- state$graph
  labels <- vapply(p$datasets,function(x) x$name,character(1))
  for(i in seq_along(ids)) {j <- match(ids[i],g$dataset_styles$dataset_id);if(!is.na(j) && nzchar(g$dataset_styles$label[j])) labels[i] <- g$dataset_styles$label[j]}
  table <- data.frame(Study=labels,ID=ids,Active=ids %in% p$selected,Visible=ids %in% setdiff(p$selected,hidden),Highlight=ids %in% g$highlight)
  layout <- data.frame(Study=labels,ID=ids,Origin=vapply(p$datasets,study_origin,character(1)))
  if(!identical(layout,registry_layout())) registry_layout(layout)
  if(!identical(table,registry_table())) {
   registry_table(table)
   session$sendCustomMessage('statlab-study-states',list(output=session$ns('studies'),active=p$selected,visible=setdiff(p$selected,hidden),highlight=g$highlight))
  }
 })
 output$studies <- DT::renderDT({
  registry_layout();table <- shiny::isolate(registry_table());ids <- table$ID
  for(field in c('Active','Visible','Highlight')) table[[field]] <- vapply(seq_len(nrow(table)),function(i) as.character(shiny::tags$input(type='checkbox',class='study-toggle',`data-study`=ids[i],`data-field`=field,`data-input`=session$ns('study_toggle'),checked=if(table[[field]][i]) 'checked' else NULL,`aria-label`=paste(c(Active='Compare',Visible='Show',Highlight='Highlight')[[field]],table$Study[i]))),character(1))
  table$Study <- vapply(seq_along(ids),function(i) as.character(shiny::tags$div(shiny::tags$span(table$Study[i]),shiny::tags$small(class='study-origin',shiny::isolate(study_origin(state$project$datasets[[ids[i]]]))))),character(1))
  DT::datatable(table,escape=FALSE,colnames=c('Study','ID','Compare','Show','Highlight'),rownames=FALSE,selection=list(mode='multiple',selected=match(shiny::isolate(chosen()),ids)),options=list(dom='ftip',pageLength=5,scrollX=TRUE,columnDefs=list(list(targets=1,visible=FALSE),list(targets=2:4,orderable=FALSE)),language=list(search='Search studies:')))
 },server=FALSE)
 shiny::observeEvent(input$study_toggle,{
  event <- input$study_toggle
  tryCatch({
   next_state <- apply_study_toggle(state$project,state$view,state$graph,event$id,event$field,event$checked)
   if(!identical(state$project,next_state$project)) state$project <- next_state$project
   if(!identical(state$view,next_state$view)) state$view <- next_state$view
   if(!identical(state$graph,next_state$graph)) commit_edit(state,graph=next_state$graph)
  },error=function(e) shiny::showNotification(conditionMessage(e),type='error'))
 })
 shiny::observeEvent(input$studies_rows_selected,{chosen(names(state$project$datasets)[input$studies_rows_selected])},ignoreNULL=FALSE)
 shiny::observeEvent(input$only_rows,set_selection(chosen()))
 shiny::observeEvent(input$activate,set_selection(union(state$project$selected,chosen())))
 shiny::observeEvent(input$deactivate,set_selection(setdiff(state$project$selected,chosen())))
 shiny::observeEvent(input$matched,set_selection(names(state$project$datasets)[input$studies_rows_all]))
 shiny::observeEvent(input$hide,{v <- state$view;v$hidden <- union(v$hidden %||% character(),chosen());state$view <- v})
 shiny::observeEvent(input$unhide,{v <- state$view;v$hidden <- setdiff(v$hidden %||% character(),chosen());state$view <- v})
 shiny::observeEvent(input$highlight,{g <- state$graph;g$highlight <- union(g$highlight,chosen());commit_edit(state,graph=g)})
 shiny::observeEvent(input$clear_highlight,{g <- state$graph;g$highlight <- character();commit_edit(state,graph=g)})
 move <- function(direction) {
  if(length(chosen())!=1) {shiny::showNotification('Select exactly one study row to reorder.',type='message');return()}
  p <- state$project;ids <- names(p$datasets);i <- match(chosen(),ids);target <- i+direction
  if(!is.na(i) && target>=1 && target<=length(ids)) {ids[c(i,target)] <- ids[c(target,i)];p$datasets <- p$datasets[ids];p$selected <- ids[ids %in% p$selected];state$project <- p}
 }
 shiny::observeEvent(input$up,move(-1L));shiny::observeEvent(input$down,move(1L))
 shiny::observeEvent(input$toggle_legend,{g <- state$graph;g$legend_collapsed <- !g$legend_collapsed;commit_edit(state,graph=g);state$restore <- state$restore+1L})
 output$selected_count <- shiny::renderText(paste(length(state$project$selected),'selected ·',length(setdiff(state$project$selected,state$view$hidden %||% character())),'visible ·',length(state$project$datasets),'available'))
 output$filters <- shiny::renderUI({
  shiny::tagList(lapply(factors,function(nm) shiny::selectizeInput(session$ns(paste0('filter_',nm)),tools::toTitleCase(gsub('_',' ',nm)),choices=character(),multiple=TRUE,options=list(maxItems=NULL))),shiny::helpText('Empty filter = all available levels. Missing subgroups remain absent.'))
 })
 shiny::outputOptions(output,'filters',suspendWhenHidden=FALSE)
 previous_filter_choices <- NULL;previous_filter_restore <- NULL
 shiny::observeEvent(list(state$project$data,state$project$selected,state$view$hidden,state$view$endpoint,state$restore),{
  d <- state$project$data;ids <- setdiff(state$project$selected,state$view$hidden %||% character())
  d <- d[d$dataset_id %in% ids & d$endpoint==state$view$endpoint,,drop=FALSE]
  available <- lapply(d[factors],function(x) sort(unique(x)))
  if(identical(available,previous_filter_choices) && identical(state$restore,previous_filter_restore)) return()
  previous_filter_choices <<- available;previous_filter_restore <<- state$restore
  for(nm in factors) {
   selected <- unlist(state$view$filters[[nm]] %||% character())
   shiny::updateSelectizeInput(session,paste0('filter_',nm),choices=sort(unique(c(d[[nm]],selected))),selected=selected)
  }
 })
 filters <- shiny::reactive(stats::setNames(lapply(factors,function(nm) input[[paste0('filter_',nm)]] %||% character()),factors))
 pending_filters <- shiny::reactiveVal(NULL)
 shiny::observeEvent(state$restore,{
  pending_filters(stats::setNames(lapply(factors,function(nm) unlist(state$view$filters[[nm]] %||% character())),factors))
 },ignoreInit=FALSE,priority=100)
 effective_filters <- shiny::reactive(pending_filters() %||% filters())
 shiny::observeEvent(list(filters(),pending_filters()),{
  current <- filters();target <- pending_filters()
  if(!is.null(target) && !all(vapply(factors,function(nm) setequal(current[[nm]],target[[nm]]),logical(1)))) return()
  pending_filters(NULL)
  v <- state$view;v$filters <- current;if(!identical(v,state$view)) state$view <- v
 },ignoreInit=FALSE)
 data_cache <- shiny::reactiveVal(NULL);data_error <- shiny::reactiveVal('Select one or more datasets.')
 shiny::observeEvent(list(state$project$data,state$project$selected,state$project$datasets,state$view$endpoint,state$view$hidden,effective_filters()),{
  ids <- setdiff(state$project$selected,state$view$hidden %||% character())
  tryCatch({
   if(!length(ids)) fail('Select one or more datasets.')
   value <- comparison_data(state$project,ids,state$view$endpoint,effective_filters())
   if(!identical(value,data_cache())) data_cache(value)
   data_error('')
  },error=function(e) {data_error(conditionMessage(e));data_cache(NULL)})
 },ignoreNULL=FALSE)
 data <- shiny::reactive({shiny::validate(shiny::need(!is.null(data_cache()),data_error()));data_cache()})
 analysis_config <- shiny::reactive(graph_analysis_config(state$graph));preparation_count <- shiny::reactiveVal(0L)
 scientific_signature <- shiny::reactive(preparation_fingerprint(data()))
 numeric_cache <- shiny::reactiveVal(NULL);numeric_key <- shiny::reactiveVal(NULL)
 prepared <- shiny::reactive({
  cfg <- analysis_config();key <- list(data=scientific_signature(),role=cfg)
  cached <- shiny::isolate(numeric_cache());previous <- shiny::isolate(numeric_key())
  if(!is.null(cached) && identical(previous,key)) return(cached)
  value <- prepare_numeric_data(data(),cfg);numeric_cache(value);numeric_key(key)
  preparation_count(shiny::isolate(preparation_count())+1L);value
 })
 figure <- shiny::reactive({tryCatch(build_figure(data(),state$axes,state$graph,prepared()),error=function(e) {shiny::validate(shiny::need(FALSE,conditionMessage(e)))})})
 output$plot <- plotly::renderPlotly(interactive_figure(figure()))
 output$diagnostics <- shiny::renderText(paste(c(figure()$notes,'No observations are changed by graph editing or transformations.',paste('Numeric preparations:',preparation_count(),'(cosmetic edits reuse the prepared data)')),collapse='\n'))
 output$coordinates <- DT::renderDT(DT::datatable(figure()$coordinates,options=list(scrollX=TRUE,pageLength=5)))
 output$summary <- shiny::renderUI({d <- data();shiny::div(class='figure-summary',shiny::strong(paste(length(unique(d$dataset_id)),'independent studies')),paste(' · ',nrow(d),'input rows · ',unique(d$endpoint),' · ',paste(unique(d$mode),collapse=' + ')))})
 list(data=data,figure=figure,prepared=prepared,preparation_count=preparation_count)
})
