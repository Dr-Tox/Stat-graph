data_ui <- function(id) {
 ns <- shiny::NS(id)
 condition <- function(value) paste0("input['",ns('source_method'),"'] == '",value,"'")
 shiny::div(class='guided-workspace',shiny::h2('Your data, ready to compare'),
  shiny::p('Import a study in three steps. Your existing studies remain available and are never replaced by an import.'),
  shiny::tabsetPanel(id=ns('workspace_tab'),
   shiny::tabPanel('Import Data',value='import',shiny::fluidRow(
    shiny::column(5,
     shiny::div(class='workflow-card',shiny::h3('1. Choose your data'),
      shiny::radioButtons(ns('source_method'),'How would you like to add data?',c('Upload CSV / Excel'='file','Paste from Excel / enter a table'='paste'),inline=FALSE),
      shiny::conditionalPanel(condition('file'),shiny::fileInput(ns('files'),'Choose one or more files',multiple=TRUE,accept=c('.csv','.xlsx')),
       shiny::tags$details(shiny::tags$summary('Excel options'),shiny::numericInput(ns('sheet'),'Sheet number',1,min=1))),
      shiny::conditionalPanel(condition('paste'),shiny::textAreaInput(ns('paste'),'Paste cells including the header row',rows=6,placeholder='dose\tvalue\tsex\n0\t4.2\tFemale\n1\t4.8\tFemale'),
       shiny::actionButton(ns('read_paste'),'Preview pasted data'),shiny::helpText('You can also type a header and one row here, then add observations in Manage Studies.')),
      shiny::selectInput(ns('file'),'Loaded table to preview',choices=character())),
     shiny::div(class='workflow-card',shiny::h3('2. Describe your measurements'),
      shiny::radioButtons(ns('mode'),'What does each row represent?',c('One individual measurement'='raw','One group: mean, SD and sample size'='summary')),
      shiny::uiOutput(ns('mode_guide')),shiny::downloadButton(ns('template'),'Download CSV template'),
      shiny::actionButton(ns('example'),'Preview an example table'),shiny::helpText('Example tables are synthetic and are not imported until you confirm.'),
      shiny::selectInput(ns('format'),'How is the table arranged?',c('One dose and response per row'='long','Doses in column headers (individual data)'='wide')),
      shiny::h4('Match the columns in your table'),shiny::uiOutput(ns('mapping')),
      shiny::textInput(ns('endpoint'),'What was measured? (biological endpoint)','Relative liver weight'),
      shiny::helpText('Used when there is no endpoint column. Do not combine biologically different measurements.'),
      shiny::textInput(ns('dose_unit'),'Dose unit used in your table','mg/kg/day'),shiny::textInput(ns('response_unit'),'Response unit used in your table','%'),
      shiny::tags$details(shiny::tags$summary('Optional unit conversion'),shiny::helpText('Leave blank to retain original units. Conversions are explicit and checked for compatibility.'),
       shiny::textInput(ns('target_dose'),'Convert dose to',value=''),shiny::textInput(ns('target_response'),'Convert response to',value='')),
      shiny::tags$details(shiny::tags$summary('Optional study metadata'),shiny::textInput(ns('species'),'Species / model'),shiny::textAreaInput(ns('notes'),'Study notes')))),
    shiny::column(7,shiny::div(class='workflow-card',shiny::h3('3. Review and import'),
      shiny::uiOutput(ns('import_status')),DT::DTOutput(ns('preview')),
      shiny::textInput(ns('name'),'Study name (blank = file name)'),
      shiny::checkboxInput(ns('all_files'),'Import all loaded files with these column choices',FALSE),
      shiny::checkboxInput(ns('confirm_endpoint'),'I checked the endpoint, units and column choices',FALSE),
      shiny::actionButton(ns('import'),'Import studies',class='btn-primary'),shiny::uiOutput(ns('import_result')))))),
   shiny::tabPanel('Manage Studies',value='manage',shiny::div(class='workflow-card',
    shiny::h3('Choose a study to inspect or edit'),shiny::selectInput(ns('dataset'),'Study',choices=character()),shiny::uiOutput(ns('origin')),
    shiny::fluidRow(shiny::column(5,shiny::textInput(ns('rename'),'Display name')),shiny::column(7,shiny::actionButton(ns('save_meta'),'Save name / notes'))),
    shiny::textAreaInput(ns('edit_notes'),'Study notes'),
    shiny::div(class='compact-actions',shiny::actionButton(ns('up'),'Move up'),shiny::actionButton(ns('down'),'Move down'),shiny::actionButton(ns('remove'),'Remove dataset')),
    shiny::h4('Observations'),shiny::helpText('Double-click a cell to edit. Select rows to delete. Display rounding never changes the stored measurements.'),
    shiny::checkboxInput(ns('technical'),'Show all columns (identifiers and fixed factors)',FALSE),DT::DTOutput(ns('table')),
    shiny::actionButton(ns('add_row'),'Add observation / treatment group'),shiny::actionButton(ns('delete_rows'),'Delete selected rows'),
    shiny::tags$details(shiny::tags$summary('Add another endpoint from the original table'),shiny::uiOutput(ns('variables')),
     shiny::textInput(ns('variable_endpoint'),'Biological endpoint name'),shiny::textInput(ns('variable_unit'),'Response unit'),shiny::actionButton(ns('add_variable'),'Add mapped endpoint to this study')),
    shiny::tags$details(shiny::tags$summary('Harmonize endpoint names or convert units'),shiny::selectInput(ns('remap_source_endpoint'),'Source endpoint',choices=character()),
     shiny::textInput(ns('mapped_endpoint'),'Common biological endpoint'),shiny::textInput(ns('mapped_dose'),'Target dose unit'),shiny::textInput(ns('mapped_response'),'Target response unit'),
     shiny::checkboxInput(ns('confirm_remap'),'I confirm endpoint equivalence and unit conversion'),shiny::actionButton(ns('remap'),'Apply explicit harmonization'))))))
}
data_server <- function(id,state) shiny::moduleServer(id,function(input,output,session) {
 sources <- shiny::reactiveVal(list())
 table_refresh <- shiny::reactiveVal(0L)
 import_message <- shiny::reactiveVal(NULL)
 output$import_result <- shiny::renderUI({text <- import_message();if(length(text)) shiny::div(class='source-card',text)})
 output$mode_guide <- shiny::renderUI({
  if(input$mode=='summary') shiny::helpText('Example: dose = 1, mean = 4.5, SD = 0.4, n = 8. Do not enter SEM in the SD column. SD and n are optional for mean-only plots; inference requires them.')
  else shiny::helpText('Example: dose = 1, value = 4.5. Enter one row for each animal or experimental unit. Repeat the dose for individual replicates.')
 })
 output$template <- shiny::downloadHandler(filename=function() paste0('statlab_',input$mode,'_template.csv'),content=function(file) utils::write.csv(import_template(input$mode),file,row.names=FALSE))
 shiny::observeEvent(input$example,{sources(list('Synthetic example table'=import_template(input$mode)));shiny::updateSelectInput(session,'file',choices='Synthetic example table',selected='Synthetic example table')})
 shiny::observeEvent(input$mode,{shiny::updateSelectInput(session,'format',choices=if(input$mode=='summary') c('One group per row'='long') else c('One dose and response per row'='long','Doses in column headers'='wide'),selected=if(input$mode=='summary') 'long' else shiny::isolate(input$format %||% 'long'))})
 guard <- function(expr) tryCatch(force(expr),error=function(e) {table_refresh(table_refresh()+1L);shiny::showNotification(conditionMessage(e),type='error',duration=12)})
 shiny::observeEvent(list(sources(),input$file,input$mode,input$format,input$endpoint,input$dose_unit,input$response_unit,input$target_dose,input$target_response,
  lapply(c('dataset_id','endpoint','dose_unit','response_unit','dose','value','mean','sd','n','treatment',factors),function(key) input[[paste0('map_',key)]])),{
  import_message(NULL);shiny::updateCheckboxInput(session,'confirm_endpoint',value=FALSE)
 },ignoreInit=TRUE)
 shiny::observeEvent(input$files,guard({
   files <- input$files; tabs <- lapply(seq_len(nrow(files)),function(i) read_table_file(files$datapath[i],files$name[i],input$sheet))
   names(tabs) <- make.unique(files$name); sources(tabs)
   shiny::updateSelectInput(session,'file',choices=names(tabs),selected=names(tabs)[1])
 }))
 shiny::observeEvent(input$sheet,{
   if(!is.null(input$files)) guard({files <- input$files; tabs <- lapply(seq_len(nrow(files)),function(i) read_table_file(files$datapath[i],files$name[i],input$sheet)); names(tabs) <- make.unique(files$name); sources(tabs)})
 },ignoreInit=TRUE)
 shiny::observeEvent(input$read_paste,guard({
   sep <- if(grepl('\t',input$paste,fixed=TRUE)) '\t' else ','
   tab <- utils::read.table(text=input$paste,header=TRUE,sep=sep,stringsAsFactors=FALSE,check.names=FALSE)
   sources(list('Manual table'=tab)); shiny::updateSelectInput(session,'file',choices='Manual table',selected='Manual table')
 }))
 source <- shiny::reactive({shiny::req(input$file); sources()[[input$file]]})
 output$import_status <- shiny::renderUI({
  if(is.null(input$file) || !length(sources()) || is.null(sources()[[input$file]])) return(shiny::div(class='empty-guide','Start at step 1: upload a file or paste a table. A preview will appear here.'))
  tab <- sources()[[input$file]];shiny::div(class='source-card',shiny::strong(input$file),shiny::div(paste(nrow(tab),'rows ·',ncol(tab),'columns')),shiny::p('Choose the measurement type and match columns in step 2. This preview has not been imported.'))
 })
 output$preview <- DT::renderDT({shiny::req(source()); DT::datatable(head(source(),100),options=list(scrollX=TRUE,pageLength=5))})
 output$mapping <- shiny::renderUI({
   shiny::req(source()); cols <- names(source()); ns <- session$ns
   keys <- c('dataset_id','endpoint','dose_unit','response_unit',if(input$format!='wide') 'dose',if(input$format!='wide') if(input$mode=='raw') 'value' else c('mean','sd','n'),'treatment',factors)
   aliases <- list(dataset_id=c('study_name','dataset_id'),dose=c('dose_mg_kg_day','dose'),value=c('value','response'),mean=c('mean_serumconc_ng_ML','mean'),sd=c('sd_serumconc','sd'),n=c('n'),exposure=c('days_exposed','exposure'))
   labels <- c(dataset_id='Study identifier (splits a table into independent studies)',endpoint='Biological endpoint column',dose_unit='Dose unit column',response_unit='Response unit column',dose='Dose column',value='Individual response column',mean='Group mean column',sd='Standard deviation (SD) column',n='Sample size (n) column',treatment='Treatment / group name',sex='Sex',generation='Generation',exposure='Exposure duration',collection_time='Collection time',subgroup='Other experimental subgroup')
   field <- function(key) {candidate <- intersect(aliases[[key]] %||% key,cols);shiny::selectInput(ns(paste0('map_',key)),labels[[key]],c('Not in this table'='(none)',stats::setNames(cols,cols)),selected=if(length(candidate)) candidate[1] else '(none)')}
   required <- intersect(keys,c('dose','value','mean','sd','n'))
   shiny::tagList(if(input$format=='wide') shiny::selectizeInput(ns('wide'),'Measurement columns (numeric dose headers)',cols,multiple=TRUE),
    lapply(required,field),shiny::tags$details(shiny::tags$summary('Optional groups, study IDs and columns for endpoints / units'),lapply(setdiff(keys,required),field)))
 })
 shiny::observeEvent(input$import,guard({
   if(input$mode=='summary' && input$format=='wide') fail('Summary data requires one group per row. Use the summary template.')
   if(!isTRUE(input$confirm_endpoint)) fail('Confirm endpoint identity and mappings before importing.')
   tabs <- if(input$all_files) sources() else sources()[input$file]
   if(!length(tabs)) fail('Load a source table first.')
   mapping <- stats::setNames(lapply(c('dataset_id','endpoint','dose_unit','response_unit','dose','value','mean','sd','n','treatment',factors),function(key) input[[paste0('map_',key)]] %||% '(none)'),c('dataset_id','endpoint','dose_unit','response_unit','dose','value','mean','sd','n','treatment',factors))
   p <- state$project
   for(filename in names(tabs)) {
     tab <- tabs[[filename]]
     split_col <- mapping$dataset_id
     pieces <- if(split_col!='(none)') {
       if(!split_col %in% names(tab) || anyNA(tab[[split_col]])) fail('Mapped study IDs are missing.')
       split(tab,as.character(tab[[split_col]]))
     } else stats::setNames(list(tab),if(nzchar(input$name) && length(tabs)==1) input$name else tools::file_path_sans_ext(filename))
     for(nm in names(pieces)) {
       d <- map_table(pieces[[nm]],mapping,input$mode,input$endpoint,input$dose_unit,input$response_unit,input$target_dose,input$target_response,
         if(input$format=='wide') input$wide else NULL)
       p <- add_dataset(p,d,nm,list(species=input$species,notes=input$notes,source=filename,source_table=pieces[[nm]]),
         list(columns=mapping,mode=input$mode,endpoint=input$endpoint,source_dose_unit=input$dose_unit,source_response_unit=input$response_unit))
     }
   }
   added <- length(p$datasets)-length(state$project$datasets);state$project <- p;import_message(paste('Imported',added,if(added==1) 'study.' else 'studies.','Open Compare Datasets to plot them, or Manage Studies to edit observations.')); shiny::showNotification('Datasets added. Existing observations were retained.',type='message')
 }))
 shiny::observe({
   p <- state$project; choices <- stats::setNames(names(p$datasets),vapply(p$datasets,function(x) paste0(x$name,' [',x$id,']'),character(1)))
   selected <- if(input$dataset %in% names(p$datasets)) input$dataset else names(p$datasets)[1]
   shiny::updateSelectInput(session,'dataset',choices=choices,selected=selected)
 })
 shiny::observeEvent(input$dataset,{
   meta <- state$project$datasets[[input$dataset]]; shiny::req(meta)
   shiny::updateTextInput(session,'rename',value=meta$name); shiny::updateTextAreaInput(session,'edit_notes',value=meta$metadata$notes %||% '')
   d <- state$project$data[state$project$data$dataset_id==input$dataset,,drop=FALSE]
   shiny::updateSelectInput(session,'remap_source_endpoint',choices=unique(d$endpoint),selected=unique(d$endpoint)[1])
   shiny::updateTextInput(session,'mapped_endpoint',value=unique(d$endpoint)[1]); shiny::updateTextInput(session,'mapped_dose',value=unique(d$dose_unit)[1]); shiny::updateTextInput(session,'mapped_response',value=unique(d$response_unit)[1])
 })
 editing <- shiny::reactive({d <- state$project$data; d[d$dataset_id==input$dataset,,drop=FALSE]})
 output$origin <- shiny::renderUI({shiny::req(input$dataset);source_card(state$project$datasets[[input$dataset]],editing())})
 output$table <- DT::renderDT({
  table_refresh();d <- editing();hidden <- if(isTRUE(input$technical)) character() else c('dataset_id','mode','endpoint','dose_unit','response_unit',factors[vapply(d[factors],function(x) all(x=='Unspecified'),logical(1))],if(all(d$mode=='raw')) c('mean','sd','n') else if(all(d$mode=='summary')) 'value')
  tab <- DT::datatable(d,rownames=FALSE,editable=list(target='cell',disable=list(columns=match(c('dataset_id','mode'),names(d))-1)),selection='multiple',options=list(scrollX=TRUE,pageLength=10,columnDefs=list(list(targets=which(names(d) %in% hidden)-1,visible=FALSE))))
  numeric <- names(d)[vapply(d,is.numeric,logical(1))];if(length(numeric)) tab <- DT::formatSignif(tab,columns=numeric,digits=6)
  tab
 },server=TRUE)
 shiny::observeEvent(input$table_cell_edit,guard({
   info <- input$table_cell_edit; d <- editing(); col <- names(d)[info$col+1]
   if(col %in% c('dataset_id','mode')) fail('Dataset identity and input mode are protected.')
   d[info$row,col] <- DT::coerceValue(info$value,d[[col]]); validate_data(d)
   p <- state$project; p$data <- dplyr::bind_rows(p$data[p$data$dataset_id!=input$dataset,,drop=FALSE],d); validate_data(p$data); state$project <- p
 }))
 shiny::observeEvent(input$save_meta,guard({
   if(!nzchar(input$rename)) fail('Enter a study name.')
   p <- state$project; shiny::req(p$datasets[[input$dataset]]); p$datasets[[input$dataset]]$name <- input$rename; p$datasets[[input$dataset]]$metadata$notes <- input$edit_notes; state$project <- p
 }))
 shiny::observeEvent(input$remove,{shiny::req(input$dataset); state$project <- remove_dataset(state$project,input$dataset)})
 move <- function(delta) {
   p <- state$project; ids <- names(p$datasets); i <- match(input$dataset,ids); j <- i+delta
   if(is.na(i) || j<1 || j>length(ids)) return()
   ids[c(i,j)] <- ids[c(j,i)]; p$datasets <- p$datasets[ids]; p$selected <- ids[ids %in% p$selected]; state$project <- p
 }
 shiny::observeEvent(input$up,move(-1)); shiny::observeEvent(input$down,move(1))
 shiny::observeEvent(input$delete_rows,guard({
   rows <- input$table_rows_selected; if(!length(rows)) fail('Select rows to delete.')
   p <- state$project; d <- editing()[-rows,,drop=FALSE]
   p$data <- dplyr::bind_rows(p$data[p$data$dataset_id!=input$dataset,,drop=FALSE],d)
   if(nrow(p$data)) validate_data(p$data); state$project <- p
 }))
 shiny::observeEvent(input$add_row,guard({
   d <- editing(); if(!nrow(d)) fail('Import an initial row first.')
   row <- d[nrow(d),,drop=FALSE]
   if(row$mode=='summary') {row$dose <- max(d$dose)+1; row$treatment <- paste('Dose',row$dose)}
   p <- state$project; p$data <- dplyr::bind_rows(p$data,row); validate_data(p$data); state$project <- p
 }))
 shiny::observeEvent(editing(),{choices <- unique(editing()$endpoint);shiny::updateSelectInput(session,'remap_source_endpoint',choices=choices,selected=if(length(input$remap_source_endpoint) && input$remap_source_endpoint %in% choices) input$remap_source_endpoint else choices[1])})
 source_frame <- function() {
  shiny::req(input$dataset)
  tab <- state$project$datasets[[input$dataset]]$metadata$source_table
  if(is.null(tab)) return(NULL)
  if(is.data.frame(tab)) tab else as.data.frame(dplyr::bind_rows(tab))
 }
 output$variables <- shiny::renderUI({
  tab <- source_frame();if(is.null(tab)) return(shiny::helpText('No retained source table for this example study. Imported studies retain their source columns.'))
  numeric <- names(tab)[vapply(tab,is.numeric,logical(1))]
  shiny::tagList(shiny::selectInput(session$ns('variable_column'),'Response / mean source column',numeric),
   shiny::selectInput(session$ns('variable_sd'),'SD source column (summary only)',c('(none)',numeric)))
 })
 shiny::observeEvent(input$add_variable,guard({
  p <- state$project;meta <- p$datasets[[input$dataset]];tab <- source_frame()
  if(is.null(tab)) fail('This dataset has no retained source table.')
  existing <- p$data[p$data$dataset_id==input$dataset,,drop=FALSE]
  if(!nzchar(input$variable_endpoint) || input$variable_endpoint %in% existing$endpoint) fail('Use a new, explicit biological endpoint name. Existing measurements are never overwritten.')
  mapping <- meta$mapping$columns;mode <- meta$mapping$mode
  mapping[[if(mode=='raw') 'value' else 'mean']] <- input$variable_column
  mapping$sd <- input$variable_sd;mapping$endpoint <- '(none)';mapping$response_unit <- '(none)'
  d <- map_table(tab,mapping,mode,input$variable_endpoint,meta$mapping$source_dose_unit,input$variable_unit,
    target_dose=unique(existing$dose_unit)[1])
  d$dataset_id <- input$dataset;p$data <- dplyr::bind_rows(p$data,d);validate_data(p$data)
  p$datasets[[input$dataset]]$mapping$additional_endpoints[[input$variable_endpoint]] <- list(column=input$variable_column,sd=input$variable_sd,unit=input$variable_unit)
  state$project <- p
 }))
 shiny::observeEvent(input$remap,guard({
   if(!input$confirm_remap) fail('Confirm endpoint equivalence and unit conversion.')
   p <- state$project; rows <- p$data$dataset_id==input$dataset & p$data$endpoint==input$remap_source_endpoint; d <- p$data[rows,,drop=FALSE]
   if(!nrow(d)) fail('Select a nonempty dataset.')
   df <- vapply(d$dose_unit,unit_factor,numeric(1),to=input$mapped_dose)
   rf <- vapply(d$response_unit,unit_factor,numeric(1),to=input$mapped_response)
   d$dose <- d$dose*df; for(col in c('value','mean','sd')) d[[col]] <- d[[col]]*rf
   d$endpoint <- input$mapped_endpoint; d$dose_unit <- input$mapped_dose; d$response_unit <- input$mapped_response
   validate_data(d); p$data[rows,] <- d
   p$datasets[[input$dataset]]$mapping$harmonization[[input$remap_source_endpoint]] <- list(endpoint=input$mapped_endpoint,dose_unit=input$mapped_dose,response_unit=input$mapped_response)
   state$project <- p
 }))
})
