data_ui <- function(id) {
 ns <- shiny::NS(id)
 shiny::fluidRow(shiny::column(4,
 shiny::h3('Import & map'),
 shiny::fileInput(ns('files'),'CSV / Excel files',multiple=TRUE,accept=c('.csv','.xlsx')),
 shiny::numericInput(ns('sheet'),'Excel sheet number',1,min=1),
 shiny::textAreaInput(ns('paste'),'Paste a table with a header (tabs or CSV)',rows=4),
 shiny::actionButton(ns('read_paste'),'Use pasted / manual table'),
 shiny::helpText('For manual entry, paste a header and one row, then add or edit observations below.'),
 shiny::selectInput(ns('file'),'Preview / mapping template',choices=character()),
 shiny::selectInput(ns('mode'),'Input mode',c('Individual observations'='raw','Summary means (SD and n when available)'='summary')),
 shiny::selectInput(ns('format'),'Layout',c('Long / summary'='long','Wide: numeric dose headers'='wide')),
 shiny::uiOutput(ns('mapping')),
 shiny::helpText('Mapped endpoint/unit columns take precedence over fallback labels. Keep distinct biological endpoints separate.'),
 shiny::textInput(ns('endpoint'),'Fallback / common biological endpoint','Relative liver weight'),
 shiny::checkboxInput(ns('confirm_endpoint'),'I confirm endpoint identity and the selected mappings',FALSE),
 shiny::textInput(ns('dose_unit'),'Fallback source X / dose unit','mg/kg/day'),shiny::textInput(ns('target_dose'),'Target X / dose unit (blank = retain source)','mg/kg/day'),
 shiny::textInput(ns('response_unit'),'Fallback source response unit','%'),shiny::textInput(ns('target_response'),'Target response unit (blank = retain source)','%'),
 shiny::textInput(ns('name'),'Study name (single import; blank = file name)'),
 shiny::textInput(ns('species'),'Species / model'),shiny::textAreaInput(ns('notes'),'Study metadata / notes'),
 shiny::checkboxInput(ns('all_files'),'Import every loaded file using this mapping',FALSE),
 shiny::actionButton(ns('import'),'Validate & add studies',class='btn-primary')),
 shiny::column(8,shiny::h3('Source preview'),DT::DTOutput(ns('preview')),
 shiny::h3('Dataset registry'),shiny::selectInput(ns('dataset'),'Edit dataset',choices=character()),
 shiny::fluidRow(shiny::column(6,shiny::textInput(ns('rename'),'Display name')),shiny::column(6,shiny::actionButton(ns('save_meta'),'Save metadata / name'))),
 shiny::textAreaInput(ns('edit_notes'),'Dataset notes'),
 shiny::fluidRow(shiny::column(4,shiny::actionButton(ns('up'),'Move up')),shiny::column(4,shiny::actionButton(ns('down'),'Move down')),shiny::column(4,shiny::actionButton(ns('remove'),'Remove dataset'))),
 shiny::h4('Canonical observations'),shiny::helpText('Double-click to edit. Validation rejects inconsistent edits. Dataset identity and input mode are protected. Select rows to delete.'),
 DT::DTOutput(ns('table')),shiny::actionButton(ns('add_row'),'Add observation / treatment group'),
 shiny::actionButton(ns('delete_rows'),'Delete selected rows'),
 shiny::h4('Add another response variable without reimporting'),shiny::uiOutput(ns('variables')),
 shiny::textInput(ns('variable_endpoint'),'Biological endpoint name for this variable'),
 shiny::textInput(ns('variable_unit'),'Response unit for this variable'),
 shiny::actionButton(ns('add_variable'),'Add mapped endpoint to this study'),
 shiny::h4('Harmonize an existing study'),shiny::selectInput(ns('remap_source_endpoint'),'Source endpoint to harmonize',choices=character()),
 shiny::textInput(ns('mapped_endpoint'),'Endpoint name'),shiny::textInput(ns('mapped_dose'),'Target dose unit'),shiny::textInput(ns('mapped_response'),'Target response unit'),
 shiny::checkboxInput(ns('confirm_remap'),'I confirm endpoint equivalence and unit conversion'),
 shiny::actionButton(ns('remap'),'Apply explicit harmonization')))
}
data_server <- function(id,state) shiny::moduleServer(id,function(input,output,session) {
 sources <- shiny::reactiveVal(list())
 table_refresh <- shiny::reactiveVal(0L)
 guard <- function(expr) tryCatch(force(expr),error=function(e) {table_refresh(table_refresh()+1L);shiny::showNotification(conditionMessage(e),type='error',duration=12)})
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
 output$preview <- DT::renderDT({shiny::req(source()); DT::datatable(head(source(),100),options=list(scrollX=TRUE,pageLength=5))})
 output$mapping <- shiny::renderUI({
   shiny::req(source()); cols <- names(source()); ns <- session$ns
   keys <- c('dataset_id','endpoint','dose_unit','response_unit',if(input$format!='wide') 'dose',if(input$format!='wide') if(input$mode=='raw') 'value' else c('mean','sd','n'),'treatment',factors)
   aliases <- list(dataset_id=c('study_name','dataset_id'),dose=c('dose_mg_kg_day','dose'),value=c('value','response'),mean=c('mean_serumconc_ng_ML','mean'),sd=c('sd_serumconc','sd'),n=c('n'),exposure=c('days_exposed','exposure'))
   shiny::tagList(if(input$format=='wide') shiny::selectizeInput(ns('wide'),'Response columns (headers must be numeric doses)',cols,multiple=TRUE),
     lapply(keys,function(key) {
       candidate <- intersect(aliases[[key]] %||% key,cols)
       shiny::selectInput(ns(paste0('map_',key)),paste('Map',key),c('(none)',cols),selected=if(length(candidate)) candidate[1] else '(none)')
     }))
 })
 shiny::observeEvent(input$import,guard({
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
   state$project <- p; shiny::showNotification('Datasets added. Existing observations were retained.',type='message')
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
 output$table <- DT::renderDT({table_refresh();DT::datatable(editing(),rownames=FALSE,editable=list(target='cell',disable=list(columns=match(c('dataset_id','mode'),names(empty_data()))-1)),selection='multiple',options=list(scrollX=TRUE,pageLength=10))},server=TRUE)
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
