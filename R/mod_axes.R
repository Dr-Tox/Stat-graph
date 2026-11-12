axis_defaults <- function() c(list(scale='linear',min=NA_real_,max=NA_real_,major=numeric(),minor=numeric(),
 labels=character(),interval=NA_real_,title='',unit='',text_size=12,reverse=FALSE,format='auto',sigma=1),axis_extensions())
axis_settings <- function() list(x=axis_defaults(),y=axis_defaults(), intervals=data.frame(start=numeric(),end=numeric(),width=numeric()),
 zero='exclude', independent_y=FALSE, grid=TRUE,break_symbol='//',break_color='#384e59',break_size=18)
parse_numbers <- function(text) {
 if (!nzchar(trimws(text %||% ''))) return(numeric())
 x <- suppressWarnings(as.numeric(trimws(strsplit(text,',',fixed=TRUE)[[1]])))
 if(any(!is.finite(x))) fail('Tick positions must be comma-separated finite numbers.'); x
}
parse_intervals <- function(text) {
 if(!nzchar(trimws(text %||% ''))) return(data.frame(start=numeric(),end=numeric(),width=numeric()))
 rows <- strsplit(trimws(text),'\n')[[1]]
 values <- lapply(rows,parse_numbers)
 if(any(lengths(values)!=3)) fail('Each interval needs: start,end,relative width.')
 d <- as.data.frame(do.call(rbind,values)); names(d) <- c('start','end','width'); d
}
validate_axis <- function(a) {
 for(nm in c('min','max','interval')) if(length(a[[nm]])!=1 || !(is.numeric(a[[nm]]) || (is.logical(a[[nm]]) && is.na(a[[nm]]))) || (!is.na(a[[nm]]) && !is.finite(a[[nm]]))) fail('Axis limits and intervals must be finite numbers or blank.')
 for(nm in c('text_size','title_size')) if(!is.numeric(a[[nm]]) || !is.finite(a[[nm]]) || a[[nm]]<=0) fail(paste(nm,'must be positive.'))
 for(nm in c('title_margin','tick_length')) if(!is.numeric(a[[nm]]) || !is.finite(a[[nm]]) || a[[nm]]<0) fail(paste(nm,'must be nonnegative.'))
 if(!is.numeric(a$tick_angle) || !is.finite(a$tick_angle)) fail('Tick angle must be finite.')
 for(nm in c('title_color','tick_color')) tryCatch(grDevices::col2rgb(a[[nm]]),error=function(e) fail(paste('Invalid',nm)))
 for(nm in c('major','minor')) if(!is.numeric(a[[nm]]) || any(!is.finite(a[[nm]]))) fail('Axis ticks must be finite numeric values.')
 if(!a$scale %in% c('linear','log10','pseudo_log')) fail('Unknown axis scale.')
 if(!is.na(a$min) && !is.na(a$max) && a$min>=a$max) fail('Axis minimum must be smaller than maximum.')
 if(a$scale=='log10' && any(c(a$min,a$max,a$major,a$minor)<=0,na.rm=TRUE)) fail('Log10 limits and ticks must be strictly positive.')
 if(!is.finite(a$sigma) || a$sigma<=0) fail('Pseudo-log sigma must be positive.')
 if(!is.na(a$interval) && (!is.finite(a$interval) || a$interval<=0)) fail('Tick interval must be positive.')
 if(length(a$labels) && length(a$labels)!=length(a$major)) fail('Provide one custom label for each major tick.')
 invisible(a)
}
validate_intervals <- function(intervals,data=NULL,log=FALSE) {
 if(!nrow(intervals)) return(invisible(intervals))
 if(any(!is.finite(as.matrix(intervals))) || any(intervals$start>=intervals$end) || any(intervals$width<=0))
   fail('Intervals require finite start < end and positive relative widths.')
 if(nrow(intervals)>1 && any(intervals$start[-1]<=intervals$end[-nrow(intervals)])) fail('Intervals must be ordered and non-overlapping, with a positive gap.')
 if(log && any(intervals$start<=0)) fail('Log10 interval boundaries must be positive. Use the separate-control option for zero.')
 if(!is.null(data)) {if('.x' %in% names(data)) data$dose <- data$.x}
 if(!is.null(data) && any(vapply(seq_len(nrow(intervals)),function(i) !any(data$dose>=intervals$start[i] & data$dose<=intervals$end[i]),logical(1))))
   fail('An interval has no observations after filtering. Adjust the intervals or selected studies.')
 invisible(intervals)
}
axis_transform <- function(a) {
 tr <- switch(a$scale,linear=scales::identity_trans(),log10=scales::log10_trans(),
   pseudo_log=scales::pseudo_log_trans(sigma=a$sigma,base=10))
 if(a$reverse) scales::trans_new(paste0('reverse_',tr$name),
   transform=function(x) -tr$transform(x),inverse=function(x) tr$inverse(-x),
   breaks=tr$breaks,domain=tr$domain) else tr
}
axis_scale <- function(a,which='x',range=NULL,annotation_padding=FALSE) {
 validate_axis(a)
 breaks <- if(length(a$major)) a$major else if(!is.na(a$interval)) function(lim) {
   lo <- ceiling(min(lim)/a$interval)*a$interval; hi <- floor(max(lim)/a$interval)*a$interval
   if((hi-lo)/a$interval>10000) fail('Tick interval would generate more than 10,000 ticks; increase it.')
   if(lo>hi) numeric() else seq(lo,hi,by=a$interval)
 } else ggplot2::waiver()
 labels <- if(length(a$labels)) a$labels else switch(a$format,scientific=scales::label_scientific(),
   decimal=scales::label_number(),auto=ggplot2::waiver())
 fn <- if(which=='x') ggplot2::scale_x_continuous else ggplot2::scale_y_continuous
 fn(trans=axis_transform(a),breaks=breaks,expand=if(annotation_padding) ggplot2::expansion(mult=c(.05,.18)) else ggplot2::waiver(),minor_breaks=if(length(a$minor)) a$minor else ggplot2::waiver(),labels=labels)
}
axis_labels_ui <- function(id) {
 ns <- shiny::NS(id)
 shiny::tagList(lapply(c('x','y'),function(ax) shiny::tags$details(shiny::tags$summary(paste(toupper(ax),'axis title & units')),
  shiny::textInput(ns(paste0(ax,'_title')),'Title (blank = endpoint / X metadata)'),
  shiny::textInput(ns(paste0(ax,'_unit')),'Display unit (blank = mapped metadata; never converts data)'),
  shiny::checkboxInput(ns(paste0(ax,'_title_visible')),'Show axis title',TRUE),
  shiny::selectInput(ns(paste0(ax,'_unit_position')),'Units',c('In title'='title','Separate line'='separate','Hidden'='hidden')),
  shiny::tags$details(shiny::tags$summary('Advanced typography'),
   shiny::numericInput(ns(paste0(ax,'_title_size')),'Title size (pt)',13,min=6),
   shiny::textInput(ns(paste0(ax,'_title_color')),'Title color','#203844'),
   shiny::selectInput(ns(paste0(ax,'_title_face')),'Title face',c('plain','bold','italic','bold.italic')),
   shiny::textInput(ns(paste0(ax,'_title_family')),'Title font family (blank = base font)'),
   shiny::numericInput(ns(paste0(ax,'_title_margin')),'Distance from axis (pt)',10,min=0)))))
}
axes_ui <- function(id) {
 ns <- shiny::NS(id)
 shiny::tagList(shiny::helpText('Valid edits update the graph. Invalid drafts leave the last valid figure unchanged. All coordinates remain in original units.'),
  lapply(c('x','y'),function(ax) shiny::tags$details(shiny::tags$summary(paste(toupper(ax),'axis')),
   shiny::selectInput(ns(paste0(ax,'_scale')),'Scale',c('Linear'='linear','Log10'='log10','Pseudo-log'='pseudo_log')),
   shiny::checkboxInput(ns(paste0(ax,'_reverse')),'Reverse direction',FALSE),
   shiny::tags$details(shiny::tags$summary('Limits & transformation'),
    shiny::numericInput(ns(paste0(ax,'_min')),'Minimum (blank = automatic)',NA),shiny::numericInput(ns(paste0(ax,'_max')),'Maximum (blank = automatic)',NA),
    shiny::numericInput(ns(paste0(ax,'_sigma')),'Pseudo-log sigma',1,min=.000001)),
   shiny::tags$details(shiny::tags$summary('Ticks & advanced styling'),
    shiny::textInput(ns(paste0(ax,'_major')),'Major ticks (comma-separated)'),shiny::textInput(ns(paste0(ax,'_minor')),'Minor ticks (comma-separated)'),
    shiny::textInput(ns(paste0(ax,'_labels')),'Custom major tick labels (comma-separated)'),shiny::numericInput(ns(paste0(ax,'_interval')),'Major interval (original units)',NA),
    shiny::selectInput(ns(paste0(ax,'_format')),'Numeric format',c('auto','decimal','scientific')),
    shiny::numericInput(ns(paste0(ax,'_text_size')),'Tick font size (pt)',12,min=6),shiny::numericInput(ns(paste0(ax,'_tick_angle')),'Tick rotation (degrees)',0),
    shiny::textInput(ns(paste0(ax,'_tick_color')),'Tick label color','#465b68'),
    shiny::selectInput(ns(paste0(ax,'_tick_face')),'Tick font face',c('plain','bold','italic','bold.italic')),
    shiny::textInput(ns(paste0(ax,'_tick_family')),'Tick font family (blank = base font)'),
    shiny::numericInput(ns(paste0(ax,'_tick_length')),'Tick length (pt)',4,min=0)))),
  shiny::tags$details(open=TRUE,shiny::tags$summary('X intervals & breaks'),
   DT::DTOutput(ns('interval_table')),
   shiny::fluidRow(shiny::column(4,shiny::numericInput(ns('new_start'),'Minimum',0)),shiny::column(4,shiny::numericInput(ns('new_end'),'Maximum',1)),shiny::column(4,shiny::numericInput(ns('new_width'),'Width',1,min=.01))),
   shiny::actionButton(ns('add_interval'),'Add interval'),shiny::actionButton(ns('remove_interval'),'Remove selected'),
   shiny::actionButton(ns('interval_up'),'Move up'),shiny::actionButton(ns('interval_down'),'Move down'),shiny::actionButton(ns('auto_intervals'),'Automatic intervals'),
   shiny::verbatimTextOutput(ns('status')),
   shiny::tags$details(shiny::tags$summary('Advanced break indicators & text interchange'),
    shiny::textInput(ns('break_symbol'),'Break indicator','//'),shiny::textInput(ns('break_color'),'Break indicator color','#384e59'),
    shiny::numericInput(ns('break_size'),'Break indicator size (pt)',18,min=6),
    shiny::textAreaInput(ns('intervals'),'2.0 interval text interchange: start,end,width',rows=4),shiny::actionButton(ns('import_text'),'Import interval text'))),
  shiny::selectInput(ns('zero'),'Zero X on log10',c('Warn and exclude'='exclude','Separate zero-control panel'='separate')),
  shiny::checkboxInput(ns('independent_y'),'Independent Y ranges (explicitly labeled)',FALSE),shiny::checkboxInput(ns('grid'),'Show gridlines',TRUE),
  shiny::actionButton(ns('apply'),'Apply axis settings'),shiny::actionButton(ns('reset'),'Restore automatic scaling'))
}
move_interval <- function(d,index,direction) {
 target <- index+direction
 if(length(index)!=1 || is.na(index) || target<1 || target>nrow(d)) return(d)
 order <- seq_len(nrow(d));order[c(index,target)] <- order[c(target,index)];d[order,,drop=FALSE]
}
axes_server <- function(id,state,data=NULL) shiny::moduleServer(id,function(input,output,session) {
 selected_interval <- shiny::reactiveVal(integer())
 draft <- shiny::reactiveVal(shiny::isolate(state$axes$intervals));status <- shiny::reactiveVal('Automatic X range.');syncing <- shiny::reactiveVal(FALSE)
 field_ids <- unlist(lapply(c('x','y'),function(ax) paste0(ax,'_',names(axis_defaults()))))
 select_fields <- c('scale','format','title_face','unit_position','tick_face')
 text_fields <- c('major','minor','labels','title','unit','title_color','title_family','tick_color','tick_family')
 sync <- function(a) {
  syncing(TRUE)
  for(ax in c('x','y')) for(nm in names(axis_defaults())) {
   value <- a[[ax]][[nm]];key <- paste0(ax,'_',nm)
   if(nm %in% c('major','minor','labels')) shiny::updateTextInput(session,key,value=paste(value,collapse=','))
   else if(nm %in% text_fields) shiny::updateTextInput(session,key,value=value)
   else if(nm %in% select_fields) shiny::updateSelectInput(session,key,selected=value)
   else if(is.logical(value) && !is.na(value)) shiny::updateCheckboxInput(session,key,value=value)
   else shiny::updateNumericInput(session,key,value=value)
  }
  selected_interval(integer());draft(a$intervals);shiny::updateTextAreaInput(session,'intervals',value='')
  shiny::updateSelectInput(session,'zero',selected=a$zero)
  for(nm in c('grid','independent_y')) shiny::updateCheckboxInput(session,nm,value=a[[nm]])
  for(nm in c('break_symbol','break_color')) shiny::updateTextInput(session,nm,value=a[[nm]])
  shiny::updateNumericInput(session,'break_size',value=a$break_size)
  session$onFlushed(function() syncing(FALSE),once=TRUE)
 }
 collect <- function(use_text=FALSE) {
  a <- state$axes
  for(ax in c('x','y')) {
   for(nm in names(axis_defaults())) {
    v <- input[[paste0(ax,'_',nm)]]
    if(!is.null(v)) a[[ax]][[nm]] <- if(is.numeric(axis_defaults()[[nm]]) && !nm %in% c('major','minor')) as.numeric(v) else v
   }
   for(nm in c('major','minor')) {
    v <- input[[paste0(ax,'_',nm)]]
    if(!is.null(v)) a[[ax]][[nm]] <- parse_numbers(v)
   }
   v <- input[[paste0(ax,'_labels')]]
   if(!is.null(v)) a[[ax]]$labels <- if(nzchar(v)) trimws(strsplit(v,',',fixed=TRUE)[[1]]) else character()
   validate_axis(a[[ax]])
  }
  a$intervals <- if(use_text && nzchar(input$intervals %||% '')) parse_intervals(input$intervals) else draft()
  for(nm in c('zero','independent_y','grid','break_symbol','break_color','break_size')) a[[nm]] <- input[[nm]] %||% a[[nm]]
  validate_intervals(a$intervals,if(!is.null(data)) data() else NULL,log=a$x$scale=='log10')
  tryCatch(grDevices::col2rgb(a$break_color),error=function(e) fail('Invalid break-indicator color.'))
  a
 }
 apply <- function(use_text=FALSE) {
  if(syncing()) return()
  tryCatch({a <- collect(use_text);commit_edit(state,axes=a);status(if(nrow(a$intervals)) paste(nrow(a$intervals),'valid visible intervals. Reordered drafts must remain in ascending dose order.') else 'Automatic X range.')},
   error=function(e) {status(conditionMessage(e));if(use_text) shiny::showNotification(conditionMessage(e),type='error',duration=10)})
 }
 output$interval_table <- DT::renderDT({d <- draft();rownames(d) <- NULL;DT::datatable(d,rownames=TRUE,editable=list(target='cell',disable=list(columns=0)),selection=list(mode='single',selected=shiny::isolate(selected_interval())),options=list(dom='t',scrollX=TRUE,pageLength=8,ordering=FALSE))})
 shiny::observeEvent(input$interval_table_rows_selected,selected_interval(input$interval_table_rows_selected %||% integer()),ignoreNULL=FALSE)
 output$status <- shiny::renderText(status())
 shiny::observeEvent(input$interval_table_cell_edit,{
  i <- input$interval_table_cell_edit;d <- draft();d[i$row,i$col] <- suppressWarnings(as.numeric(i$value));draft(d);apply()
 })
 shiny::observeEvent(input$add_interval,{d <- draft();d <- rbind(d,data.frame(start=input$new_start,end=input$new_end,width=input$new_width));selected_interval(nrow(d));draft(d);apply()})
 shiny::observeEvent(input$remove_interval,{rows <- selected_interval();if(length(rows)){d <- draft()[-rows,,drop=FALSE];selected_interval(if(nrow(d)) min(rows,nrow(d)) else integer());draft(d);apply()}})
 for(nm in c('interval_up','interval_down')) local({key <- nm;shiny::observeEvent(input[[key]],{rows <- selected_interval();if(length(rows)){direction <- if(key=='interval_up') -1L else 1L;d <- move_interval(draft(),rows,direction);selected_interval(max(1L,min(nrow(d),rows+direction)));draft(d);apply()}})})
 shiny::observeEvent(input$auto_intervals,{selected_interval(integer());draft(axis_settings()$intervals);shiny::updateTextAreaInput(session,'intervals',value='');apply()})
 shiny::observeEvent(input$import_text,{tryCatch({draft(parse_intervals(input$intervals));apply()},error=function(e) status(conditionMessage(e)))})
 live <- shiny::debounce(shiny::reactive(lapply(c(field_ids,'zero','independent_y','grid','break_symbol','break_color','break_size'),function(id) input[[id]])),400)
 shiny::observeEvent(live(),apply(),ignoreInit=TRUE)
 shiny::observeEvent(input$apply,apply(TRUE))
 shiny::observeEvent(input$reset,{a <- reset_known_settings(state$axes,axis_settings());commit_edit(state,axes=a);sync(a)})
 shiny::observeEvent(state$restore,sync(state$axes),ignoreInit=TRUE)
})
