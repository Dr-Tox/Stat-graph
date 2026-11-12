annotations_ui <- function(id) {
 ns <- shiny::NS(id)
 shiny::tagList(
  shiny::tags$details(open=TRUE,shiny::tags$summary('Manual annotations'),
   shiny::helpText('User-provided annotations do not require a StatLab analysis. Every anchor identifies a study, endpoint and complete experimental stratum.'),
   shiny::selectInput(ns('kind'),'Annotation type',c('Asterisks'='asterisk','Compact letter'='letter','Free group label'='text','Comparison bracket'='bracket','P-value label'='pvalue')),
   shiny::selectInput(ns('dataset'),'Study',choices=character()),shiny::selectInput(ns('endpoint'),'Endpoint',choices=character()),
   shiny::uiOutput(ns('strata')),shiny::selectInput(ns('treatment'),'Treatment / first bracket endpoint',choices=character()),
   shiny::selectInput(ns('treatment2'),'Second bracket endpoint',choices=character()),
   shiny::textInput(ns('text'),'Label','*'),shiny::numericInput(ns('p'),'User-provided p-value (p-value labels)',NA,min=0,max=1),
   shiny::tags$details(shiny::tags$summary('Position & advanced appearance'),
    shiny::numericInput(ns('dose'),'Dose anchor (blank if treatment is unique)',NA),shiny::numericInput(ns('dose2'),'Second dose anchor',NA),
    shiny::numericInput(ns('y'),'Y position in original response units (blank = automatic)',NA),
    shiny::numericInput(ns('dx'),'Horizontal offset (original X units)',0),shiny::numericInput(ns('dy'),'Vertical offset (original Y units)',0),
    shiny::numericInput(ns('size'),'Font size (mm)',5,min=.5),shiny::textInput(ns('color'),'Color','#203844'),
    shiny::selectInput(ns('face'),'Font face',c('plain','bold','italic','bold.italic')),
    shiny::numericInput(ns('line_width'),'Bracket line width',.6,min=.1),shiny::numericInput(ns('tip'),'Bracket tip (fraction of transformed Y range)',.025,min=0)),
   shiny::actionButton(ns('add'),'Add user-provided annotation',class='btn-primary')),
  shiny::tags$details(shiny::tags$summary('Manual letter assignment table'),
   shiny::helpText('Paste TSV or CSV: dataset_id, sex, treatment, letter. Optional endpoint, dose and other stratum columns. Omitted strata must identify exactly one level.'),
   shiny::textAreaInput(ns('letters'),'Letter assignments',rows=5,placeholder='dataset_id,sex,treatment,letter\nstudy_1,Male,Control,a'),shiny::actionButton(ns('import_letters'),'Add letter assignments')),
  shiny::tags$details(shiny::tags$summary('Automatic annotations from Statistics'),
   shiny::verbatimTextOutput(ns('evidence')),
   shiny::selectInput(ns('automatic_kind'),'Automatic display',c('Asterisks on brackets'='asterisk','P-values on brackets'='pvalue','Complete all-pairs compact letters'='letter')),
   shiny::actionButton(ns('automatic'),'Use validated analysis'),
   shiny::tags$details(shiny::tags$summary('Asterisk thresholds & notes'),DT::DTOutput(ns('thresholds')),
    shiny::selectInput('graphs-p_format','P-value label format',c('scientific','decimal','threshold')),shiny::numericInput('graphs-p_digits','P-value significant digits (1–17)',4,min=1,max=17),
    shiny::actionButton(ns('threshold_add'),'Add threshold'),shiny::actionButton(ns('threshold_remove'),'Remove selected threshold'),
    shiny::checkboxInput(ns('show_thresholds'),'Include threshold convention in figure note',TRUE))),
  shiny::tags$details(open=TRUE,shiny::tags$summary('Manage annotations'),
   shiny::helpText('Double-click a cell to edit; select rows to delete. Editing an automatic label or anchor changes its source to manual. Positions and styles can be adjusted independently.'),
   DT::DTOutput(ns('table')),shiny::actionButton(ns('delete'),'Delete selected annotations'),shiny::actionButton(ns('clear'),'Clear annotations'),
   shiny::tags$details(shiny::tags$summary('Legacy 2.0 corner label'),shiny::textInput('graphs-annotation','User-provided corner text (legacy positioning)'))))
}
import_letter_assignments <- function(text,p,endpoint) {
 sep <- if(grepl('\t',text,fixed=TRUE)) '\t' else ','
 tab <- utils::read.table(text=text,sep=sep,header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
 if(!all(c('dataset_id','treatment','letter') %in% names(tab))) fail('Letter assignments require dataset_id, treatment and letter columns.')
 out <- list()
 for(i in seq_len(nrow(tab))) {
  row <- tab[i,,drop=FALSE];ep <- if('endpoint' %in% names(tab)) row$endpoint else endpoint
  d <- p$data[p$data$dataset_id==row$dataset_id & p$data$endpoint==ep & p$data$treatment==row$treatment,,drop=FALSE]
  for(nm in intersect(c(factors,'dose'),names(tab))) d <- d[d[[nm]]==row[[nm]],,drop=FALSE]
  if(!nrow(d)) fail(paste('Letter row',i,'has no matching experimental group.'))
  if(any(vapply(d[c(factors,'dose')],function(v) length(unique(v))!=1,logical(1)))) fail(paste('Letter row',i,'is ambiguous: specify all varying strata and dose.'))
  anchors <- as.list(d[1,c('dataset_id','endpoint','treatment','dose',factors)])
  out[[i]] <- do.call(new_annotation,c(anchors,list(type='letter',text=as.character(row$letter))))
 }
 dplyr::bind_rows(out)
}
annotations_server <- function(id,state) shiny::moduleServer(id,function(input,output,session) {
 revision <- shiny::reactiveVal(0L)
 guard <- function(expr) tryCatch(force(expr),error=function(e) {revision(revision()+1L);shiny::showNotification(conditionMessage(e),type='error',duration=12)})
 save <- function(anns) {g <- state$graph;g$annotations <- normalize_table(anns,empty_annotations(),annotation_row());validate_annotations(g$annotations);commit_edit(state,graph=g)}
 shiny::observeEvent(list(state$project$datasets,state$restore),{
  p <- state$project;ids <- names(p$datasets)
  shiny::updateSelectInput(session,'dataset',choices=stats::setNames(ids,vapply(p$datasets,function(d) paste(d$name,d$id),character(1))),selected=if(length(input$dataset) && input$dataset %in% ids) input$dataset else ids[1])
 })
 shiny::observeEvent(list(input$dataset,state$project$data),{p <- state$project;ep <- unique(p$data$endpoint[p$data$dataset_id==input$dataset]);shiny::updateSelectInput(session,'endpoint',choices=ep,selected=if(length(input$endpoint) && input$endpoint %in% ep) input$endpoint else ep[1])})
 base <- shiny::reactive({d <- state$project$data;d[d$dataset_id==input$dataset & d$endpoint==input$endpoint,,drop=FALSE]})
 output$strata <- shiny::renderUI({d <- base();shiny::tagList(lapply(factors,function(nm) shiny::selectInput(session$ns(nm),gsub('_',' ',nm),unique(d[[nm]]),selected=shiny::isolate(input[[nm]]))))})
 scoped <- shiny::reactive({d <- base();for(nm in factors) {shiny::req(input[[nm]]);d <- d[d[[nm]]==input[[nm]],,drop=FALSE]};d})
 shiny::observeEvent(scoped(),{groups <- unique(scoped()$treatment);for(nm in c('treatment','treatment2')) shiny::updateSelectInput(session,nm,choices=groups,selected=if(length(input[[nm]]) && input[[nm]] %in% groups) input[[nm]] else groups[1])})
 shiny::observeEvent(input$add,guard({
  anchors <- stats::setNames(lapply(factors,function(nm) input[[nm]]),factors)
  text <- if(input$kind=='pvalue') {if(!is.finite(input$p) || input$p<0 || input$p>1) fail('Enter a user-provided p-value between 0 and 1.');paste0('p = ',format(input$p,digits=4))} else input$text
  row <- do.call(new_annotation,c(anchors,list(type=input$kind,dataset_id=input$dataset,endpoint=input$endpoint,treatment=input$treatment,treatment2=if(input$kind=='bracket') input$treatment2 else '',
    dose=input$dose %||% NA_real_,dose2=input$dose2 %||% NA_real_,text=text,p=input$p %||% NA_real_,y=input$y %||% NA_real_,dx=input$dx,dy=input$dy,size=input$size,color=input$color,face=input$face,line_width=input$line_width,tip=input$tip)))
  validate_annotations(row);save(dplyr::bind_rows(state$graph$annotations,row))
 }))
 shiny::observeEvent(input$import_letters,guard(save(dplyr::bind_rows(state$graph$annotations,import_letter_assignments(input$letters,state$project,input$endpoint)))))
 output$table <- DT::renderDT({revision();d <- effective_annotations(state$graph);d$evidence <- NULL;DT::datatable(d,rownames=FALSE,editable=list(target='cell',disable=list(columns=match(c('id','source'),names(d))-1)),selection='multiple',options=list(scrollX=TRUE,pageLength=5))})
 shiny::observeEvent(input$table_cell_edit,guard({
  info <- input$table_cell_edit;d <- state$graph$annotations;col <- setdiff(names(d),'evidence')[info$col+1]
  if(col %in% c('id','source')) fail('Annotation identity/provenance is protected.')
  d[info$row,col] <- DT::coerceValue(info$value,d[[col]])
  if(col=='visible' && is.na(d$visible[info$row])) fail('Visibility must be true or false.')
  if(col=='p') {
   if(!is.finite(d$p[info$row]) || d$p[info$row]<0 || d$p[info$row]>1) fail('P-values must be between 0 and 1.')
   if(d$label_mode[info$row]=='pvalue' || d$type[info$row]=='pvalue') d$text[info$row] <- format_p_label(d$p[info$row],state$graph)
   if(d$label_mode[info$row]=='asterisk') d$text[info$row] <- p_symbol(d$p[info$row],state$graph$thresholds)
  }
  if(!col %in% c('y','dx','dy','size','color','face','visible','line_width','tip')) {d$source[info$row] <- 'manual';d$evidence[info$row] <- ''}
  save(d)
 }))
 shiny::observeEvent(input$delete,{if(length(input$table_rows_selected)) save(state$graph$annotations[-input$table_rows_selected,,drop=FALSE])})
 shiny::observeEvent(input$clear,save(empty_annotations()))
 output$thresholds <- DT::renderDT(DT::datatable(state$graph$thresholds,rownames=FALSE,editable=TRUE,selection='single',options=list(dom='t')))
 shiny::observeEvent(input$thresholds_cell_edit,guard({d <- state$graph$thresholds;i <- input$thresholds_cell_edit;d[i$row,i$col+1] <- DT::coerceValue(i$value,d[[i$col+1]]);validate_thresholds(d);g <- state$graph;g$thresholds <- d;commit_edit(state,graph=g)}))
 shiny::observeEvent(input$threshold_add,guard({g <- state$graph;cutoff <- min(g$thresholds$p)/10;g$thresholds <- rbind(data.frame(p=cutoff,label='*****'),g$thresholds);validate_thresholds(g$thresholds);commit_edit(state,graph=g)}))
 shiny::observeEvent(input$threshold_remove,guard({g <- state$graph;if(length(input$thresholds_rows_selected)) g$thresholds <- g$thresholds[-input$thresholds_rows_selected,,drop=FALSE];validate_thresholds(g$thresholds);commit_edit(state,graph=g)}))
 shiny::observeEvent(input$show_thresholds,{g <- state$graph;if(!identical(input$show_thresholds,g$show_thresholds)){g$show_thresholds <- input$show_thresholds;commit_edit(state,graph=g)}})
 shiny::observeEvent(state$restore,{shiny::updateCheckboxInput(session,'show_thresholds',value=state$graph$show_thresholds)},ignoreInit=TRUE)
 output$evidence <- shiny::renderText({ctx <- state$project$analysis_context;if(is.null(ctx)) 'Run Statistics to enable automatic annotations. Manual annotations remain available.' else paste('Last analysis:',ctx$settings$method,'· study',ctx$settings$dataset,'·',ctx$settings$endpoint,'\nOnly validated pairwise comparisons can be annotated automatically.')})
 shiny::observeEvent(input$automatic,guard({g <- state$graph;g$annotations <- dplyr::bind_rows(g$annotations,automatic_annotations(state$project,input$automatic_kind,g$thresholds));validate_annotations(g$annotations);commit_edit(state,graph=g)}))
})
