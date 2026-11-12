editor_property <- function(label,...,advanced=NULL,open=FALSE) shiny::tags$details(class='editor-property',open=if(open) TRUE else NULL,
 shiny::tags$summary(label),shiny::tagList(...),if(!is.null(advanced)) shiny::tags$details(class='editor-advanced',shiny::tags$summary('Advanced settings'),advanced))
editor_section <- function(label,...,open=FALSE) shiny::tags$details(class='editor-category',open=if(open) TRUE else NULL,shiny::tags$summary(label),shiny::tagList(...))
graph_field_specs <- function() list(
 type=list('select','Graph type',c('Dose-response','Scatter','Line','Bar','Mean ± SD','Mean ± SEM','Mean with CI','Boxplot','Violin','Individual points')),
 x_variable=list('select','X variable',c('Dose'='dose','Exposure duration'='exposure','Collection time'='collection_time')),
 p_format=list('select','P-value label format',c('scientific','decimal','threshold')),p_digits=list('number','P-value significant digits (1–17)'),
 title=list('text','Main title (retained when studies change)'),title_visible=list('checkbox','Show main title'),title_auto=list('checkbox','Use endpoint and study count automatically'),
 title_family=list('text','Title font family (blank = base font)'),title_size=list('number','Title size (pt)'),title_face=list('select','Title face',c('plain','bold','italic','bold.italic')),
 title_color=list('text','Title color'),title_hjust=list('number','Title horizontal alignment (0 left, 1 right)'),title_spacing=list('number','Title spacing below (pt)'),
 subtitle=list('text','Subtitle'),subtitle_visible=list('checkbox','Show subtitle'),subtitle_family=list('text','Subtitle font family (blank = base font)'),
 subtitle_size=list('number','Subtitle size (pt)'),subtitle_face=list('select','Subtitle face',c('plain','bold','italic','bold.italic')),
 subtitle_color=list('text','Subtitle color'),subtitle_hjust=list('number','Subtitle horizontal alignment'),subtitle_spacing=list('number','Subtitle spacing below (pt)'),
 color=list('multi','Color grouping',c('study','sex','generation','exposure','collection_time','subgroup','treatment','dose','mode')),
 shape=list('select','Point symbol grouping',c('sex','subgroup','study','generation','exposure','collection_time','treatment','dose','mode')),
 line=list('select','Line style grouping',c('collection_time','study','sex','subgroup','generation','exposure','treatment','dose','mode')),
 facet=list('multi','Facet by',c('study','sex','generation','exposure','collection_time','subgroup','treatment','dose','mode')),
 palette=list('select','Palette',c('okabe','default','Set1','Set2','Dark2','Paired','viridis','plasma','monochrome')),
 colors=list('text','Legacy positional color list (prefer study IDs below)'),shapes=list('text','Custom shape codes (comma-separated)'),line_types=list('text','Custom line types (comma-separated)'),
 point_size=list('number','Point size'),alpha=list('number','Opacity (0–1)'),dim_alpha=list('number','Non-highlighted opacity multiplier (0–1)'),
 line_width=list('number','Line width'),error_line_width=list('number','Error-bar thickness'),error_width=list('number','Error cap / jitter width (plot coordinates)'),dodge_width=list('number','Group spacing (blank = automatic; plot coordinates)'),
 error=list('select','Error bars',c('SD','SEM','CI','None')),error_color=list('text','Error-bar color (blank = group color)'),show_lines=list('checkbox','Connect within independent study and stratum'),
 theme=list('select','Base theme',c('bw','classic','minimal','dark')),font=list('text','Base font family'),font_size=list('number','Base font size (pt)'),
 panel_background=list('text','Panel background (blank = theme)'),plot_background=list('text','Figure background (blank = theme)'),grid_color=list('text','Gridline color'),
 axis_lines=list('checkbox','Show axis lines'),axis_line_color=list('text','Axis line color'),axis_line_width=list('number','Axis line thickness'),
 panel_border=list('checkbox','Show panel border'),border_color=list('text','Panel border color'),
 margin_top=list('number','Top margin (pt)'),margin_right=list('number','Right margin (pt)'),margin_bottom=list('number','Bottom margin (pt)'),margin_left=list('number','Left margin (pt)'),
 legend=list('select','Legend position',c('right','left','top','bottom','inside_tr','inside_br','inside_tl','inside_bl','none')),
 legend_compact=list('checkbox','Group interactive legend by study'),legend_collapsed=list('checkbox','Collapse interactive legend'),legend_static=list('checkbox','Include legend in static exports'),
 legend_size=list('number','Legend font size (pt)'),legend_key_size=list('number','Legend key size (pt)'),panel_spacing=list('number','Facet spacing (lines)'),facet_columns=list('number','Facet columns per interval'),
 export_width=list('number','Figure width (inches)'),export_height=list('number','Figure height (inches)'),export_dpi=list('number','PNG resolution (DPI)'))
graph_sections <- function() list(
 'Graph Type'=c('type','x_variable','show_lines'),
 'Titles & Labels'=c('title','title_visible','title_auto','title_family','title_size','title_face','title_color','title_hjust','title_spacing','subtitle','subtitle_visible','subtitle_family','subtitle_size','subtitle_face','subtitle_color','subtitle_hjust','subtitle_spacing'),
 'Data & Groups'=c('color','shape','line','palette','colors','shapes','line_types','point_size','alpha','dim_alpha','error','error_line_width','error_width','error_color','dodge_width'),
 'Lines & Appearance'=c('line_width','theme','font','font_size','panel_background','plot_background','grid_color','axis_lines','axis_line_color','axis_line_width','panel_border','border_color','margin_top','margin_right','margin_bottom','margin_left'),
 'Legends & Layout'=c('legend','legend_compact','legend_collapsed','legend_static','legend_size','legend_key_size','facet','panel_spacing','facet_columns'),
 'Export Settings'=c('export_width','export_height','export_dpi'))
graph_ui <- function(id) {
 ns <- shiny::NS(id);spec <- graph_field_specs();defaults <- graph_defaults()
 field <- function(nm) {
  x <- spec[[nm]];value <- defaults[[nm]]
  switch(x[[1]],text=shiny::textInput(ns(nm),x[[2]],value),number=shiny::numericInput(ns(nm),x[[2]],value),
   checkbox=shiny::checkboxInput(ns(nm),x[[2]],value),select=shiny::selectInput(ns(nm),x[[2]],x[[3]],selected=value),
   multi=shiny::selectizeInput(ns(nm),x[[2]],x[[3]],selected=value,multiple=TRUE,options=list(maxItems=NULL)))
 }
 fields <- function(keys) shiny::tagList(lapply(keys,field))
 reset_choices <- c(stats::setNames(paste0('graph.',names(spec)),vapply(spec,function(x) x[[2]],character(1))),
  stats::setNames(unlist(lapply(c('x','y'),function(ax) paste0('axes.',ax,'.',names(axis_defaults())))),unlist(lapply(c('x','y'),function(ax) paste(toupper(ax),names(axis_defaults()))))))
 shiny::tagList(
  shiny::div(class='editor-toolbar',shiny::actionButton(ns('undo'),'Undo'),shiny::actionButton(ns('redo'),'Redo'),shiny::textOutput(ns('history'))),
  shiny::helpText('Changes apply automatically after a short pause. Data and statistical calculations are not undone by graph editing.'),
  editor_section('Graph Type',editor_property('Plot & X variable',fields(c('type','x_variable')),advanced=fields('show_lines'))),
  editor_section('Titles & Labels',
   editor_property('Main Title',fields(c('title','title_visible','title_auto')),advanced=fields(c('title_family','title_size','title_face','title_color','title_hjust','title_spacing'))),
   editor_property('Subtitle',fields(c('subtitle','subtitle_visible')),advanced=fields(c('subtitle_family','subtitle_size','subtitle_face','subtitle_color','subtitle_hjust','subtitle_spacing'))),
   axis_labels_ui('axes')),
  editor_section('Axes',editor_property('Axis settings',axes_ui('axes'),open=TRUE)),
  editor_section('Data & Groups',
   editor_property('Dataset colors',fields(c('palette','color')),DT::DTOutput(ns('dataset_styles')),advanced=fields('colors')),
   editor_property('Point symbols & size',fields(c('shape','point_size')),advanced=shiny::tagList(DT::DTOutput(ns('symbol_styles')),fields(c('shapes','dodge_width')))),
   editor_property('Line types',fields('line'),advanced=shiny::tagList(DT::DTOutput(ns('line_styles')),fields('line_types'))),
   editor_property('Error bars',fields(c('error','error_line_width','error_width')),advanced=fields('error_color')),
   editor_property('Transparency & highlighting',fields(c('alpha','dim_alpha')),shiny::helpText('Select studies in the study manager to highlight them.'))),
  editor_section('Statistical Annotations',annotations_ui('annotations')),
  editor_section('Lines & Appearance',
   editor_property('Lines & base theme',fields(c('line_width','theme','font','font_size'))),
   editor_property('Background & grid',fields(c('panel_background','plot_background','grid_color')),shiny::helpText('Gridline visibility is shared with Axes.')),
   editor_property('Axis lines & panel border',fields(c('axis_lines','panel_border')),advanced=fields(c('axis_line_color','axis_line_width','border_color'))),
   editor_property('Margins',fields(c('margin_top','margin_right','margin_bottom','margin_left')))),
  editor_section('Legends & Layout',
   editor_property('Legend',fields(c('legend','legend_compact','legend_collapsed','legend_static')),advanced=fields(c('legend_size','legend_key_size'))),
   editor_property('Facets & spacing',fields(c('facet','facet_columns','panel_spacing')))),
  editor_section('Export Settings',editor_property('Publication dimensions',fields(c('export_width','export_height','export_dpi')),
   shiny::helpText('The same dimensions are used in Export / Projects. PNG, SVG and PDF share the same plot layers.'))),
  editor_section('Presets & Reset',
   shiny::selectInput(ns('style_preset'),'Editable style starting point',c('Clean Scientific','Journal Black & White','Color Publication','Presentation')),
   shiny::actionButton(ns('style_apply'),'Apply style preset'),
   shiny::selectInput(ns('reset_property'),'Reset an individual property',reset_choices),shiny::actionButton(ns('reset_one'),'Reset property'),
   shiny::selectInput(ns('reset_section'),'Reset section',c(names(graph_sections()),'Axes','Statistical Annotations')),
   shiny::actionButton(ns('reset_current'),'Reset selected section'),shiny::actionButton(ns('reset_all'),'Reset all graph settings (includes annotations)'),
   shiny::helpText('Save reusable user presets in Export / Projects. Undo is available for resets.')),
  shiny::actionButton(ns('apply'),'Apply graph settings'),shiny::verbatimTextOutput(ns('status')))
}
graph_server <- function(id,state,data) shiny::moduleServer(id,function(input,output,session) {
 spec <- graph_field_specs();syncing <- shiny::reactiveVal(FALSE);status <- shiny::reactiveVal('Ready. All edits affect display only.')
 sync <- function(g) {
  syncing(TRUE)
  for(nm in names(spec)) {
   v <- g[[nm]];type <- spec[[nm]][[1]]
   if(type=='multi') shiny::updateSelectizeInput(session,nm,selected=v)
   else if(type=='select') shiny::updateSelectInput(session,nm,selected=v)
   else if(type=='checkbox') shiny::updateCheckboxInput(session,nm,value=v)
   else if(type=='number') shiny::updateNumericInput(session,nm,value=v)
   else shiny::updateTextInput(session,nm,value=v)
  }
  shiny::updateTextInput(session,'annotation',value=g$annotation)
  session$onFlushed(function() syncing(FALSE),once=TRUE)
 }
 shiny::observeEvent(data(),{
  source_vars <- grep('^xaux_',names(data()),value=TRUE);source_vars <- source_vars[vapply(data()[source_vars],function(x) all(is.finite(x)),logical(1))]
  shiny::updateSelectInput(session,'x_variable',choices=c('Dose'='dose','Exposure duration'='exposure','Collection time'='collection_time',stats::setNames(source_vars,paste('Source:',sub('^xaux_','',source_vars)))),selected=state$graph$x_variable)
  choices <- graph_types(data());shiny::updateSelectInput(session,'type',choices=choices,selected=if(state$graph$type %in% choices) state$graph$type else choices[1])
 },ignoreNULL=TRUE)
 apply <- function() {
  if(syncing()) return()
  tryCatch({g <- state$graph
   for(nm in names(spec)) {
    v <- input[[nm]]
    if(spec[[nm]][[1]]=='multi') g[[nm]] <- v %||% character() else if(!is.null(v)) g[[nm]] <- if(spec[[nm]][[1]]=='number') as.numeric(v) else v
   }
   g$annotation <- input$annotation %||% g$annotation
   if(nzchar(g$colors)) {g <- migrate_positional_colors(g,data());shiny::updateTextInput(session,'colors',value='')}
   if(nzchar(g$shapes) || nzchar(g$line_types)) {g <- migrate_legacy_symbols(g,data());shiny::updateTextInput(session,'shapes',value='');shiny::updateTextInput(session,'line_types',value='')}
   validate_graph_settings(g);commit_edit(state,graph=g);status('Applied. Scientific observations are unchanged.')
  },error=function(e) status(conditionMessage(e)))
 }
 live <- shiny::debounce(shiny::reactive(lapply(c(names(spec),'annotation'),function(nm) input[[nm]])),400)
 shiny::observeEvent(live(),apply(),ignoreInit=TRUE);shiny::observeEvent(input$apply,apply())
 shiny::observeEvent(state$restore,sync(state$graph),ignoreInit=TRUE)
 output$status <- shiny::renderText(status())
 output$history <- shiny::renderText(paste(length(state$undo %||% list()),'undo ·',length(state$redo %||% list()),'redo'))
 shiny::observeEvent(input$undo,travel_edit(state,'undo'));shiny::observeEvent(input$redo,travel_edit(state,'redo'))
 shiny::observeEvent(input$style_apply,{preset <- style_preset(input$style_preset,state$graph,state$axes);commit_edit(state,preset$graph,preset$axes);state$restore <- state$restore+1L})
 shiny::observeEvent(input$reset_one,{
  parts <- strsplit(input$reset_property,'.',fixed=TRUE)[[1]];g <- state$graph;a <- state$axes
  if(parts[1]=='graph') g[[parts[2]]] <- graph_defaults()[[parts[2]]] else a[[parts[2]]][[parts[3]]] <- axis_defaults()[[parts[3]]]
  commit_edit(state,g,a);state$restore <- state$restore+1L
 })
 shiny::observeEvent(input$reset_current,{
  g <- state$graph;a <- state$axes;keys <- graph_sections()[[input$reset_section]]
  if(length(keys)) for(key in keys) g[[key]] <- graph_defaults()[[key]]
  if(input$reset_section=='Titles & Labels') for(ax in c('x','y')) for(nm in c('title','unit',grep('^title_|^unit_position$',names(axis_defaults()),value=TRUE))) a[[ax]][[nm]] <- axis_defaults()[[nm]]
  if(input$reset_section=='Axes') a <- reset_known_settings(a,axis_settings())
  if(input$reset_section=='Data & Groups') {g$dataset_styles <- graph_defaults()$dataset_styles;g$legacy_group_colors <- graph_defaults()$legacy_group_colors;g$highlight <- character();g$symbol_styles <- graph_defaults()$symbol_styles;g$line_styles <- graph_defaults()$line_styles}
  if(input$reset_section=='Statistical Annotations') {g$annotations <- empty_annotations();g$annotation <- '';g$thresholds <- graph_defaults()$thresholds;g$p_format <- 'scientific';g$p_digits <- 4;g$show_thresholds <- TRUE}
  commit_edit(state,g,a);state$restore <- state$restore+1L
 })
 shiny::observeEvent(input$reset_all,{commit_edit(state,reset_known_settings(state$graph,graph_defaults()),reset_known_settings(state$axes,axis_settings()));state$restore <- state$restore+1L})
 styles <- shiny::reactive({
  p <- state$project;ids <- names(p$datasets);g <- state$graph
  data.frame(dataset_id=ids,label=vapply(ids,function(id) {i <- match(id,g$dataset_styles$dataset_id);if(!is.na(i)) g$dataset_styles$label[i] else ''},character(1)),
   color=vapply(ids,function(id) {i <- match(id,g$dataset_styles$dataset_id);if(!is.na(i) && nzchar(g$dataset_styles$color[i])) g$dataset_styles$color[i] else stable_study_color(id,g$palette)},character(1)))
 })
 output$dataset_styles <- DT::renderDT(DT::datatable(styles(),rownames=FALSE,editable=list(target='cell',disable=list(columns=0)),options=list(scrollX=TRUE,pageLength=5)))
 shiny::observeEvent(input$dataset_styles_cell_edit,{
  tryCatch({i <- input$dataset_styles_cell_edit;d <- styles();if(i$col==0) fail('Study IDs are immutable.');d[i$row,i$col+1] <- as.character(i$value)
   g <- state$graph;g$dataset_styles <- d;validate_graph_settings(g);commit_edit(state,graph=g)
  },error=function(e) shiny::showNotification(conditionMessage(e),type='error',duration=10))
 })
 symbol_table <- function(kind) {
  g <- state$graph;p <- state$project;field <- g[[if(kind=='symbol') 'shape' else 'line']]
  values <- if(field=='study') names(p$datasets) else sort(unique(as.character(p$data[[field]])))
  table <- g[[if(kind=='symbol') 'symbol_styles' else 'line_styles']]
  result <- data.frame(field=rep(field,length(values)),value=values,stringsAsFactors=FALSE)
  result[[kind]] <- vapply(values,function(value) {
   index <- which(table$field==field & table$value==value)
   if(length(index) && !is.na(table[[kind]][index[1]]) && (kind=='symbol' || nzchar(table[[kind]][index[1]]))) return(table[[kind]][index[1]])
   if(kind=='symbol') default_symbol(field,value) else default_line(field,value)
  },if(kind=='symbol') numeric(1) else character(1));result
 }
 for(kind in c('symbol','line')) local({type <- kind;id <- paste0(type,'_styles')
  output[[id]] <- DT::renderDT(DT::datatable(symbol_table(type),rownames=FALSE,editable=list(target='cell',disable=list(columns=c(0,1))),options=list(scrollX=TRUE,pageLength=5)))
  shiny::observeEvent(input[[paste0(id,'_cell_edit')]],{
   tryCatch({i <- input[[paste0(id,'_cell_edit')]];if(i$col!=2) fail('Group identities are protected.')
    shown <- symbol_table(type);row <- shown[i$row,,drop=FALSE];row[[type]] <- DT::coerceValue(i$value,row[[type]])
    g <- state$graph;table <- g[[id]];index <- which(table$field==row$field & table$value==row$value)
    if(length(index)) table[index[1],] <- row else table <- rbind(table,row)
    g[[id]] <- table;validate_graph_settings(g);commit_edit(state,graph=g)
   },error=function(e) shiny::showNotification(conditionMessage(e),type='error',duration=10))
  })
 })

})
