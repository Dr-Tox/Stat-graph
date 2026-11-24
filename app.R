# Run from the StatLab directory with shiny::runApp().
required <- c('shiny','bslib','ggplot2','plotly','dplyr','tidyr','readr','readxl','DT','patchwork','scales','jsonlite','openxlsx','multcomp','svglite','RColorBrewer','rlang','digest')
missing <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop('Install missing packages with scripts/install.R: ',paste(missing,collapse=', '))
for(file in list.files('R',pattern='\\.R$',full.names=TRUE)) source(file,local=TRUE)
options(shiny.maxRequestSize=500*1024^2)
ui <- shiny::navbarPage(title=shiny::span(class='brand','StatLab ',shiny::span('2.2.0',class='version')),
 theme=bslib::bs_theme(version=5,bootswatch='flatly',primary='#146c78',base_font='sans-serif'),
 header=shiny::tagList(shiny::tags$link(rel='stylesheet',type='text/css',href='styles.css'),shiny::tags$script(src='editor.js'),
 shiny::div(class='topline','SCIENTIFIC COMPARISON WORKSPACE',shiny::span('LOCAL DATA · INDEPENDENT STUDIES',class='local-badge'))),
 shiny::tabPanel('Compare Datasets',compare_ui('compare')),
 shiny::tabPanel('Data Manager',data_ui('data')),
 shiny::tabPanel('Statistics',statistics_ui('statistics')),
 shiny::tabPanel('Results',shiny::h3('Last explicitly executed statistical comparison'),shiny::p('Results describe the saved analysis selection. Re-run inference after changing statistical settings.'),DT::DTOutput('results'),
   shiny::h3('Current comparison: descriptive summaries'),DT::DTOutput('descriptions')),
 shiny::tabPanel('Export / Projects',export_ui('export')),
 footer=shiny::div(class='footer','StatLab 2.2.0 · Scientific visualization first · Synthetic example studies loaded for exploration'))
server <- function(input,output,session) {
 state <- shiny::reactiveValues(project=example_project(),axes=axis_settings(),graph=graph_defaults(),
   view=list(endpoint='Relative liver weight',filters=list()),stats_settings=list(),restore=0L,undo=list(),redo=list())
 cmp <- compare_server('compare',state)
 axes_server('axes',state,cmp$prepared);graph_server('graphs',state,cmp$data);annotations_server('annotations',state)
 data_server('data',state);statistics_server('statistics',state);export_server('export',state,cmp)
 output$results <- DT::renderDT(DT::datatable(state$project$results,options=list(scrollX=TRUE)))
 output$descriptions <- DT::renderDT(DT::datatable(summarise_groups(cmp$data()),options=list(scrollX=TRUE)))
 last_data <- shiny::reactiveVal(NULL)
 last_restore <- shiny::reactiveVal(0L)
 shiny::observeEvent(state$project$data,{
  current <- state$project$data
  if(identical(last_restore(),state$restore) && !is.null(last_data()) && !identical(last_data(),current) && nrow(state$project$results)) {
    p <- state$project;p$results <- data.frame();state$project <- p
  }
  last_data(current);last_restore(state$restore)
 },ignoreInit=FALSE)
}
shiny::shinyApp(ui,server)
