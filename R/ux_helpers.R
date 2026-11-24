study_origin <- function(meta) {
 source <- meta$metadata$source %||% ''
 if(nzchar(source)) return(paste('Imported:',basename(source)))
 if(isTRUE(meta$metadata$example) || grepl('Synthetic data',meta$metadata$notes %||% '',fixed=TRUE)) return('Example data — synthetic')
 'Saved study — source not recorded'
}
apply_study_toggle <- function(project,view,graph,id,field,checked) {
 if(length(id)!=1 || !id %in% names(project$datasets) || !field %in% c('Active','Visible','Highlight') || !is.logical(checked) || length(checked)!=1 || is.na(checked)) fail('Invalid study selection.')
 if(field=='Active') project$selected <- if(checked) union(project$selected,id) else setdiff(project$selected,id)
 if(field=='Visible') {
  view$hidden <- if(checked) setdiff(view$hidden %||% character(),id) else union(view$hidden %||% character(),id)
  if(checked) project$selected <- union(project$selected,id)
 }
 if(field=='Highlight') graph$highlight <- if(checked) union(graph$highlight,id) else setdiff(graph$highlight,id)
 project$selected <- names(project$datasets)[names(project$datasets) %in% project$selected]
 list(project=project,view=view,graph=graph)
}
import_template <- function(mode='raw') {
 if(mode=='summary') data.frame(dose=c(0,1,10),mean=c(4,4.5,5),sd=c(.3,.4,.5),n=c(8,8,8),sex='Female',treatment=c('Control','Low','High'))
 else data.frame(dose=c(0,0,1,1),value=c(4,4.2,4.5,4.7),sex='Female',treatment=c('Control','Control','Low','Low'))
}
source_card <- function(meta,d) shiny::div(class='source-card',shiny::strong(meta$name),shiny::div(study_origin(meta)),
 shiny::div(paste(nrow(d),'stored rows ·',paste(unique(d$endpoint),collapse=', '),'·',paste(ifelse(unique(d$mode)=='raw','Individual measurements','Summary statistics'),collapse=', '))),
 shiny::div(paste('Dose unit:',paste(unique(d$dose_unit),collapse=', '),'· Response unit:',paste(unique(d$response_unit),collapse=', '))))

treatment_choices <- function(d) {
 groups <- unique(d$treatment)
 labels <- vapply(groups,function(group) {
  rows <- d[d$treatment==group,,drop=FALSE]
  n <- if(all(rows$mode=='raw')) nrow(rows) else if(anyNA(rows$n)) 'not supplied' else sum(rows$n)
  paste0(group,' · dose ',paste(unique(rows$dose),collapse=', '),' · n = ',n)
 },character(1))
 stats::setNames(groups,labels)
}
