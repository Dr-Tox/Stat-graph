# All 2.1 properties extend the existing graph/axis configuration.
graph_extensions <- function() list(
 title_visible=TRUE,title_auto=FALSE,title_family='',title_size=18,title_face='bold',title_color='#203844',title_hjust=0,title_spacing=12,
 subtitle_visible=TRUE,subtitle_family='',subtitle_size=12,subtitle_face='plain',subtitle_color='#536875',subtitle_hjust=0,subtitle_spacing=8,
 symbol_styles=data.frame(field=character(),value=character(),symbol=numeric()),line_styles=data.frame(field=character(),value=character(),line=character()),
 error_line_width=.7,dodge_width=NA_real_,panel_background='',plot_background='',grid_color='#e5e9ed',axis_lines=TRUE,axis_line_color='#394d58',axis_line_width=.5,
 panel_border=TRUE,border_color='#a9b6bf',margin_top=12,margin_right=12,margin_bottom=12,margin_left=12,
 legend_compact=TRUE,legend_collapsed=FALSE,legend_static=TRUE,legend_size=10,legend_key_size=12,facet_columns=1,
 highlight=character(),dim_alpha=.15,dataset_styles=data.frame(dataset_id=character(),label=character(),color=character()),
 annotations=empty_annotations(),legacy_group_colors=data.frame(key=character(),color=character()),thresholds=data.frame(p=c(.0001,.001,.01,.05),label=c('****','***','**','*')),
 show_thresholds=TRUE,p_format='scientific',p_digits=4,export_width=12,export_height=7,export_dpi=300)
axis_extensions <- function() list(title_visible=TRUE,title_size=13,title_color='#203844',title_face='plain',title_family='',title_margin=10,
 unit_position='title',tick_angle=0,tick_color='#465b68',tick_face='plain',tick_family='',tick_length=4)
normalize_table <- function(value,template,defaults=NULL) {
 d <- if(is.data.frame(value)) value else if(length(value)) as.data.frame(dplyr::bind_rows(value)) else template
 for(nm in names(template)) {
  proto <- template[[nm]]
  default <- defaults[[nm]] %||% if(is.numeric(proto)) NA_real_ else if(is.logical(proto)) FALSE else ''
  if(!nm %in% names(d)) d[[nm]] <- rep(default,nrow(d))
  d[[nm]] <- if(is.numeric(proto)) {v <- suppressWarnings(as.numeric(d[[nm]]));if(any(!is.na(d[[nm]]) & nzchar(as.character(d[[nm]])) & is.na(v))) fail(paste('Nonnumeric table field:',nm));v} else if(is.logical(proto)) as.logical(d[[nm]]) else as.character(d[[nm]])
  if(!is.na(default)) d[[nm]][is.na(d[[nm]])] <- default
 }
 d[c(names(template),setdiff(names(d),names(template)))]
}
validate_graph_settings <- function(g) {
 for(nm in c('point_size','line_width','error_line_width','font_size','title_size','subtitle_size','legend_size','export_width','export_height','export_dpi'))
  if(!is.numeric(g[[nm]]) || length(g[[nm]])!=1 || !is.finite(g[[nm]]) || g[[nm]]<=0) fail(paste(nm,'must be positive.'))
 for(nm in c('alpha','dim_alpha','title_hjust','subtitle_hjust')) if(!is.finite(g[[nm]]) || g[[nm]]<0 || g[[nm]]>1) fail(paste(nm,'must be between 0 and 1.'))
 if(!g$theme %in% c('bw','minimal','classic','dark')) fail('Unknown graph theme.')
 for(nm in c('title_color','subtitle_color','error_color','panel_background','plot_background','grid_color','axis_line_color','border_color'))
  if(nzchar(g[[nm]])) tryCatch(grDevices::col2rgb(g[[nm]]),error=function(e) fail(paste('Invalid color for',nm)))
 if(nrow(g$dataset_styles)) {
  if(anyDuplicated(g$dataset_styles$dataset_id)) fail('Each dataset style must have a unique study ID.')
  for(color in g$dataset_styles$color[nzchar(g$dataset_styles$color)]) tryCatch(grDevices::col2rgb(color),error=function(e) fail('Invalid dataset color.'))
 }
 if(!is.na(g$dodge_width) && (!is.finite(g$dodge_width) || g$dodge_width<=0)) fail('Grouping width must be positive or automatic.')
 if(!g$p_format %in% c('scientific','decimal','threshold') || !is.finite(g$p_digits) || g$p_digits<1 || g$p_digits>17) fail('P-value format must use 1–17 significant digits.')
 if(nrow(g$symbol_styles) && any(!is.na(g$symbol_styles$symbol) & (g$symbol_styles$symbol<0 | g$symbol_styles$symbol>25 | g$symbol_styles$symbol!=floor(g$symbol_styles$symbol)))) fail('Symbol codes must be integers from 0 to 25, or blank for automatic.')
 if(nrow(g$line_styles) && any(!g$line_styles$line %in% c('','blank','solid','dashed','dotted','dotdash','longdash','twodash') & !grepl('^([0-9A-Fa-f]{2}){1,4}$',g$line_styles$line))) fail('Use a named line type or an even-length hexadecimal line pattern.')
 validate_thresholds(g$thresholds);validate_annotations(g$annotations)
 invisible(g)
}
edit_snapshot <- function(state) list(graph=state$graph,axes=state$axes)
commit_edit <- function(state,graph=NULL,axes=NULL,record=TRUE) {
 before <- edit_snapshot(state);after <- list(graph=graph %||% before$graph,axes=axes %||% before$axes)
 if(identical(before,after)) return(invisible(FALSE))
 if(record) {
  history <- state$undo %||% list();history[[length(history)+1L]] <- before
  state$undo <- tail(history,100L);state$redo <- list()
 }
 state$graph <- after$graph;state$axes <- after$axes
 invisible(TRUE)
}
travel_edit <- function(state,direction) {
 stack <- state[[direction]] %||% list();if(!length(stack)) return(invisible(FALSE))
 opposite <- if(direction=='undo') 'redo' else 'undo';other <- state[[opposite]] %||% list()
 other[[length(other)+1L]] <- edit_snapshot(state);target <- stack[[length(stack)]]
 state[[opposite]] <- other;state[[direction]] <- head(stack,-1)
 commit_edit(state,target$graph,target$axes,record=FALSE);state$restore <- state$restore+1L
 invisible(TRUE)
}
style_preset <- function(name,g=graph_defaults(),a=axis_settings()) {
 if(!name %in% c('Clean Scientific','Journal Black & White','Color Publication','Presentation')) fail('Unknown style preset.')
 g$theme <- 'classic';g$font <- 'sans';g$font_size <- 12;g$title_size <- 18;g$subtitle_size <- 12
 g$panel_background <- '#ffffff';g$plot_background <- '#ffffff';g$grid_color <- '#edf0f3'
 g$axis_lines <- TRUE;g$panel_border <- FALSE;g$point_size <- 3;g$line_width <- .8;g$error_line_width <- .7;g$alpha <- .85
 g$title_family <- '';g$subtitle_family <- '';g$title_color <- '#203844';g$subtitle_color <- '#536875'
 for(ax in c('x','y')) {a[[ax]]$title_family <- '';a[[ax]]$tick_family <- '';a[[ax]]$title_color <- '#203844';a[[ax]]$tick_color <- '#465b68'}
 g$palette <- 'okabe';g$colors <- '';g$legacy_group_colors <- graph_defaults()$legacy_group_colors;g$dataset_styles$color <- rep('',nrow(g$dataset_styles));g$legend_compact <- TRUE
 a$grid <- FALSE
 if(name=='Journal Black & White') {
  g$palette <- 'monochrome';g$line <- 'study';g$title_face <- 'plain';g$font <- 'serif';g$title_color <- '#222222';g$subtitle_color <- '#444444'
  for(ax in c('x','y')) {a[[ax]]$title_color <- '#222222';a[[ax]]$tick_color <- '#333333'}
  g$annotations$color <- rep('#222222',nrow(g$annotations))
 }
 if(name=='Color Publication') {g$palette <- 'viridis';g$theme <- 'bw';g$panel_border <- TRUE;a$grid <- TRUE}
 if(name=='Presentation') {g$font_size <- 17;g$title_size <- 25;g$subtitle_size <- 16;g$point_size <- 4;g$line_width <- 1.2;g$error_line_width <- 1;g$legend_size <- 14}
 list(graph=g,axes=a)
}
stable_study_color <- function(id,palette='okabe') {
 hash <- sum(utf8ToInt(id)*seq_along(utf8ToInt(id)))
 index <- suppressWarnings(as.integer(sub('^study_','',id)))
 if(is.na(index)) index <- hash
 base <- c('#0072B2','#E69F00','#009E73','#CC79A7','#56B4E9','#D55E00','#000000','#8B5A2B')
 if(palette=='monochrome') return('#222222')
 if(palette=='okabe' && index>=1 && index<=length(base)) return(base[index])
 if(palette %in% c('viridis','plasma')) return(scales::viridis_pal(option=if(palette=='plasma') 'C' else 'D')(256)[1+(index*53)%%256])
 if(palette %in% rownames(RColorBrewer::brewer.pal.info)) {
  n <- RColorBrewer::brewer.pal.info[palette,'maxcolors'];if(index<=n && index>=1) return(RColorBrewer::brewer.pal(n,palette)[index])
 }
 grDevices::hcl(h=(index*137.508)%%360,c=65,l=53)
}
study_style_labels <- function(d,g) {
 ids <- unique(d$dataset_id)
 if(is.factor(d$study)) {ordered <- d$dataset_id[match(levels(d$study),as.character(d$study))];ids <- unique(c(ordered[!is.na(ordered)],ids))}
 labels <- vapply(ids,function(id) if('study' %in% names(d)) as.character(d$study[match(id,d$dataset_id)]) else id,character(1))
 for(i in seq_along(ids)) {
  row <- match(ids[i],g$dataset_styles$dataset_id)
  if(!is.na(row) && nzchar(g$dataset_styles$label[row])) labels[i] <- g$dataset_styles$label[row]
 }
 duplicates <- duplicated(labels) | duplicated(labels,fromLast=TRUE)
 labels[duplicates] <- paste0(labels[duplicates],' [',ids[duplicates],']')
 stats::setNames(labels,ids)
}
graph_analysis_config <- function(g) {
 raw <- g$type %in% c('Scatter','Individual points','Boxplot','Violin')
 list(x_variable=g$x_variable,type=if(raw) if(g$type=='Scatter') 'Scatter' else 'Individual points' else 'Dose-response',
  error=if(raw) 'None' else switch(g$type,'Mean ± SD'='SD','Mean ± SEM'='SEM','Mean with CI'='CI',g$error))
}
preparation_fingerprint <- function(d) {
 fields <- c(names(empty_data()),grep('^xaux_',names(d),value=TRUE));out <- as.data.frame(d[fields],stringsAsFactors=FALSE)
 for(nm in names(out)) out[[nm]] <- if(nm %in% c('dose','value','mean','sd','n') || startsWith(nm,'xaux_')) as.numeric(out[[nm]]) else as.character(out[[nm]])
 rownames(out) <- NULL;digest::digest(out,algo='sha256')
}
color_group_key <- function(row,fields) {
 fields[fields=='study'] <- 'dataset_id';fields <- sort(unique(fields))
 as.character(jsonlite::toJSON(as.list(row[1,fields,drop=FALSE]),auto_unbox=TRUE,digits=17))
}
migrate_positional_colors <- function(g,d,legacy_order=FALSE) {
 if(!nzchar(g$colors)) return(g)
 z <- prepare_graph_data(d,g);groups <- if(legacy_order) sort(unique(as.character(z$.color))) else levels(z$.color)
 colors <- trimws(strsplit(g$colors,',',fixed=TRUE)[[1]])
 if(length(colors)!=length(groups)) fail('The legacy color count does not match the selected groups. Select its original studies/strata or enter study-ID colors.')
 tryCatch(grDevices::col2rgb(colors),error=function(e) fail('Invalid imported color.'))
 for(i in seq_along(groups)) {
  row <- z[match(groups[i],as.character(z$.color)),,drop=FALSE]
  if(identical(g$color,'study')) {
   index <- match(row$dataset_id,g$dataset_styles$dataset_id)
   if(is.na(index)) g$dataset_styles <- rbind(g$dataset_styles,data.frame(dataset_id=row$dataset_id,label='',color=colors[i])) else g$dataset_styles$color[index] <- colors[i]
  } else {
   key <- color_group_key(row,g$color);index <- match(key,g$legacy_group_colors$key)
   if(is.na(index)) g$legacy_group_colors <- rbind(g$legacy_group_colors,data.frame(key=key,color=colors[i])) else g$legacy_group_colors$color[index] <- colors[i]
  }
 }
 g$colors <- '';g
}
reset_known_settings <- function(value,defaults) {
 out <- value
 for(nm in names(defaults)) out[[nm]] <- if(is.list(defaults[[nm]]) && !is.data.frame(defaults[[nm]])) reset_known_settings(out[[nm]] %||% list(),defaults[[nm]]) else defaults[[nm]]
 out
}
style_group_value <- function(row,field) if(field=='study') as.character(row$dataset_id[1]) else as.character(row[[field]][1])
default_symbol <- function(field,value) {
 if(field=='sex' && tolower(value) %in% c('female','f')) return(16)
 if(field=='sex' && tolower(value) %in% c('male','m')) return(17)
 if(value %in% c('Unspecified','raw')) return(16)
 if(value=='summary') return(17)
 symbols <- c(16,17,15,18,3,7,8,0:14)
 index <- if(field=='study') suppressWarnings(as.integer(sub('^study_','',value))) else NA_integer_
 if(is.na(index)) index <- 1L+sum(utf8ToInt(value)*seq_along(utf8ToInt(value)))
 as.numeric(symbols[1L+(index-1L)%%length(symbols)])
}
default_line <- function(field,value) {
 if(value=='Unspecified') return('solid')
 if(field=='sex' && tolower(value) %in% c('female','f')) return('solid')
 if(field=='sex' && tolower(value) %in% c('male','m')) return('dashed')
 types <- c('solid','dashed','dotted','dotdash','longdash','twodash')
 index <- if(field=='study') suppressWarnings(as.integer(sub('^study_','',value))) else NA_integer_
 if(is.na(index)) index <- 1L+sum(utf8ToInt(value)*seq_along(utf8ToInt(value)))
 types[1L+(index-1L)%%length(types)]
}
resolve_shape_values <- function(z,g) {
 groups <- levels(z$.shape)
 if(nzchar(g$shapes)) {
  values <- parse_numbers(g$shapes);if(length(values)<length(groups)) fail('Not enough legacy shape codes for the displayed groups.')
  return(stats::setNames(values[seq_along(groups)],groups))
 }
 stats::setNames(vapply(groups,function(group) {
  row <- z[match(group,as.character(z$.shape)),,drop=FALSE];value <- style_group_value(row,g$shape)
  match <- which(g$symbol_styles$field==g$shape & g$symbol_styles$value==value)
  if(length(match) && !is.na(g$symbol_styles$symbol[match[1]])) g$symbol_styles$symbol[match[1]] else default_symbol(g$shape,value)
 },numeric(1)),groups)
}
resolve_line_values <- function(z,g) {
 groups <- levels(z$.linetype)
 if(nzchar(g$line_types)) {
  values <- trimws(strsplit(g$line_types,',',fixed=TRUE)[[1]]);if(length(values)<length(groups)) fail('Not enough legacy line types for the displayed groups.')
  return(stats::setNames(values[seq_along(groups)],groups))
 }
 stats::setNames(vapply(groups,function(group) {
  row <- z[match(group,as.character(z$.linetype)),,drop=FALSE];value <- style_group_value(row,g$line)
  match <- which(g$line_styles$field==g$line & g$line_styles$value==value)
  if(length(match) && nzchar(g$line_styles$line[match[1]])) g$line_styles$line[match[1]] else default_line(g$line,value)
 },character(1)),groups)
}
migrate_legacy_symbols <- function(g,d,universe=d) {
 for(kind in c('shape','line')) {
  key <- if(kind=='shape') 'shapes' else 'line_types';if(!nzchar(g[[key]])) next
  values <- if(kind=='shape') parse_numbers(g[[key]]) else trimws(strsplit(g[[key]],',',fixed=TRUE)[[1]])
  field <- g[[kind]];labels <- sort(unique(map_identity(universe,field)));source <- universe
  if(length(values)!=length(labels)) {labels <- sort(unique(map_identity(d,field)));source <- d}
  if(length(values)!=length(labels)) fail('Legacy symbol/line entries cannot be assigned unambiguously. Select the original groups or use the keyed style table.')
  for(i in seq_along(labels)) {
   row <- source[match(labels[i],map_identity(source,field)),,drop=FALSE];value <- style_group_value(row,field)
   table <- if(kind=='shape') 'symbol_styles' else 'line_styles';column <- if(kind=='shape') 'symbol' else 'line'
   match <- which(g[[table]]$field==field & g[[table]]$value==value)
   if(length(match)) g[[table]][match[1],column] <- values[i] else {
    entry <- data.frame(field=field,value=value,stringsAsFactors=FALSE);entry[[column]] <- values[i];g[[table]] <- rbind(g[[table]],entry)
   }
  }
  g[[key]] <- ''
 }
 g
}
