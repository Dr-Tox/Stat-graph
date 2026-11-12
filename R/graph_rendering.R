axis_label <- function(a,title,unit) {
 if(!a$title_visible) return(NULL)
 label <- if(nzchar(a$title)) a$title else title
 unit <- if(nzchar(a$unit)) a$unit else unit
 if(a$unit_position=='hidden' || !nzchar(unit)) return(label)
 paste0(label,if(a$unit_position=='separate') '\n' else ' ','(',unit,')')
}
font_family <- function(specific,base) if(nzchar(specific)) specific else base
graph_title_text <- function(d,g) {
 if(!g$title_visible) return('')
 if(g$title_auto) paste(unique(d$endpoint),'·',length(unique(d$dataset_id)),'independent studies') else g$title
}
graph_title_theme <- function(g) ggplot2::theme(
 plot.title=ggplot2::element_text(family=font_family(g$title_family,g$font),size=g$title_size,face=g$title_face,color=g$title_color,hjust=g$title_hjust,margin=ggplot2::margin(b=g$title_spacing)),
 plot.subtitle=ggplot2::element_text(family=font_family(g$subtitle_family,g$font),size=g$subtitle_size,face=g$subtitle_face,color=g$subtitle_color,hjust=g$subtitle_hjust,margin=ggplot2::margin(b=g$subtitle_spacing)),
 plot.caption=ggplot2::element_text(family=g$font,size=max(7,g$font_size*.65),hjust=0),
 plot.background=ggplot2::element_rect(fill=if(nzchar(g$plot_background)) g$plot_background else 'white',color=NA))
graph_panel_theme <- function(g,a) {
 t <- ggplot2::theme(
 axis.title.x=ggplot2::element_text(size=a$x$title_size,color=a$x$title_color,face=a$x$title_face,family=font_family(a$x$title_family,g$font),margin=ggplot2::margin(t=a$x$title_margin)),
 axis.title.y=ggplot2::element_text(size=a$y$title_size,color=a$y$title_color,face=a$y$title_face,family=font_family(a$y$title_family,g$font),margin=ggplot2::margin(r=a$y$title_margin)),
 axis.text.x=ggplot2::element_text(size=a$x$text_size,color=a$x$tick_color,face=a$x$tick_face,family=font_family(a$x$tick_family,g$font),angle=a$x$tick_angle),
 axis.text.y=ggplot2::element_text(size=a$y$text_size,color=a$y$tick_color,face=a$y$tick_face,family=font_family(a$y$tick_family,g$font),angle=a$y$tick_angle),
 axis.ticks.length.x=grid::unit(a$x$tick_length,'pt'),axis.ticks.length.y=grid::unit(a$y$tick_length,'pt'),
 axis.line=if(g$axis_lines) ggplot2::element_line(color=g$axis_line_color,linewidth=g$axis_line_width) else ggplot2::element_blank(),
 panel.border=if(g$panel_border) ggplot2::element_rect(color=g$border_color,fill=NA,linewidth=g$axis_line_width) else ggplot2::element_blank(),
 panel.grid=if(a$grid) ggplot2::element_line(color=g$grid_color) else ggplot2::element_blank(),
 legend.text=ggplot2::element_text(size=g$legend_size),legend.title=ggplot2::element_text(size=g$legend_size),legend.key.size=grid::unit(g$legend_key_size,'pt'),
 plot.margin=ggplot2::margin(g$margin_top,g$margin_right,g$margin_bottom,g$margin_left))
 if(nzchar(g$panel_background)) t <- t+ggplot2::theme(panel.background=ggplot2::element_rect(fill=g$panel_background,color=NA))
 if(nzchar(g$plot_background)) t <- t+ggplot2::theme(plot.background=ggplot2::element_rect(fill=g$plot_background,color=NA))
 t
}
apply_graph_encodings <- function(out,d,g) {
 labels <- study_style_labels(d,g);out$study <- factor(unname(labels[out$dataset_id]),levels=unname(labels))
 order <- order(match(out$dataset_id,names(labels)))
 color <- map_identity(out,g$color);line <- map_identity(out,g$line)
 out$.color <- factor(color,levels=if('study' %in% g$color) unique(color[order]) else sort(unique(color)))
 out$.shape <- factor(map_identity(out,g$shape));out$.linetype <- factor(line,levels=if('study' %in% g$line) unique(line[order]) else sort(unique(line)))
 series_fields <- c('dataset_id','endpoint',setdiff(factors,g$x_variable),if(g$x_variable!='dose') 'dose')
 series <- paste(map_identity(out,series_fields),out$.color,out$.linetype,sep=' · ')
 out$.series <- factor(series,levels=unique(series[order]))
 out$.alpha <- g$alpha*if(length(intersect(g$highlight,out$dataset_id))) ifelse(out$dataset_id %in% g$highlight,1,g$dim_alpha) else 1
 out
}
resolve_graph_colors <- function(z,g) {
 groups <- levels(z$.color)
 values <- vapply(groups,function(group) {
  row <- z[match(group,as.character(z$.color)),,drop=FALSE];id <- row$dataset_id
  i <- match(id,g$dataset_styles$dataset_id)
  if('study' %in% g$color && !is.na(i) && nzchar(g$dataset_styles$color[i])) return(g$dataset_styles$color[i])
  legacy <- match(color_group_key(row,g$color),g$legacy_group_colors$key)
  if(!is.na(legacy)) return(g$legacy_group_colors$color[legacy])
  if('study' %in% g$color) {
   if(!is.na(i) && nzchar(g$dataset_styles$color[i])) return(g$dataset_styles$color[i])
   if(length(g$color)==1) return(stable_study_color(id,g$palette))
   return(stable_study_color(paste(id,map_identity(row,setdiff(g$color,'study')),sep=':'),g$palette))
  }
  stable_study_color(paste0('group:',group),g$palette)
 },character(1))
 if(nzchar(g$colors)) {
  custom <- trimws(strsplit(g$colors,',',fixed=TRUE)[[1]])
  if(length(custom)!=length(values)) fail('Legacy positional colors no longer match the displayed legend. Use stable study-ID colors in Data & Groups.')
  tryCatch(grDevices::col2rgb(custom),error=function(e) fail('Invalid custom color.'));values[] <- custom
 }
 values
}
html_face <- function(text,face) {
 text <- as.character(htmltools::htmlEscape(text))
 if(grepl('bold',face,fixed=TRUE)) text <- paste0('<b>',text,'</b>')
 if(grepl('italic',face,fixed=TRUE)) text <- paste0('<i>',text,'</i>')
 text
}

panel_group_width <- function(z,a,g,control=FALSE) {
 if(!is.na(g$dodge_width)) return(g$dodge_width)
 tr <- if(control) scales::identity_trans() else axis_transform(a$x)
 values <- sort(unique(tr$transform(z$.x)));spacing <- diff(values);spacing <- spacing[is.finite(spacing) & spacing>0]
 if(length(spacing)) .8*min(spacing) else .4
}
add_display_x <- function(z,a,g) {
 z$.display_x <- z$.x;z$.body_width <- rep(.3,nrow(z))
 if(!g$type %in% c('Bar','Boxplot','Violin') || !nrow(z)) return(z)
 panels <- list()
 if(a$x$scale=='log10' && any(z$.x==0)) panels[[1]] <- which(z$.x==0)
 eligible <- if(a$x$scale=='log10') which(z$.x>0) else seq_len(nrow(z))
 if(nrow(a$intervals)) for(i in seq_len(nrow(a$intervals))) panels[[length(panels)+1]] <- intersect(eligible,which(z$.x>=a$intervals$start[i] & z$.x<=a$intervals$end[i]))
 else panels[[length(panels)+1]] <- eligible
 for(panel in panels) {
  if(!length(panel)) next
  control <- a$x$scale=='log10' && all(z$.x[panel]==0)
  width <- panel_group_width(z[panel,,drop=FALSE],a,g,control)
  tr <- if(control) scales::identity_trans() else axis_transform(a$x)
  keys <- paste(map_identity(z[panel,,drop=FALSE],g$facet),z$.x[panel],sep=':')
  for(rows in split(panel,keys)) {
   groups <- as.integer(z$.series[rows]);levels <- sort(unique(groups));rank <- match(groups,levels)
   z$.display_x[rows] <- tr$inverse(tr$transform(z$.x[rows])+width*((rank-.5)/length(levels)-.5))
   z$.body_width[rows] <- width*.9/length(levels)
  }
 }
 z
}
