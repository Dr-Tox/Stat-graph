graph_defaults <- function() c(list(x_variable='dose',type='Dose-response',title='Independent studies · common endpoint',subtitle='',
 color=c('study'),shape='sex',line='collection_time',facet=character(),point_size=3,alpha=.8,line_width=.7,
 font='sans',font_size=12,legend='right',theme='bw',palette='okabe',colors='',shapes='',line_types='',
 panel_spacing=1,error='SD',error_width=.06,error_color='',annotation='',show_lines=TRUE),graph_extensions())
graph_types <- function(d) {
 types <- c('Scatter','Dose-response','Line','Bar','Mean ± SD','Mean ± SEM','Mean with CI')
 if(any(d$mode=='summary' & is.na(d$sd))) types <- setdiff(types,c('Mean ± SD','Mean ± SEM','Mean with CI'))
 if(any(d$mode=='summary' & is.na(d$n))) types <- setdiff(types,c('Mean ± SEM','Mean with CI'))
 if(all(d$mode=='raw')) types <- c(types,'Boxplot','Violin','Individual points')
 types
}
palette_values <- function(n,palette='okabe') {
 if(!n) return(character())
 if(palette=='okabe') return(grDevices::colorRampPalette(c('#0072B2','#E69F00','#009E73','#CC79A7','#56B4E9','#D55E00','#000000'))(n))
 if(palette %in% c('viridis','plasma')) return(scales::viridis_pal(option=if(palette=='plasma') 'C' else 'D')(n))
 if(palette=='default') return(scales::hue_pal()(n))
 maxc <- RColorBrewer::brewer.pal.info[palette,'maxcolors']
 if(is.na(maxc)) fail('Unknown color palette.')
 grDevices::colorRampPalette(RColorBrewer::brewer.pal(max(3,min(n,maxc)),palette))(n)
}
map_identity <- function(d,vars) {
 vars <- intersect(vars,names(d))
 if(!length(vars)) return(rep('All',nrow(d)))
 apply(d[vars],1,paste,collapse=' · ')
}
prepare_numeric_data <- function(d,g) {
 if(startsWith(g$x_variable,'xaux_') && !g$type %in% c('Scatter','Individual points')) fail('Generic source X variables support individual/scatter plots. Use mapped dose, exposure or collection time for group response series.')
 if(!g$type %in% graph_types(d)) fail('Distribution plots require raw observations for every selected dataset.')
 if(g$type=='Scatter' || (g$type %in% c('Individual points','Boxplot','Violin') && all(d$mode=='raw'))) {
   out <- d; out$y <- ifelse(out$mode=='raw',out$value,out$mean); out$ymin <- out$ymax <- NA_real_
 } else {
   out <- summarise_groups(d); out$study <- d$study[match(out$dataset_id,d$dataset_id)]
   out$y <- out$mean
   error <- switch(g$type,'Mean ± SD'='SD','Mean ± SEM'='SEM','Mean with CI'='CI',g$error)
   if(error %in% c('SEM','CI') && any(out$mode=='summary' & (is.na(out$sd) | is.na(out$n)))) fail('SEM and confidence intervals require SD and n for all summary groups.')
   err <- switch(error,'None'=rep(NA_real_,nrow(out)),'SEM'=out$se,'CI'=(out$upper-out$lower)/2,out$sd)
   out$ymin <- out$mean-err; out$ymax <- out$mean+err
   if(g$type=='Scatter') out$ymin <- out$ymax <- NA_real_
 }
 out$.x <- if(g$x_variable=='dose') out$dose else suppressWarnings(as.numeric(as.character(out[[g$x_variable]])))
 if(length(out$.x)!=nrow(out) || any(!is.finite(out$.x))) fail('Selected X variable must contain finite numeric values. Map numeric exposure or collection times in Data Manager.')
 out$.tooltip <- paste0('Study: ',out$dataset_id,'<br>Endpoint: ',out$endpoint,'<br>Dose: ',format(out$dose,digits=12),' ',out$dose_unit,'<br>Response: ',format(out$y,digits=12),' ',out$response_unit,'<br>Input mode: ',out$mode,'<br>Sex: ',out$sex,'<br>Subgroup: ',out$subgroup,'<br>Generation: ',out$generation,'<br>Exposure: ',out$exposure,'<br>Collection time: ',out$collection_time)

 out
}
prepare_graph_data <- function(d,g) apply_graph_encodings(prepare_numeric_data(d,g),d,g)
build_figure <- function(d,a=axis_settings(),g=graph_defaults(),prepared=NULL) {
 g <- normalize_settings(g,graph_defaults());a <- normalize_settings(a,axis_settings())
 validate_axis(a$x); validate_axis(a$y)
 if(startsWith(g$x_variable,'xaux_') && !g$type %in% c('Scatter','Individual points')) fail('Generic source X variables support individual/scatter plots. Use mapped dose, exposure or collection time for group response series.')
 if(!g$type %in% graph_types(d)) fail('This graph type is unsupported by the selected input modes or uncertainty information.')
 z <- apply_graph_encodings(if(is.null(prepared)) prepare_numeric_data(d,g) else prepared,d,g); notes <- paste0('Axes: X = ',a$x$scale,if(a$x$scale=='pseudo_log') paste0(' (sigma=',a$x$sigma,')') else '',', Y = ',a$y$scale,if(a$y$scale=='pseudo_log') paste0(' (sigma=',a$y$sigma,')') else '')
 if(!g$type %in% c('Scatter','Individual points','Boxplot','Violin')) notes <- c(notes,paste('Error bars:',switch(g$type,'Mean ± SD'='SD','Mean ± SEM'='SEM','Mean with CI'='95% t confidence interval',g$error)))
 if(nzchar(g$annotation)) notes <- c(notes,'Legacy corner text is user-provided and retains its 2.0 positioning.')
 if(any(d$mode=='summary' & (is.na(d$n) | is.na(d$sd)))) notes <- c(notes,'Some supplied summaries lack n or SD: unsupported interval types and inference are unavailable.')
 if(any(is.na(z$ymin)) && !g$type %in% c('Scatter','Individual points','Boxplot','Violin') && g$error!='None') notes <- c(notes,'Some group uncertainty is unavailable: those groups have points without error bars.')
 if(g$type=='Mean with CI' || g$error=='CI') notes <- c(notes,'Confidence intervals are 95% per-group t intervals, not simultaneous intervals.')
 total <- nrow(z)
 if(a$x$scale=='log10' && any(z$.x<0)) notes <- c(notes,sprintf('%d negative X observations excluded from log10.',sum(z$.x<0)))
 separate <- a$x$scale=='log10' && a$zero=='separate' && any(z$.x==0)
 control <- if(separate) z[z$.x==0,,drop=FALSE] else z[FALSE,,drop=FALSE]
 if(a$x$scale=='log10') {
   if(any(z$.x==0)) notes <- c(notes,if(separate) 'Zero controls are displayed in a separate linear panel at their true value 0.' else sprintf('%d zero-control points explicitly excluded from log10; choose separate controls or pseudo-log to include them.',sum(z$.x==0)))
   z <- z[z$.x>0,,drop=FALSE]
 }
 if(a$y$scale=='log10') {
   bad <- z$y<=0; badc <- control$y<=0
   if(any(bad) || any(badc)) notes <- c(notes,sprintf('%d nonpositive Y points excluded from log10.',sum(bad)+sum(badc)))
   z <- z[!bad,,drop=FALSE]; control <- control[!badc,,drop=FALSE]
   for(which in c('z','control')) {
     obj <- get(which); nbad <- sum(obj$ymin<=0,na.rm=TRUE)
     if(nbad) notes <- c(notes,sprintf('%d error bars cross zero: entire bars omitted on log10, never clamped.',nbad))
     obj$ymax[!is.na(obj$ymin) & obj$ymin<=0] <- NA_real_; obj$ymin[!is.na(obj$ymin) & obj$ymin<=0] <- NA_real_
     assign(which,obj)
   }
 }
 if(!nrow(z) && !nrow(control)) fail('No points remain on the chosen logarithmic scales.')
 intervals <- a$intervals
 validate_intervals(intervals,if(nrow(intervals)) z else NULL,log=a$x$scale=='log10')
 if(nrow(intervals)) {
   visible <- Reduce(`|`,lapply(seq_len(nrow(intervals)),function(i) z$.x>=intervals$start[i] & z$.x<=intervals$end[i]))
   if(any(!visible)) notes <- c(notes,sprintf('%d plotted points outside requested intervals; omitted regions are marked by axis breaks.',sum(!visible)))
 } else {
   intervals <- data.frame(start=a$x$min,end=a$x$max,width=1)
 }
 xbounds <- c(a$x$min,a$x$max); ybounds <- c(a$y$min,a$y$max)
 if(any(!is.na(xbounds))) notes <- c(notes,'User X limits crop the viewport; original coordinates are retained.')
 if(any(!is.na(ybounds))) notes <- c(notes,'User Y limits crop the viewport; original coordinates are retained.')
 allz <- add_display_x(dplyr::bind_rows(z,control),a,g)
 if(g$type %in% c('Bar','Boxplot','Violin')) notes <- c(notes,'Horizontal grouping offsets are visual only; original doses remain in tooltips and annotation anchors.')
 annotation_info <- resolve_annotations(allz,d,g,a);resolved <- annotation_info$data;notes <- c(notes,annotation_info$notes)
 if(nrow(resolved)) {
  visible_x <- function(x) {
   if(a$x$scale=='log10' && a$zero=='separate' && x==0) return(TRUE)
   if(!is.na(a$x$min) && x<a$x$min || !is.na(a$x$max) && x>a$x$max) return(FALSE)
   if(!nrow(a$intervals)) return(TRUE)
   any(x>=a$intervals$start & x<=a$intervals$end)
  }
  keep <- vapply(seq_len(nrow(resolved)),function(i) visible_x(resolved$x1[i]) && (resolved$type[i]!='bracket' || visible_x(resolved$x2[i])),logical(1))
  if(any(!keep)) notes <- c(notes,paste(sum(!keep),'annotations hidden because their anchors are outside the visible X ranges. Brackets require both anchors to be visible.'))
  resolved <- resolved[keep,,drop=FALSE]
  if(nrow(resolved) && any((!is.na(a$y$min) & resolved$plot_y<a$y$min) | (!is.na(a$y$max) & resolved$plot_y>a$y$max))) notes <- c(notes,'Some annotations lie outside fixed Y limits; adjust their Y positions or the viewport.')
 }
 if(nrow(resolved) && any(resolved$type=='bracket' & resolved$x1!=resolved$x2) && nrow(a$intervals)>1) notes <- c(notes,'Bracket segments with // continue across omitted X regions; endpoints retain their original group anchors.')
 yr <- if(a$independent_y) c(NA_real_,NA_real_) else range(c(allz$y,allz$ymin,allz$ymax,if(nrow(resolved)) resolved$plot_y),na.rm=TRUE)
 if(all(is.finite(yr)) && diff(yr)==0) {
  yr <- if(a$y$scale=='log10') 10^(log10(yr)+c(-.05,.05)) else yr+c(-.5,.5)
 }
 yr[!is.na(ybounds)] <- ybounds[!is.na(ybounds)]
 colors <- resolve_graph_colors(allz,g)
 panels <- list(); widths <- numeric()
 make_panel <- function(dat,xrange,control_panel=FALSE,index=1L) {
   ax <- a$x
   if(control_panel) {ax$scale <- 'linear'; ax$major <- 0; ax$labels <- 'Control (0)'; ax$minor <- numeric(); ax$interval <- NA_real_; ax$reverse <- FALSE}
   xlab <- if(nzchar(ax$title)) ax$title else switch(g$x_variable,dose='Dose',exposure='Exposure duration',collection_time='Collection time',sub('^xaux_','',g$x_variable))
   xu <- if(nzchar(ax$unit)) ax$unit else if(g$x_variable=='dose') unique(d$dose_unit) else 'mapped units'
   ylab <- if(nzchar(a$y$title)) a$y$title else unique(d$endpoint)
   yu <- if(nzchar(a$y$unit)) a$y$unit else unique(d$response_unit)
   for(nm in g$facet) dat[[nm]] <- factor(as.character(dat[[nm]]),levels=unique(as.character(allz[[nm]])))
   dat <- add_display_x(dat,a,g)
   p <- ggplot2::ggplot(dat,ggplot2::aes(x=.x,y=y,color=.color,shape=.shape,group=.series,text=.tooltip,linetype=.linetype,alpha=.alpha))
   dodge <- ggplot2::position_dodge(width=g$error_width)
   if(g$type=='Boxplot') p <- p+ggplot2::geom_boxplot(ggplot2::aes(x=.display_x,width=.body_width,group=interaction(.series,.x)),orientation='x',position='identity')
   else if(g$type=='Violin') {
     cells <- table(interaction(dat$.series,dat$.x,drop=TRUE))
     if(any(cells<2)) fail('Violin density requires at least two raw observations per displayed group.')
     p <- p+ggplot2::geom_violin(ggplot2::aes(x=.display_x,width=.body_width,group=interaction(.series,.x)),orientation='x',position='identity')
   } else if(g$type=='Bar') p <- p+ggplot2::geom_col(ggplot2::aes(x=.display_x,width=.body_width,fill=.color),position='identity')
   else p <- p+ggplot2::geom_point(size=g$point_size,position=if(g$type=='Individual points') ggplot2::position_jitter(width=g$error_width,height=0,seed=1) else ggplot2::position_identity())
   if(g$show_lines && g$type %in% c('Dose-response','Line')) {
    line_data <- dat[ave(seq_len(nrow(dat)),dat$.series,FUN=length)>1,,drop=FALSE]
    if(nrow(line_data)) p <- p+ggplot2::geom_line(data=line_data,linewidth=g$line_width)
   }
   if(!g$type %in% c('Boxplot','Violin','Individual points','Scatter')) {
     eb <- ggplot2::geom_errorbar(ggplot2::aes(x=if(g$type=='Bar') .display_x else .x,ymin=ymin,ymax=ymax),width=g$error_width,linewidth=g$error_line_width,position=ggplot2::position_identity(),na.rm=TRUE)
     if(nzchar(g$error_color)) eb <- ggplot2::geom_errorbar(ggplot2::aes(x=if(g$type=='Bar') .display_x else .x,ymin=ymin,ymax=ymax),color=g$error_color,width=g$error_width,linewidth=g$error_line_width,position=ggplot2::position_identity(),na.rm=TRUE)
     p <- p+eb
   }
   if(g$type %in% c('Boxplot','Violin','Bar')) p <- p+ggplot2::scale_fill_manual(values=colors,drop=FALSE,guide='none')
   p <- p+ggplot2::scale_alpha_identity()+ggplot2::scale_color_manual(values=colors,drop=FALSE,name=paste(g$color,collapse=' + '))+
     ggplot2::scale_shape_manual(values=resolve_shape_values(allz,g),drop=FALSE,name=g$shape)+
     ggplot2::scale_linetype_manual(values=resolve_line_values(allz,g),drop=FALSE,name=g$line)
   if(length(g$facet)) p <- p+ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(g$facet)),scales='fixed',drop=FALSE,ncol=g$facet_columns)
   theme <- switch(g$theme,minimal=ggplot2::theme_minimal,classic=ggplot2::theme_classic,bw=ggplot2::theme_bw,dark=ggplot2::theme_dark)
   p <- p+theme(base_size=g$font_size,base_family=g$font)+
     ggplot2::theme(legend.position=if(startsWith(g$legend,'inside_')) 'inside' else g$legend,panel.spacing=grid::unit(g$panel_spacing,'lines'),
       axis.text.x=ggplot2::element_text(size=a$x$text_size),axis.text.y=ggplot2::element_text(size=a$y$text_size))
   if(startsWith(g$legend,'inside_')) p <- p+ggplot2::theme(legend.position.inside=switch(g$legend,inside_tr=c(.88,.88),inside_br=c(.88,.12),inside_tl=c(.12,.88),inside_bl=c(.12,.12)))
   p <- p+graph_panel_theme(g,a)
   if(!a$grid) p <- p+ggplot2::theme(panel.grid=ggplot2::element_blank())
   p <- p+axis_scale(ax,'x')+axis_scale(a$y,'y',annotation_padding=nrow(resolved)>0)+
     ggplot2::coord_cartesian(xlim=xrange,ylim=yr,expand=TRUE)+
     ggplot2::labs(x=axis_label(a$x,xlab,xu),y=axis_label(a$y,ylab,yu),
       subtitle=if(a$independent_y) 'INDEPENDENT Y RANGE' else NULL,
       caption=if(index>1) paste(a$break_symbol,'X range',paste(xrange,collapse='–')) else if(control_panel) 'Separate zero-control panel · linear X' else NULL)
   p <- p+ggplot2::theme(plot.caption=ggplot2::element_text(color=a$break_color,size=a$break_size,hjust=.5))
   if(nzchar(g$annotation)) p <- p+ggplot2::annotate('text',x=Inf,y=Inf,label=g$annotation,hjust=1.1,vjust=1.5)
   annotation_range <- xrange
   if(anyNA(annotation_range)) {r <- range(dat$.x);annotation_range[is.na(annotation_range)] <- r[is.na(annotation_range)]}
   p <- annotation_layers(p,resolved,annotation_range,a$x,g,control_panel,index==1L)
   p
 }
 if(nrow(control)) {panels[[1]] <- make_panel(control,c(-.5,.5),TRUE); widths <- .35}
 for(i in seq_len(nrow(intervals))) {
   row <- intervals[i,]; dat <- z
   if(nrow(a$intervals)) dat <- z[z$.x>=row$start & z$.x<=row$end,,drop=FALSE]
   if(!nrow(dat)) next
   panels[[length(panels)+1]] <- make_panel(dat,c(row$start,row$end),index=length(panels)+1L)
   widths <- c(widths,row$width)
 }
 # Use one complete static legend; interactive traces retain their own groups.
 static_panels <- lapply(seq_along(panels),function(i) if(i==1 && g$legend_static) panels[[i]] else panels[[i]]+ggplot2::theme(legend.position='none'))
 static <- patchwork::wrap_plots(static_panels,nrow=1,widths=widths,guides='collect')+
   patchwork::plot_annotation(title=if(g$title_visible) graph_title_text(d,g) else NULL,subtitle=if(g$subtitle_visible) g$subtitle else NULL,theme=graph_title_theme(g),
     caption=paste(c('Exploratory visualization · study identities retained',if(any(d$mode=='summary')) 'Includes supplied summary statistics; no individual observations reconstructed.',notes),collapse='\n'))
 list(static=static,panels=panels,widths=widths,notes=notes,coordinates=allz,original_count=total,title=graph_title_text(d,g),subtitle=if(g$subtitle_visible) g$subtitle else '',independent_y=a$independent_y,graph=g,axes=a,annotations=resolved)
}
interactive_figure <- function(fig) {
 g <- fig$graph;a <- fig$axes
 plots <- lapply(fig$panels,function(p) plotly::ggplotly(p,tooltip='text'))
 widget <- if(length(plots)==1) plots[[1]] else plotly::subplot(plots,nrows=1,widths=fig$widths/sum(fig$widths),shareY=FALSE,titleX=TRUE,titleY=TRUE,margin=.035)
 widget <- plotly::plotly_build(widget);seen <- character();labels <- study_style_labels(fig$coordinates,g)
 for(i in seq_along(widget$x$data)) {
  t <- widget$x$data[[i]];name <- t$name %||% '';text <- as.character(t$text %||% '')
  id <- if(length(text) && grepl('Study: ',text[1],fixed=TRUE)) sub('.*Study: ([^<]+).*','\\1',text[1]) else ''
  key <- name
  if(g$legend_compact && nzchar(id)) {
   sex <- sub('.*<br>Sex: ([^<]+).*','\\1',text[1]);subgroup <- sub('.*<br>Subgroup: ([^<]+).*','\\1',text[1])
   short <- paste(c(sex,if(subgroup!='Unspecified') subgroup),collapse=' · ')
   if(length(unique(fig$coordinates$collection_time))>1) short <- paste(short,sub('.*<br>Collection time: ([^<]+).*','\\1',text[1]),sep=' · ')
   key <- paste(id,name,sep=':');t$name <- short;t$legendgroup <- id
   t$legendgrouptitle <- list(text=unname(labels[id]))
  } else t$legendgroup <- name
  if(key %in% seen) t$showlegend <- FALSE else if(isTRUE(t$showlegend)) seen <- c(seen,key)
  widget$x$data[[i]] <- t
 }
 annotations <- widget$x$layout$annotations %||% list()
 if(length(plots)>1) {
  boundaries <- head(cumsum(fig$widths/sum(fig$widths)),-1)
  annotations <- c(annotations,lapply(boundaries,function(x) list(x=x,y=-.09,xref='paper',yref='paper',text=as.character(htmltools::htmlEscape(a$break_symbol)),showarrow=FALSE,font=list(size=a$break_size,color=a$break_color))))
  # Subplot drops ggplotly facet axis titles; restore one shared title per axis.
  for(axis in c('x','y')) {
   label <- fig$panels[[1]]$labels[[axis]];settings <- a[[axis]]
   if(length(label) && nzchar(label)) annotations <- c(annotations,list(list(x=if(axis=='x') .5 else -.08,y=if(axis=='x') -.135 else .5,xref='paper',yref='paper',xanchor=if(axis=='x') 'center' else 'center',yanchor='middle',textangle=if(axis=='x') 0 else -90,
    text=gsub('\n','<br>',html_face(label,settings$title_face),fixed=TRUE),showarrow=FALSE,font=list(size=settings$title_size,color=settings$title_color,family=font_family(settings$title_family,g$font)))))
  }
 }
 if(fig$independent_y) annotations <- c(annotations,list(list(x=.5,y=1.04,xref='paper',yref='paper',text='INDEPENDENT Y RANGES',showarrow=FALSE)))
 if(nzchar(fig$subtitle)) annotations <- c(annotations,list(list(x=g$subtitle_hjust,y=1.10,xref='paper',yref='paper',xanchor=if(g$subtitle_hjust<.5) 'left' else if(g$subtitle_hjust>.5) 'right' else 'center',text=html_face(fig$subtitle,g$subtitle_face),showarrow=FALSE,font=list(size=g$subtitle_size,color=g$subtitle_color,family=font_family(g$subtitle_family,g$font)))))
 note <- fig$notes[grepl('^Manual significance|^Asterisks:|^Shared compact',fig$notes)]
 if(length(note)) annotations <- c(annotations,list(list(x=0,y=-.22,xref='paper',yref='paper',xanchor='left',yanchor='top',text=paste(htmltools::htmlEscape(note),collapse='<br>'),showarrow=FALSE,font=list(size=max(9,g$font_size*.7),color='#536875'))))
 plotly::layout(widget,title=list(text=html_face(fig$title,g$title_face),x=g$title_hjust,xanchor=if(g$title_hjust<.5) 'left' else if(g$title_hjust>.5) 'right' else 'center',font=list(size=g$title_size,color=g$title_color,family=font_family(g$title_family,g$font))),
  annotations=annotations,showlegend=g$legend!='none' && !g$legend_collapsed,
  legend=list(groupclick='togglegroup',tracegroupgap=3,font=list(size=g$legend_size),itemsizing='constant'),
  margin=list(l=if(length(plots)>1) 95+a$y$title_margin else 80,t=75+g$title_spacing+g$subtitle_spacing+if(nzchar(fig$subtitle)) g$subtitle_size else 0,b=100+length(note)*16))
}
