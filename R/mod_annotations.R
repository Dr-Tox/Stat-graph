annotation_row <- function() list(id='',type='asterisk',source='manual',label_mode='literal',dataset_id='',endpoint='',sex='Unspecified',generation='Unspecified',
 exposure='Unspecified',collection_time='Unspecified',subgroup='Unspecified',treatment='',treatment2='',dose=NA_real_,dose2=NA_real_,
 text='*',p=NA_real_,y=NA_real_,dx=0,dy=0,size=5,color='#203844',face='plain',visible=TRUE,line_width=.6,tip=.025,evidence='')
empty_annotations <- function() as.data.frame(annotation_row(),stringsAsFactors=FALSE)[FALSE,]
new_annotation <- function(...) {
 row <- annotation_row();v <- list(...);row[names(v)] <- v
 if(!nzchar(row$id)) row$id <- paste0('ann_',format(Sys.time(),'%Y%m%d%H%M%OS6'),sprintf('_%06d',sample.int(999999,1)))
 as.data.frame(row,stringsAsFactors=FALSE)
}
validate_thresholds <- function(d) {
 if(!is.data.frame(d) || !all(c('p','label') %in% names(d)) || !nrow(d) || any(!is.finite(d$p)) || any(d$p<=0 | d$p>=1) || anyDuplicated(d$p) || is.unsorted(d$p,strictly=TRUE))
  fail('Asterisk thresholds require unique increasing p cutoffs strictly between 0 and 1.')
 if(anyNA(d$label) || any(!nzchar(d$label))) fail('Each threshold needs a label.')
 invisible(d)
}
p_symbol <- function(p,thresholds) {
 validate_thresholds(thresholds)
 vapply(p,function(value) {if(!is.finite(value) || value<0 || value>1) fail('P-values must lie between 0 and 1.');hit <- which(value<thresholds$p);if(length(hit)) thresholds$label[hit[1]] else 'ns'},character(1))
}
validate_annotations <- function(d) {
 if(!is.data.frame(d) || !all(names(empty_annotations()) %in% names(d))) fail('Invalid annotation table schema.')
 if(!nrow(d)) return(invisible(d))
 if(anyDuplicated(d$id) || any(!nzchar(d$id))) fail('Annotation IDs must be nonempty and unique.')
 if(any(!d$type %in% c('asterisk','letter','text','bracket','pvalue')) || any(!d$source %in% c('manual','automatic'))) fail('Unsupported annotation type or source.')
 for(nm in c('dataset_id','endpoint','treatment',factors)) if(anyNA(d[[nm]]) || any(!nzchar(d[[nm]]))) fail(paste('Annotation anchors require an explicit',nm))
 for(nm in c('dx','dy','size','line_width','tip')) if(any(!is.finite(d[[nm]]))) fail(paste('Annotation',nm,'must be finite.'))
 if(any(d$size<=0 | d$line_width<=0 | d$tip<0)) fail('Annotation size/line width must be positive; bracket tip must be nonnegative.')
 if(any(!is.na(d$p) & (!is.finite(d$p) | d$p<0 | d$p>1))) fail('Annotation p-values must be between 0 and 1 or blank.')
 if(any(!is.na(d$y) & !is.finite(d$y))) fail('Annotation Y must be finite or automatic.')
 if(any(d$type=='bracket' & !nzchar(d$treatment2))) fail('A bracket needs two explicitly selected treatment groups.')
 for(color in d$color) tryCatch(grDevices::col2rgb(color),error=function(e) fail('Invalid annotation color.'))
 invisible(d)
}
scientific_fingerprint <- function(d) {
 cols <- names(empty_data());d <- as.data.frame(d[cols],stringsAsFactors=FALSE)
 for(nm in cols) d[[nm]] <- if(is.numeric(empty_data()[[nm]])) as.numeric(d[[nm]]) else as.character(d[[nm]])
 if(nrow(d)) d <- d[do.call(order,c(unname(as.list(d)),list(na.last=TRUE))),,drop=FALSE]
 rownames(d) <- NULL
 digest::digest(jsonlite::toJSON(d,dataframe='rows',na='null',digits=17),algo='sha256',serialize=FALSE)
}
analysis_context <- function(d,settings,results) {
 groups <- sort(unique(d$treatment));method <- unique(results$method)
 pairs <- if(method %in% c('Student t-test','Welch t-test','Mann-Whitney U')) matrix(groups,nrow=2)
  else if(method %in% c('Tukey','Dunn')) utils::combn(groups,2)
  else if(method=='Dunnett') rbind(setdiff(groups,settings$control),settings$control) else matrix(character(),nrow=2)
 list(engine='StatLab independent groups',version=1L,settings=settings,groups=groups,
   pairs=if(ncol(pairs)) data.frame(group1=pairs[1,],group2=pairs[2,]) else data.frame(group1=character(),group2=character()),
   data_hash=scientific_fingerprint(d),result_hash=digest::digest(jsonlite::toJSON(results,dataframe='rows',na='null',digits=17),algo='sha256',serialize=FALSE),
   results=results)
}
context_data <- function(d,ctx) {
 s <- ctx$settings;d <- d[d$dataset_id==s$dataset & d$endpoint==s$endpoint & d$treatment %in% unlist(ctx$groups),,drop=FALSE]
 for(nm in factors) if(length(s$strata[[nm]])) d <- d[d[[nm]]==s$strata[[nm]],,drop=FALSE]
 d
}
validate_analysis_context <- function(p) {
 ctx <- p$analysis_context
 if(is.null(ctx) || !identical(ctx$engine,'StatLab independent groups')) fail('No validated analysis context. Run an explicit statistical comparison first; 2.0 result tables need to be recalculated before automatic annotation.')
 ctx$groups <- unlist(ctx$groups);ctx$pairs <- normalize_table(ctx$pairs,data.frame(group1=character(),group2=character()))
 ctx$results <- if(is.data.frame(ctx$results)) ctx$results else restore_result_rows(ctx$results)
 if(!identical(scientific_fingerprint(context_data(p$data,ctx)),ctx$data_hash)) fail('Statistical results are stale: scientific observations changed. Re-run the analysis.')
 actual <- digest::digest(jsonlite::toJSON(p$results,dataframe='rows',na='null',digits=17),algo='sha256',serialize=FALSE)
 if(!identical(actual,ctx$result_hash)) fail('The current result table does not match the validated analysis. Re-run the analysis.')
 if(!nrow(ctx$pairs) || nrow(ctx$pairs)!=nrow(p$results)) fail('Omnibus results do not identify pairwise comparisons. Run a justified post-hoc method before adding comparison annotations.')
 if(any(!is.finite(p$results$p) | p$results$p<0 | p$results$p>1)) fail('The analysis does not provide valid comparison p-values.')
 ctx
}
compact_letters <- function(groups,pairs,p,alpha=.05) {
 if(length(groups)<2 || nrow(pairs)!=choose(length(groups),2)) fail('Compact letters require a complete all-pairs comparison family; control-only comparisons are insufficient.')
 expected <- apply(utils::combn(sort(groups),2),2,paste,collapse='\r')
 actual <- vapply(seq_len(nrow(pairs)),function(i) paste(sort(as.character(unlist(pairs[i,]))),collapse='\r'),character(1))
 if(anyDuplicated(actual) || !setequal(actual,expected)) fail('Compact letters require exactly one result for every pair of groups.')
 columns <- list(groups)
 for(i in which(p<alpha)) {
  pair <- as.character(unlist(pairs[i,]));nextcols <- list()
  for(col in columns) {
   if(all(pair %in% col)) {nextcols[[length(nextcols)+1]] <- setdiff(col,pair[1]);nextcols[[length(nextcols)+1]] <- setdiff(col,pair[2])}
   else nextcols[[length(nextcols)+1]] <- col
  }
  keys <- vapply(nextcols,function(x) paste(sort(x),collapse='\r'),character(1));nextcols <- nextcols[!duplicated(keys)]
  keep <- !vapply(seq_along(nextcols),function(j) any(vapply(setdiff(seq_along(nextcols),j),function(k) all(nextcols[[j]] %in% nextcols[[k]]),logical(1))),logical(1))
  columns <- nextcols[keep]
 }
 symbols <- vapply(seq_along(columns),function(i) if(i<=26) letters[i] else if(i<=52) LETTERS[i-26] else paste0('L',i),character(1))
 stats::setNames(vapply(groups,function(group) paste(symbols[vapply(columns,function(col) group %in% col,logical(1))],collapse=''),character(1)),groups)
}
automatic_annotations <- function(p,kind='asterisk',thresholds=graph_defaults()$thresholds) {
 ctx <- validate_analysis_context(p);res <- p$results;s <- ctx$settings
 if(!kind %in% c('asterisk','pvalue','letter')) fail('Unsupported automatic annotation type.')
 if(kind=='letter' && (!unique(res$method) %in% c('Tukey','Dunn','Student t-test','Welch t-test') || (unique(res$method)=='Dunn' && any(res$correction=='none'))))
  fail('Automatic letters require justified all-pairs comparisons. Dunnett control-only or unadjusted multi-group comparisons cannot define a complete compact-letter display.')
 evidence <- as.character(jsonlite::toJSON(ctx,auto_unbox=TRUE,dataframe='rows',na='null',digits=17))
 anchors <- c(list(source='automatic',label_mode=if(kind=='letter') 'cld' else kind,dataset_id=s$dataset,endpoint=s$endpoint,evidence=evidence),s$strata)
 if(kind=='letter') {
  labels <- compact_letters(ctx$groups,ctx$pairs,res$p,unique(res$alpha))
  return(dplyr::bind_rows(lapply(names(labels),function(group) do.call(new_annotation,c(anchors,list(type='letter',treatment=group,text=labels[[group]]))))))
 }
 dplyr::bind_rows(lapply(seq_len(nrow(res)),function(i) do.call(new_annotation,c(anchors,list(type='bracket',treatment=ctx$pairs$group1[i],treatment2=ctx$pairs$group2[i],
   p=res$p[i],text=if(kind=='asterisk') p_symbol(res$p[i],thresholds) else format_p_label(res$p[i],graph_defaults()))))))
}
resolve_annotations <- function(z,d,g,a) {
 anns <- effective_annotations(g);validate_annotations(anns);resolved <- list();notes <- character();methods <- character()
 if(!nrow(anns)) return(list(data=data.frame(),notes=notes))
 lanes <- new.env(parent=emptyenv());evidence_cache <- new.env(parent=emptyenv())
 tr <- axis_transform(a$y);vals <- c(z$y,z$ymax);vals <- vals[is.finite(vals) & (a$y$scale!='log10' | vals>0)]
 span <- diff(range(tr$transform(vals)));if(!is.finite(span) || span==0) span <- .2
 for(i in seq_len(nrow(anns))) {
  row <- anns[i,,drop=FALSE];if(!isTRUE(row$visible)) next
  reject <- function(reason) paste0('Annotation ',row$id,': ',reason)
  if(row$source=='automatic') {
   ctx <- tryCatch(jsonlite::fromJSON(row$evidence,simplifyVector=FALSE),error=function(e) NULL)
   cache_key <- digest::digest(row$evidence,algo='sha256')
   valid <- if(exists(cache_key,evidence_cache,inherits=FALSE)) get(cache_key,evidence_cache) else {
    value <- !is.null(ctx) && identical(ctx$engine,'StatLab independent groups') && identical(scientific_fingerprint(context_data(d,ctx)),ctx$data_hash)
    assign(cache_key,value,evidence_cache);value
   }
   if(valid) methods <- c(methods,paste0(ctx$settings$method,'; alpha=',ctx$settings$alpha %||% ctx$results[[1]]$alpha,' (',paste(unique(vapply(ctx$results,function(r) r$correction %||% 'none',character(1))),collapse=', '),')'))
   if(!valid) {
    notes <- c(notes,reject('automatic evidence is stale or the complete analyzed family is not selected; annotation hidden.'));next
   }
  }
  scope <- z$dataset_id==row$dataset_id & z$endpoint==row$endpoint
  for(nm in factors) scope <- scope & z[[nm]]==row[[nm]]
  find_group <- function(treatment,dose) {x <- z[scope & z$treatment==treatment,,drop=FALSE];if(!is.na(dose)) x <- x[x$dose==dose,,drop=FALSE];x}
  left <- find_group(row$treatment,row$dose)
  right <- if(row$type=='bracket') find_group(row$treatment2,row$dose2) else left
  if(!nrow(left) || !nrow(right)) {notes <- c(notes,reject('anchor is absent from the current selection or logarithmic domain; not drawn.'));next}
  if(length(unique(left$.x))!=1 || length(unique(right$.x))!=1 || length(unique(left$dose))!=1 || length(unique(right$dose))!=1) {notes <- c(notes,reject('anchor maps to multiple doses or X coordinates; specify dose or use a grouped dose/time plot.'));next}
  x1 <- unique(left$.x);x2 <- unique(right$.x)
  display1 <- unique(left$.display_x %||% left$.x)+row$dx;display2 <- unique(right$.display_x %||% right$.x)+row$dx
  anchors_y <- c(left$y,left$ymax,right$y,right$ymax);anchors_y <- anchors_y[is.finite(anchors_y)]
  lane_key <- paste(c(x1,if(length(g$facet)) as.character(unlist(left[1,g$facet,drop=FALSE]))),collapse=':')
  lane <- if(exists(lane_key,lanes,inherits=FALSE)) get(lane_key,lanes)+1L else 0L;assign(lane_key,lane,lanes)
  top <- if(is.na(row$y)) tr$inverse(max(tr$transform(anchors_y))+span*(.09+.06*lane)) else row$y
  top <- top+row$dy
  if(!is.finite(top) || (a$y$scale=='log10' && top<=0)) {notes <- c(notes,reject('Y position is outside the selected transform domain; not drawn.'));next}
  if(a$x$scale=='log10' && ((x1>0 && display1<=0) || (x2>0 && display2<=0) || x1<0 || x2<0 || (a$zero!='separate' && (x1==0 || x2==0)))) {notes <- c(notes,reject('X offset is outside the log10 domain; not drawn.'));next}
  row$x1 <- x1;row$x2 <- x2;row$display_x1 <- display1;row$display_x2 <- display2;row$original_dose1 <- unique(left$dose);row$original_dose2 <- unique(right$dose);row$plot_y <- top;row$tip_y <- tr$inverse(tr$transform(top)-span*row$tip)
  row$study <- as.character(left$study[1]);row$.tooltip <- paste(if(row$source=='manual') 'User-provided annotation' else 'Validated statistical annotation',row$dataset_id,row$treatment,row$text,sep=' · ')
  resolved[[length(resolved)+1]] <- row
 }
 out <- if(length(resolved)) as.data.frame(dplyr::bind_rows(resolved)) else data.frame()
 if(nrow(out) && any(out$source=='manual')) notes <- c(notes,'Manual significance labels are user-provided; StatLab has not inferred their significance.')
 if(nrow(out) && any(out$source=='automatic')) {
  notes <- c(notes,paste0('Automatic annotations: ',paste(unique(methods),collapse='; '),'. Existing adjusted p-values are retained.'))
  if(g$show_thresholds && any(out$source=='automatic' & out$label_mode=='asterisk')) notes <- c(notes,paste('Asterisks:',paste(paste0('p < ',g$thresholds$p,' ',g$thresholds$label),collapse='; '),'; otherwise ns.'))
  if(any(out$type=='letter')) notes <- c(notes,'Shared compact letters indicate failure to reject, not equivalence.')
 }
 list(data=out,notes=notes)
}
annotation_layers <- function(p,resolved,range,ax,g,control_panel=FALSE,first_panel=FALSE) {
 if(!nrow(resolved)) return(p)
 for(i in seq_len(nrow(resolved))) {
  row <- resolved[i,,drop=FALSE]
  for(nm in g$facet) if(nm %in% names(row)) row[[nm]] <- factor(row[[nm]],levels=levels(p$data[[nm]]))
  if(!row$type=='bracket') {
   if(row$x1<range[1] || row$x1>range[2] || (control_panel && row$x1!=0) || (!control_panel && ax$scale=='log10' && row$x1<=0)) next
   row$.label_x <- row$display_x1
  } else {
   lo <- min(row$display_x1,row$display_x2);hi <- max(row$display_x1,row$display_x2)
   cross_control <- ax$scale=='log10' && min(row$x1,row$x2)==0 && max(row$x1,row$x2)>0
   if(control_panel) {if(min(row$x1,row$x2)!=0) next;from <- if(row$x1==0) row$display_x1 else row$display_x2;to <- range[2]}
   else {from <- if(cross_control) range[1] else max(lo,range[1]);to <- min(hi,range[2]);if(from>to || (ax$scale=='log10' && to<=0)) next}
   row$.from <- from;row$.to <- to
   p <- p+ggplot2::geom_segment(data=row,ggplot2::aes(x=.from,xend=.to,y=plot_y,yend=plot_y),inherit.aes=FALSE,color=row$color,linewidth=row$line_width,show.legend=FALSE)
   endpoints <- if(control_panel) if(row$x1==0) row$display_x1 else row$display_x2 else c(row$display_x1,row$display_x2)[c(row$x1,row$x2)>=range[1] & c(row$x1,row$x2)<=range[2]]
   if(!control_panel && ax$scale=='log10') endpoints <- endpoints[endpoints>0]
   for(x in endpoints) p <- p+ggplot2::geom_segment(data=row,ggplot2::aes(y=tip_y,yend=plot_y),x=x,xend=x,inherit.aes=FALSE,color=row$color,linewidth=row$line_width,show.legend=FALSE)
   # Segmented brackets show explicit continuation across omitted regions.
   continued <- if(control_panel) TRUE else lo<range[1] || hi>range[2]
   if(continued) p <- p+ggplot2::geom_text(data=row,ggplot2::aes(y=plot_y),x=if(control_panel || hi>range[2]) to else from,label='//',vjust=-.3,inherit.aes=FALSE,size=3,color=row$color,show.legend=FALSE)
   label_here <- if(cross_control) control_panel else row$x1>=range[1] && row$x1<=range[2]
   if(!label_here) next
   tr <- axis_transform(if(control_panel) utils::modifyList(ax,list(scale='linear',reverse=FALSE)) else ax)
   row$.label_x <- tr$inverse(mean(tr$transform(c(from,to))))
  }
  for(nm in g$facet) if(nm %in% names(row)) row[[nm]] <- factor(row[[nm]],levels=levels(p$data[[nm]]))
  p <- p+ggplot2::geom_text(data=row,ggplot2::aes(x=.label_x,y=plot_y,label=text),inherit.aes=FALSE,
    family=g$font,fontface=row$face,size=row$size,color=row$color,vjust=-.45,show.legend=FALSE)
 }
 p
}

format_p_label <- function(p,g) {
 if(!is.finite(p) || p<0 || p>1) fail('P-value labels require p in [0, 1].')
 if(p==0) return('p ≈ 0 (numerical)')
 if(g$p_format=='threshold') {
  hit <- which(p<g$thresholds$p)
  if(length(hit)) return(paste0('p < ',format(g$thresholds$p[hit[1]],scientific=FALSE,trim=TRUE)))
 }
 paste0('p = ',format(p,digits=g$p_digits,scientific=g$p_format=='scientific',trim=TRUE))
}
effective_annotations <- function(g) {
 d <- g$annotations
 if(!nrow(d)) return(d)
 for(i in seq_len(nrow(d))) if(d$source[i]=='automatic') {
  if(d$label_mode[i]=='asterisk') d$text[i] <- p_symbol(d$p[i],g$thresholds)
  if(d$label_mode[i]=='pvalue') d$text[i] <- format_p_label(d$p[i],g)
 }
 d
}
