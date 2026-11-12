for(f in list.files('R',full.names=TRUE,pattern='\\.R$')) source(f)
rows <- list()
for(count in c(1,2,5,14,25,35)) {
 p <- example_project(count);d <- comparison_data(p,p$selected,'Relative liver weight');g <- graph_defaults();a <- axis_settings()
 a$x$scale <- 'pseudo_log'
 memory <- tempfile();Rprofmem(memory)
 prep_time <- system.time(prepared <- prepare_numeric_data(d,graph_analysis_config(g)))['elapsed']
 construct <- system.time(fig <- build_figure(d,a,g,prepared))['elapsed']
 render <- system.time(widget <- interactive_figure(fig))['elapsed']
 cosmetic <- system.time({for(i in 1:3) {g$title <- paste('Style edit',i);g$title_size <- 18+i;fig <- build_figure(d,a,g,prepared)}})['elapsed']
 Rprofmem(NULL);alloc <- suppressWarnings(as.numeric(sub(' .*','',readLines(memory))));unlink(memory)
 rows[[length(rows)+1]] <- data.frame(studies=count,observations=nrow(d),preparation_seconds=unname(prep_time),figure_seconds=unname(construct),
   interactive_seconds=unname(render),three_cached_style_seconds=unname(cosmetic),prepared_MiB=as.numeric(object.size(prepared))/1024^2,allocated_MiB=sum(alloc,na.rm=TRUE)/1024^2)
}
out <- dplyr::bind_rows(rows);utils::write.csv(out,'artifacts/performance-editor-21.csv',row.names=FALSE);print(out)
