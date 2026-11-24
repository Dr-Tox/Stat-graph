# Synthetic static export verification for the 2.1.1 correctness fixes.
for(f in list.files('R',full.names=TRUE,pattern='\\.R$')) source(f)
out <- 'artifacts/review-fixes-2.1.1';dir.create(out,recursive=TRUE,showWarnings=FALSE)
p <- example_project(2);d <- comparison_data(p,p$selected,'Relative liver weight')
x <- d[d$dataset_id=='study_1' & d$sex=='Female' & d$dose==0,,drop=FALSE]
y <- x;x$exposure <- '1';y$exposure <- '2';y$value <- y$value+100
box <- graph_defaults();box$type <- 'Boxplot';box$x_variable <- 'exposure';box$line <- 'sex';box$title <- 'Synthetic groups: separate exposure times'
bar <- graph_defaults();bar$type <- 'Bar';bar$error_color <- '#cc0000';bar$title <- 'Synthetic groups: aligned colored SD bars'
flat <- x;flat$value <- .01;a <- axis_settings();a$y$scale <- 'log10'
g <- graph_defaults();g$title <- 'Synthetic constant positive response: Log10'
figures <- list(exposure_boxplot=build_figure(rbind(x,y),g=box),colored_error_bars=build_figure(d,g=bar),constant_log_response=build_figure(flat,a,g))
for(name in names(figures)) for(ext in c('png','svg','pdf')) {
 file <- file.path(out,paste0(name,'.',ext))
 suppressWarnings(ggplot2::ggsave(file,figures[[name]]$static,width=10,height=6,dpi=150))
 stopifnot(file.info(file)$size>1000)
 cat('PASS',basename(file),'rendered\n')
}
