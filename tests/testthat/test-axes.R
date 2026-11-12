testthat::test_that('linear, log10, pseudo-log and reverse transformations preserve exact source coordinates', {
 p <- example_project();d <- comparison_data(p,p$selected,'Relative liver weight')
 for(scale in c('linear','log10','pseudo_log')) for(reverse in c(FALSE,TRUE)) {
  a <- axis_settings();a$x$scale <- scale;a$x$reverse <- reverse;a$zero <- 'separate'
  f <- build_figure(d,a);testthat::expect_equal(sort(unique(f$coordinates$dose)),sort(unique(d$dose)))
  tr <- axis_transform(a$x);v <- if(scale=='log10') c(.001,.1,1,10000) else c(-100,0,.001,1,10000)
  testthat::expect_equal(tr$inverse(tr$transform(v)),v,tolerance=1e-10)
  testthat::expect_silent(ggplot2::ggplot_build(f$panels[[1]]))
 }
 a <- axis_settings();a$x$scale <- 'log10';f <- build_figure(d,a)
 testthat::expect_true(any(grepl('explicitly excluded',f$notes)));testthat::expect_true(all(f$coordinates$dose>0))
 testthat::expect_true(any(d$dose==0))
})
testthat::test_that('arbitrary visible intervals create real panels and reject invalid boundaries', {
 p <- example_project();d <- comparison_data(p,p$selected,'Relative liver weight')
 intervals <- data.frame(start=c(0,.01,1,100,10000),end=c(.005,.5,20,2000,10001),width=c(1,2,1,2,1))
 for(n in 1:5) {
  a <- axis_settings();a$intervals <- intervals[seq_len(n),]
  f <- build_figure(d,a);testthat::expect_length(f$panels,n);testthat::expect_equal(f$widths,a$intervals$width)
  for(i in seq_len(n)) {z <- f$panels[[i]]$data;testthat::expect_true(all(z$dose>=intervals$start[i] & z$dose<=intervals$end[i]))}
 }
 testthat::expect_error(validate_intervals(data.frame(start=c(0,1),end=c(2,3),width=1)),'non-overlapping')
 testthat::expect_error(validate_intervals(data.frame(start=1,end=0,width=1)),'start < end')
 a <- axis_settings();a$intervals <- data.frame(start=500,end=600,width=1)
 testthat::expect_error(build_figure(d,a),'no observations')
 a$x$scale <- 'log10';a$intervals <- intervals[1,,drop=FALSE]
 testthat::expect_error(build_figure(d,a),'positive')
})
testthat::test_that('custom ticks, limits and exports retain panel configuration', {
 p <- example_project();d <- comparison_data(p,p$selected,'Relative liver weight')
 a <- axis_settings();a$intervals <- parse_intervals('0,1,1\n10,100,2\n1000,10000,3');a$x$major <- c(0,.1,1,10,100,1000,10000)
 a$x$labels <- as.character(a$x$major);a$y$min <- 2;a$y$max <- 8;a$y$major <- c(2,4,6,8)
 f <- build_figure(d,a);testthat::expect_equal(f$panels[[2]]$coordinates$limits$x,c(10,100))
 testthat::expect_equal(f$panels[[1]]$coordinates$limits$y,c(2,8))
 for(fmt in c('png','svg','pdf')) {path <- tempfile(fileext=paste0('.',fmt));export_figure(f,path,fmt);testthat::expect_gt(file.info(path)$size,1000)}
 testthat::expect_error(parse_numbers('0,nope'),'finite')
 a$x$labels <- 'bad';testthat::expect_error(validate_axis(a$x),'one custom label')
})
testthat::test_that('time axes retain independent dose series and independent Y panels are labeled', {
 p <- example_project();d <- comparison_data(p,p$selected,'Relative liver weight');d$exposure <- '28'
 g <- graph_defaults();g$x_variable <- 'exposure';g$type <- 'Scatter'
 f <- build_figure(d,g=g);testthat::expect_true(all(f$coordinates$.x==28));testthat::expect_equal(f$coordinates$dose,d$dose)
 for(series in unique(f$coordinates$.series)) testthat::expect_length(unique(f$coordinates$dose[f$coordinates$.series==series]),1)
 a <- axis_settings();a$independent_y <- TRUE;a$intervals <- parse_intervals('0,1,1\n10,10000,1')
 f <- build_figure(d,a);testthat::expect_equal(f$panels[[1]]$labels$subtitle,'INDEPENDENT Y RANGE')
 testthat::expect_true(all(is.na(f$panels[[1]]$coordinates$limits$y)))
})
testthat::test_that('interactive split panels support single-point series and deduplicate legends', {
 p <- example_project();d <- comparison_data(p,c('study_1','study_3'),'Relative liver weight',list(sex='Male'))
 a <- axis_settings();a$x$scale <- 'pseudo_log';a$intervals <- parse_intervals('0,1,1\n10,100,1\n1000,10000,2')
 f <- build_figure(d,a);w <- interactive_figure(f)
 testthat::expect_true(all(c('xaxis','xaxis2','xaxis3') %in% names(w$x$layout)))
 visible <- vapply(w$x$data,function(t) isTRUE(t$showlegend),logical(1))
 testthat::expect_equal(sum(visible),2L)
 testthat::expect_equal(w$x$layout$xaxis2$tickvals,axis_transform(a$x)$transform(as.numeric(w$x$layout$xaxis2$ticktext)))
})
testthat::test_that('split facets retain aligned study/sex cells when subgroups or doses are missing', {
 p <- example_project();d <- comparison_data(p,p$selected,'Relative liver weight')
 d <- d[!(d$dataset_id=='study_3' & d$sex=='Male'),]
 a <- axis_settings();a$intervals <- parse_intervals('0,1,1\n10,10000,1');g <- graph_defaults();g$facet <- c('study','sex')
 f <- build_figure(d,a,g)
 layouts <- lapply(f$panels,function(p) ggplot2::ggplot_build(p)$layout$layout)
 testthat::expect_equal(layouts[[1]][c('ROW','COL','study','sex')],layouts[[2]][c('ROW','COL','study','sex')])
 testthat::expect_silent(interactive_figure(f))
 a$x$min <- Inf;testthat::expect_error(validate_axis(a$x),'finite numbers')
})
testthat::test_that('axis module accepts Shiny logical NA for empty numeric inputs and applies real intervals', {
 state <- shiny::reactiveValues(axes=axis_settings(),restore=0L)
 shiny::testServer(axes_server,args=list(state=state),{
  session$setInputs(x_min=NA,x_max=NA,y_min=NA,y_max=NA,x_interval=NA,y_interval=NA,
    x_scale='pseudo_log',y_scale='linear',x_major='',x_minor='',x_labels='',y_major='',y_minor='',y_labels='',
    intervals='0,1,1\n10,100,1\n1000,10000,2',zero='exclude',independent_y=FALSE,grid=TRUE,apply=1)
  testthat::expect_equal(state$axes$x$scale,'pseudo_log')
  testthat::expect_equal(nrow(state$axes$intervals),3L)
 })
})
