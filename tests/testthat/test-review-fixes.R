review_time_data <- function(variable) {
 p <- example_project(1);d <- comparison_data(p,p$selected,'Relative liver weight')
 d <- d[d$sex=='Female' & d$dose==0,,drop=FALSE]
 later <- d;d[[variable]] <- '1';later[[variable]] <- '2';later$value <- later$value+100
 rbind(d,later)
}
testthat::test_that('time-axis distributions retain each experimental time group', {
 for(variable in c('exposure','collection_time')) for(type in c('Boxplot','Violin')) {
  d <- review_time_data(variable);before <- d;g <- graph_defaults();g$type <- type;g$x_variable <- variable;g$line <- 'sex'
  fig <- build_figure(d,g=g)
  built <- suppressWarnings(ggplot2::ggplot_build(fig$panels[[1]]))$data[[1]]
  testthat::expect_equal(length(unique(built$group)),2L,info=paste(variable,type))
  if(type=='Boxplot') testthat::expect_equal(sort(built$middle),as.numeric(tapply(d$value,d[[variable]],median)))
  testthat::expect_identical(d,before)
 }
 for(variable in c('exposure','collection_time')) {
  d <- review_time_data(variable);d <- d[c(1,9:16),];g <- graph_defaults();g$type <- 'Violin';g$x_variable <- variable;g$line <- 'sex'
  testthat::expect_error(build_figure(d,g=g),'at least two raw observations')
 }
})
testthat::test_that('error-bar custom colors preserve grouped-bar positions and uncertainty', {
 p <- example_project(2);d <- comparison_data(p,p$selected,'Relative liver weight');before <- d
 for(scale in c('linear','log10','pseudo_log')) {
  g <- graph_defaults();g$type <- 'Bar';g$facet <- 'sex';a <- axis_settings();a$x$scale <- scale;a$zero <- 'separate'
  original <- build_figure(d,a,g);g$error_color <- '#cc0000';colored <- build_figure(d,a,g)
  for(i in seq_along(original$panels)) {
   old <- ggplot2::ggplot_build(original$panels[[i]])$data;new <- ggplot2::ggplot_build(colored$panels[[i]])$data
   testthat::expect_equal(new[[1]]$x,new[[2]]$x)
   for(nm in c('x','xmin','xmax','ymin','ymax')) testthat::expect_equal(new[[2]][[nm]],old[[2]][[nm]])
   testthat::expect_true(all(new[[2]]$colour=='#cc0000'))
  }
 }
 testthat::expect_identical(d,before)
})
testthat::test_that('constant positive log responses receive valid scale-aware automatic limits', {
 for(value in c(.01,1,1000)) for(reverse in c(FALSE,TRUE)) {
  p <- example_project(1);d <- comparison_data(p,p$selected,'Relative liver weight');d$value <- value;before <- d
  a <- axis_settings();a$y$scale <- 'log10';a$y$reverse <- reverse
  fig <- build_figure(d,a);built <- NULL
  testthat::expect_warning(built <- ggplot2::ggplot_build(fig$panels[[1]]),NA)
  limits <- fig$panels[[1]]$coordinates$limits$y
  testthat::expect_true(all(is.finite(limits) & limits>0))
  testthat::expect_true(limits[1]<value && limits[2]>value)
  testthat::expect_lt(diff(log10(limits)),1)
  testthat::expect_true(all(is.finite(built$layout$panel_params[[1]]$y.range)))
  testthat::expect_identical(d,before)
 }
})
