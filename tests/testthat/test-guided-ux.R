testthat::test_that('direct study toggles retain observations, order and independent visibility', {
 for(count in c(1,2,5,14,25,35)) {
  p <- example_project(count);before <- p$data;g <- graph_defaults();v <- list(hidden=character())
  x <- apply_study_toggle(p,v,g,'study_1','Active',FALSE)
  testthat::expect_false('study_1' %in% x$project$selected)
  x <- apply_study_toggle(x$project,x$view,x$graph,'study_1','Visible',TRUE)
  testthat::expect_true('study_1' %in% x$project$selected)
  testthat::expect_false('study_1' %in% x$view$hidden)
  x <- apply_study_toggle(x$project,x$view,x$graph,'study_1','Visible',FALSE)
  testthat::expect_true('study_1' %in% x$project$selected)
  testthat::expect_true('study_1' %in% x$view$hidden)
  x <- apply_study_toggle(x$project,x$view,x$graph,'study_1','Highlight',TRUE)
  testthat::expect_true('study_1' %in% x$graph$highlight)
  testthat::expect_identical(x$project$data,before)
 }
 testthat::expect_error(apply_study_toggle(p,v,g,'unknown','Active',TRUE),'Invalid')
 testthat::expect_error(apply_study_toggle(p,v,g,'study_1','Delete',TRUE),'Invalid')
})
testthat::test_that('provenance distinguishes imported files, examples and unknown saved sources', {
 p <- example_project(1);meta <- p$datasets[[1]]
 testthat::expect_match(study_origin(meta),'Example data')
 meta$metadata$source <- 'my_actual_study.csv';testthat::expect_equal(study_origin(meta),'Imported: my_actual_study.csv')
 meta$metadata <- list();testthat::expect_match(study_origin(meta),'not recorded')
})
testthat::test_that('downloadable templates map into valid raw and summary observations', {
 for(mode in c('raw','summary')) {
  tab <- import_template(mode);mapping <- as.list(stats::setNames(names(tab),names(tab)))
  d <- map_table(tab,mapping,mode,'Endpoint','mg/kg/day','%')
  testthat::expect_silent(validate_data(d));testthat::expect_equal(unique(d$mode),mode)
  labels <- treatment_choices(d);testthat::expect_setequal(unname(labels),unique(d$treatment));testthat::expect_true(all(grepl('n = ',names(labels))))
 }
})
testthat::test_that('comparison server recovers after clearing and reselecting studies', {
 state <- shiny::reactiveValues(project=example_project(4),graph=graph_defaults(),axes=axis_settings(),view=list(endpoint='Relative liver weight',filters=list()),restore=0L,undo=list(),redo=list())
 shiny::testServer(compare_server,args=list(state=state),{
  session$setInputs(endpoint='Relative liver weight');session$flushReact()
  session$setInputs(none=1);session$flushReact()
  testthat::expect_length(state$project$selected,0L)
  session$setInputs(study_toggle=list(id='study_1',field='Active',checked=TRUE,nonce=1));session$flushReact()
  session$setInputs(study_toggle=list(id='study_3',field='Active',checked=TRUE,nonce=2));session$flushReact()
  testthat::expect_equal(state$project$selected,c('study_1','study_3'))
  testthat::expect_setequal(unique(data()$dataset_id),c('study_1','study_3'))
  session$setInputs(study_toggle=list(id='study_1',field='Visible',checked=FALSE,nonce=3));session$flushReact()
  testthat::expect_equal(unique(data()$dataset_id),'study_3')
  session$setInputs(filter_sex='Male');session$flushReact();testthat::expect_equal(unique(data()$sex),'Male');testthat::expect_equal(state$view$filters$sex,'Male')
  view <- state$view;view$filters <- list(sex='Female');state$view <- view;state$restore <- state$restore+1L;session$flushReact()
  testthat::expect_equal(unique(data()$sex),'Female')
  session$setInputs(filter_sex='Female');session$flushReact()
  session$setInputs(filter_sex='Male');session$flushReact();testthat::expect_equal(unique(data()$sex),'Male')
 })
})
