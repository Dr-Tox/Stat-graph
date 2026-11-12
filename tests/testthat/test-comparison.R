testthat::test_that('1, 2, 5, 14 and 25 studies import and arbitrary subsets retain identities', {
 for(n in c(1,2,5,14,25)) {
  p <- example_project(n); testthat::expect_length(p$datasets,n)
  ids <- names(p$datasets)[seq(1,n,by=2)]
  d <- comparison_data(p,ids,'Relative liver weight')
  testthat::expect_setequal(unique(d$dataset_id),ids)
  testthat::expect_equal(nrow(d),64*length(ids))
  f <- build_figure(d)
  for(series in unique(f$coordinates$.series)) testthat::expect_length(unique(f$coordinates$dataset_id[f$coordinates$.series==series]),1)
 }
})
testthat::test_that('common endpoint and subgroup filtering never invents absent data', {
 p <- example_project();p$data <- p$data[!(p$data$dataset_id=='study_3' & p$data$sex=='Male'),]
 d <- comparison_data(p,p$selected,'Relative liver weight',list(sex='Male'))
 testthat::expect_false('study_3' %in% d$dataset_id)
 testthat::expect_true(all(d$sex=='Male'))
 g <- graph_defaults();g$facet <- c('study','sex');f <- build_figure(d,g=g)
 testthat::expect_silent(ggplot2::ggplot_build(f$panels[[1]]))
 p$data$endpoint[p$data$dataset_id=='study_4'] <- 'Serum ALT'
 testthat::expect_false('study_4' %in% comparison_data(p,p$selected,'Relative liver weight')$dataset_id)
 testthat::expect_error(comparison_data(p,p$selected,'missing'),'No observations')
})
testthat::test_that('units must match and explicit compatible conversions are exact', {
 testthat::expect_equal(unit_factor('mg/kg/day','ug/kg/day'),1000)
 testthat::expect_error(unit_factor('g','U/L'),'Cannot convert')
 p <- example_project();p$data$response_unit[p$data$dataset_id=='study_1'] <- 'g'
 testthat::expect_error(comparison_data(p,p$selected,'Relative liver weight'),'Incompatible units')
})
testthat::test_that('editing and deletion update canonical comparisons', {
 p <- example_project();old <- comparison_data(p,p$selected,'Relative liver weight')$value[1]
 p$data$value[1] <- old+10
 testthat::expect_equal(comparison_data(p,p$selected,'Relative liver weight')$value[1],old+10)
 p <- remove_dataset(p,'study_2');testthat::expect_false('study_2' %in% p$data$dataset_id)
})
