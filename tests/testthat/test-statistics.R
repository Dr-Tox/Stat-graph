stat_data <- function() {
 set.seed(71);tab <- data.frame(dose=rep(0:2,each=12),value=c(rnorm(12,4,1),rnorm(12,5,1.3),rnorm(12,6,1.1)),treatment=rep(c('Control','Low','High'),each=12))
 d <- map_table(tab,list(dose='dose',value='value',treatment='treatment'),endpoint='ALT',dose_unit='mg/kg/day',response_unit='U/L');d$dataset_id <- 'study_1';d
}
testthat::test_that('raw and sufficient-statistic t-tests match R independently', {
 d <- stat_data();d <- d[d$dose<2,];s <- summarise_groups(d);s$value <- NA_real_;s$mode <- 'summary'
 for(method in c('Welch t-test','Student t-test')) {
  ref <- stats::t.test(value~treatment,data=d,var.equal=method=='Student t-test')
  for(x in list(d,s)) {
   res <- run_analysis(x,method);testthat::expect_equal(res$p,ref$p.value,tolerance=1e-12)
   testthat::expect_equal(res$df,unname(ref$parameter),tolerance=1e-12)
   testthat::expect_equal(c(res$lower,res$upper),as.numeric(ref$conf.int),tolerance=1e-12)
  }
 }
})
testthat::test_that('ANOVA, Tukey and Dunnett match independent fitted-model references', {
 d <- stat_data();d$treatment <- factor(d$treatment,levels=sort(unique(d$treatment)))
 fit <- stats::aov(value~treatment,data=d);s <- summarise_groups(d);s$value <- NA_real_;s$mode <- 'summary'
 for(x in list(d,s)) {
  res <- run_analysis(x,'One-way ANOVA');testthat::expect_equal(res$p,summary(fit)[[1]][['Pr(>F)']][1],tolerance=1e-12)
  tuk <- run_analysis(x,'Tukey');ref <- stats::TukeyHSD(fit)$treatment
  testthat::expect_equal(sort(tuk$p),sort(unname(ref[,'p adj'])),tolerance=1e-10)
  dun <- run_analysis(x,'Dunnett',control='Control')
  refd <- multcomp::glht(fit,linfct=multcomp::mcp(treatment='Dunnett'));set.seed(2026);refp <- summary(refd)$test$pvalues
  testthat::expect_equal(sort(dun$p),sort(as.numeric(refp)),tolerance=.001)
  testthat::expect_true(all(dun$lower<dun$estimate & dun$upper>dun$estimate))
 }
})
testthat::test_that('rank methods match R and unsupported summary inference is rejected', {
 d <- stat_data();two <- d[d$dose<2,]
 testthat::expect_equal(run_analysis(two,'Mann-Whitney U')$p,stats::wilcox.test(value~treatment,data=two,exact=FALSE)$p.value)
 testthat::expect_equal(run_analysis(d,'Kruskal-Wallis')$p,stats::kruskal.test(value~treatment,data=d)$p.value)
 out <- run_analysis(d,'Dunn');testthat::expect_equal(nrow(out),3L);testthat::expect_true(all(out$p>=0 & out$p<=1))
 s <- summarise_groups(d);s$value <- NA_real_;s$mode <- 'summary'
 for(method in c('Dunn','Kruskal-Wallis','Mann-Whitney U')) testthat::expect_error(run_analysis(s,method),'raw observations')
 testthat::expect_error(run_analysis(d,'Welch t-test'),'exactly two')
 testthat::expect_error(run_analysis(d,'Dunnett',control='missing'),'valid control')
 d$dataset_id[1] <- 'unrelated';testthat::expect_error(run_analysis(d,'One-way ANOVA'),'never pooled')
 d$dataset_id <- 'study_1';d$sex[1] <- 'Female';testthat::expect_error(run_analysis(d,'One-way ANOVA'),'stratum')
 testthat::expect_error(run_analysis(stat_data(),'fabricate'),'Unsupported')
})
