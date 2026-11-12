editor_fixture <- function(n=4L) {
 p <- example_project(n);p$settings <- list(graph=graph_defaults(),axes=axis_settings(),view=list(endpoint='Relative liver weight',filters=list()),statistics=list())
 p
}
analysis_fixture <- function(method='Welch t-test',groups=c('Control','Dose 10000')) {
 p <- editor_fixture();d <- p$data[p$data$dataset_id=='study_4' & p$data$sex=='Male' & p$data$treatment %in% groups,,drop=FALSE]
 s <- list(dataset='study_4',endpoint='Relative liver weight',method=method,groups=groups,control='Control',alpha=.05,adjust='holm',strata=stats::setNames(as.list(rep('Unspecified',length(factors))),factors));s$strata$sex <- 'Male'
 p$results <- run_analysis(d,method,control='Control');p$analysis_context <- analysis_context(d,s,p$results);p$settings$statistics <- s;p
}
testthat::test_that('study colors are stable with 1, 2, 5, 14, 25 and 35 datasets and arbitrary selections', {
 p <- editor_fixture(35);g <- graph_defaults();d <- comparison_data(p,p$selected,'Relative liver weight');all <- resolve_graph_colors(prepare_graph_data(d,g),g)
 for(n in c(1,2,5,14,25,35)) {
  ids <- names(p$datasets)[seq_len(n)];small <- comparison_data(p,ids,'Relative liver weight');colors <- resolve_graph_colors(prepare_graph_data(small,g),g)
  testthat::expect_equal(colors,all[names(colors)])
 }
 g$dataset_styles <- data.frame(dataset_id='study_2',label='Renamed study',color='#aabbcc')
 z <- prepare_graph_data(d,g);colors <- resolve_graph_colors(z,g)
 testthat::expect_equal(unname(colors[unique(as.character(z$.color[z$dataset_id=='study_2']))]),'#aabbcc')
 testthat::expect_true(all(grepl('Renamed study',as.character(z$study[z$dataset_id=='study_2']))))
 g$highlight <- c('study_1','study_35');z <- prepare_graph_data(d,g)
 testthat::expect_true(all(z$.alpha[z$dataset_id=='study_2']==g$alpha*g$dim_alpha))
 testthat::expect_true(all(z$.alpha[z$dataset_id=='study_1']==g$alpha))
})
testthat::test_that('manual annotations remain bound to study, endpoint, treatment, dose and stratum', {
 p <- editor_fixture();d <- comparison_data(p,p$selected,'Relative liver weight');g <- graph_defaults()
 g$annotations <- dplyr::bind_rows(new_annotation(id='first',dataset_id='study_1',endpoint='Relative liver weight',sex='Male',treatment='Control',type='letter',text='ab'),
   new_annotation(id='second',dataset_id='study_2',endpoint='Relative liver weight',sex='Female',treatment='Control',type='asterisk',text='**'))
 f <- build_figure(d,g=g);testthat::expect_equal(nrow(f$annotations),2L)
 testthat::expect_equal(f$annotations$x1,c(0,0));testthat::expect_equal(f$annotations$dataset_id,c('study_1','study_2'));testthat::expect_equal(f$annotations$sex,c('Male','Female'))
 testthat::expect_true(any(grepl('user-provided',f$notes)))
 male <- comparison_data(p,p$selected,'Relative liver weight',list(sex='Male'));f <- build_figure(male,g=g)
 testthat::expect_equal(f$annotations$id,'first');testthat::expect_true(any(grepl('absent',f$notes)))
 bad <- g;bad$annotations$treatment[1] <- 'missing';f <- build_figure(d,g=bad);testthat::expect_false('first' %in% f$annotations$id)
})
testthat::test_that('letters, stars and brackets coexist on split and log axes with male/female facets', {
 p <- editor_fixture();d <- comparison_data(p,p$selected,'Relative liver weight');g <- graph_defaults();g$facet <- 'sex';a <- axis_settings()
 a$x$scale <- 'pseudo_log';a$y$scale <- 'log10';a$intervals <- parse_intervals('0,1,1\n10,100,1\n1000,10000,2')
 g$annotations <- dplyr::bind_rows(new_annotation(id='letter',dataset_id='study_1',endpoint='Relative liver weight',sex='Female',treatment='Dose 0.01',text='α',type='letter',y=6),
  new_annotation(id='star',dataset_id='study_3',endpoint='Relative liver weight',sex='Male',treatment='Dose 1000',text='***',y=6.2),
  new_annotation(id='bracket',dataset_id='study_4',endpoint='Relative liver weight',sex='Male',treatment='Control',treatment2='Dose 10000',type='bracket',text='p = 0.001',y=6.6))
 original <- d;f <- build_figure(d,a,g);testthat::expect_equal(d,original);testthat::expect_length(f$panels,3L)
 testthat::expect_equal(f$annotations$x1,c(.01,1000,0));testthat::expect_equal(f$annotations$x2,c(.01,1000,10000));testthat::expect_equal(f$annotations$plot_y,c(6,6.2,6.6))
 for(panel in f$panels) testthat::expect_no_error(ggplot2::ggplot_build(panel))
 testthat::expect_no_error(interactive_figure(f))
 for(fmt in c('png','svg','pdf')) {path <- tempfile(fileext=paste0('.',fmt));testthat::expect_no_error(export_figure(f,path,fmt,width=14,height=8));testthat::expect_gt(file.info(path)$size,1000)}
 a$x$scale <- 'log10';a$zero <- 'separate';a$intervals <- parse_intervals('0.001,1,1\n10,100,1\n1000,10000,1')
 f <- build_figure(d,a,g);testthat::expect_length(f$panels,4L);testthat::expect_equal(f$annotations$x1,c(.01,1000,0));testthat::expect_no_error(interactive_figure(f))
})
testthat::test_that('manual letters require unambiguous complete anchors rather than X alone', {
 p <- editor_fixture()
 anns <- import_letter_assignments('dataset_id,sex,treatment,letter\nstudy_1,Male,Control,a\nstudy_1,Female,Control,ab',p,'Relative liver weight')
 testthat::expect_equal(anns$sex,c('Male','Female'));testthat::expect_equal(anns$text,c('a','ab'))
 testthat::expect_error(import_letter_assignments('dataset_id,treatment,letter\nstudy_1,Control,a',p,'Relative liver weight'),'ambiguous')
 testthat::expect_error(import_letter_assignments('dataset_id,sex,treatment,letter\nstudy_99,Male,Control,a',p,'Relative liver weight'),'no matching')
})
testthat::test_that('automatic annotations use verified adjusted results without recomputing or pooling studies', {
 p <- analysis_fixture();before <- p$data;anns <- automatic_annotations(p,'asterisk')
 testthat::expect_equal(anns$p,p$results$p);testthat::expect_equal(anns$text,p_symbol(p$results$p,graph_defaults()$thresholds));testthat::expect_equal(anns$dataset_id,'study_4')
 testthat::expect_equal(p$data,before)
 p <- analysis_fixture('Tukey',c('Control','Dose 100','Dose 1000','Dose 10000'))
 anns <- automatic_annotations(p,'pvalue');testthat::expect_equal(anns$p,p$results$p);testthat::expect_equal(nrow(anns),6L)
 letters <- automatic_annotations(p,'letter');testthat::expect_equal(nrow(letters),4L);testthat::expect_true(all(letters$source=='automatic'))
 p2 <- analysis_fixture('Dunnett',c('Control','Dose 100','Dose 1000','Dose 10000'));testthat::expect_error(automatic_annotations(p2,'letter'),'control-only')
 p2 <- analysis_fixture('One-way ANOVA',c('Control','Dose 100','Dose 1000'));testthat::expect_error(automatic_annotations(p2),'Omnibus')
 p$data$value[p$data$dataset_id=='study_4' & p$data$sex=='Male'][1] <- 100;testthat::expect_error(automatic_annotations(p),'stale')
 old <- editor_fixture();old$results <- data.frame(p=.01);testthat::expect_error(automatic_annotations(old),'No validated')
})
testthat::test_that('automatic evidence survives save/reopen and stale rendered annotations are suppressed', {
 p <- analysis_fixture();g <- p$settings$graph;g$annotations <- automatic_annotations(p);p$settings$graph <- g
 path <- tempfile();save_project(p,path);q <- load_project(path)
 testthat::expect_no_error(automatic_annotations(q));d <- comparison_data(q,q$selected,'Relative liver weight')
 f <- build_figure(d,g=q$settings$graph);testthat::expect_equal(nrow(f$annotations),1L)
 row <- which(d$dataset_id=='study_4' & d$sex=='Male' & d$treatment=='Control')[1];d$value[row] <- d$value[row]+2
 f <- build_figure(d,g=q$settings$graph);testthat::expect_equal(nrow(f$annotations),0L);testthat::expect_true(any(grepl('stale',f$notes)))
})
testthat::test_that('compact letter display covers nonsignificant pairs and separates significant pairs', {
 groups <- c('Control','Low','High');pairs <- data.frame(group1=c('Control','Control','Low'),group2=c('Low','High','High'));p <- c(.5,.001,.4)
 labels <- compact_letters(groups,pairs,p)
 chars <- strsplit(labels,'')
 for(i in seq_len(nrow(pairs))) {overlap <- intersect(chars[[pairs$group1[i]]],chars[[pairs$group2[i]]]);if(p[i]<.05) testthat::expect_length(overlap,0L) else testthat::expect_gt(length(overlap),0)}
 testthat::expect_error(compact_letters(groups,pairs[1:2,],p[1:2]),'all-pairs')
 testthat::expect_equal(p_symbol(c(.05,.049,.009,.0009,.00009),graph_defaults()$thresholds),c('ns','*','**','***','****'))
})
testthat::test_that('v1 migration, v2 roundtrip, unknown fields and custom presets are preserved', {
 p <- editor_fixture();p$version <- 1L;p$settings$graph <- p$settings$graph[c('title','font_size','color','annotation')]
 p$settings$graph$title <- 'Manual title stays';p$settings$graph$future_field <- list(custom='retained')
 p$future_root <- list(value=42);p$presets$Legacy <- list(graph=p$settings$graph,axes=axis_settings())
 path <- tempfile();save_project(p,path);q <- load_project(path)
 testthat::expect_equal(q$version,2L);testthat::expect_equal(q$settings$graph$title,'Manual title stays');testthat::expect_equal(q$settings$graph$future_field,p$settings$graph$future_field)
 testthat::expect_equal(q$future_root,p$future_root);testthat::expect_equal(q$presets$Legacy$graph$title,'Manual title stays')
 q$settings$graph$title_color <- '#aa2255';q$settings$axes$x$tick_angle <- 37;q$settings$axes$break_symbol <- '⋯'
 q$settings$graph$annotations <- new_annotation(id='roundtrip',dataset_id='study_1',endpoint='Relative liver weight',sex='Male',treatment='Control',type='letter',text='α',y=6,dx=.001,dy=.1,face='italic')
 save_project(q,path);r <- load_project(path);testthat::expect_equal(r$settings$graph,q$settings$graph);testthat::expect_equal(r$settings$axes,q$settings$axes)
})
testthat::test_that('undo/redo restores graph and axes without changing scientific observations', {
 shiny::testServer(function(input,output,session) {
  state <- shiny::reactiveValues(project=editor_fixture(),graph=graph_defaults(),axes=axis_settings(),restore=0L,undo=list(),redo=list())
  output$dummy <- shiny::renderText('ready')
 },{
  original <- state$project$data;g <- state$graph;g$title <- 'Changed title';commit_edit(state,graph=g)
  a <- state$axes;a$x$title <- 'Dose label';commit_edit(state,axes=a)
  testthat::expect_length(state$undo,2L);travel_edit(state,'undo');testthat::expect_equal(state$axes$x$title,'')
  travel_edit(state,'undo');testthat::expect_equal(state$graph$title,graph_defaults()$title)
  travel_edit(state,'redo');testthat::expect_equal(state$graph$title,'Changed title');testthat::expect_equal(state$project$data,original)
 })
})
testthat::test_that('style presets remain editable and preserve annotation anchors and evidence', {
 p <- editor_fixture();g <- graph_defaults();g$title <- 'Custom';g$annotations <- new_annotation(id='keep',dataset_id='study_1',endpoint='Relative liver weight',sex='Male',treatment='Control')
 a <- axis_settings();a$intervals <- parse_intervals('0,1,1\n10,10000,2')
 for(name in c('Clean Scientific','Journal Black & White','Color Publication','Presentation')) {
  preset <- style_preset(name,g,a);testthat::expect_equal(preset$graph$title,'Custom');testthat::expect_equal(preset$graph$annotations[setdiff(names(g$annotations),'color')],g$annotations[setdiff(names(g$annotations),'color')]);testthat::expect_equal(preset$axes$intervals,a$intervals)
  if(name=='Journal Black & White') {testthat::expect_equal(preset$graph$annotations$color,'#222222');testthat::expect_equal(preset$axes$x$title_color,'#222222')}
 }
})
testthat::test_that('visual study order follows registry order rather than alphabetical display labels', {
 p <- editor_fixture();p$datasets <- p$datasets[c('study_3','study_1','study_4','study_2')];p$selected <- names(p$datasets)
 d <- comparison_data(p,p$selected,'Relative liver weight');z <- prepare_graph_data(d,graph_defaults())
 testthat::expect_match(levels(z$study)[1],'study_3');testthat::expect_match(levels(z$.color)[1],'study_3')
})
testthat::test_that('annotations follow dodged bar groups while retaining original dose coordinates', {
 p <- editor_fixture(2);d <- comparison_data(p,p$selected,'Relative liver weight');g <- graph_defaults();g$type <- 'Bar';g$facet <- 'sex';a <- axis_settings()
 g$annotations <- new_annotation(id='bar',dataset_id='study_2',endpoint='Relative liver weight',sex='Male',treatment='Control',text='a',y=7)
 f <- build_figure(d,a,g);row <- f$coordinates[f$coordinates$dataset_id=='study_2' & f$coordinates$sex=='Male' & f$coordinates$dose==0,]
 testthat::expect_equal(f$annotations$x1,0);testthat::expect_equal(f$annotations$display_x1,row$.display_x)
 built <- ggplot2::ggplot_build(f$panels[[1]]);bars <- built$data[[1]]
 testthat::expect_true(any(abs(as.numeric(bars$x)-row$.display_x)<1e-12))
 text <- tail(built$data,1)[[1]];testthat::expect_equal(text$x,row$.display_x)
})
testthat::test_that('changing asterisk cutoffs updates validated labels without changing p-values or observations', {
 p <- analysis_fixture();g <- graph_defaults();g$annotations <- automatic_annotations(p);old_p <- g$annotations$p;original <- p$data
 g$thresholds <- data.frame(p=c(1e-10,.05),label=c('strong','detected'));a <- effective_annotations(g)
 testthat::expect_equal(a$text,'detected');testthat::expect_equal(a$p,old_p);testthat::expect_equal(p$data,original)
 g$annotations <- automatic_annotations(p,'pvalue');g$p_digits <- 15;g$p_format <- 'decimal'
 testthat::expect_match(effective_annotations(g)$text,'p = 0.0000000')
 g$p_format <- 'threshold';testthat::expect_match(effective_annotations(g)$text,'p < 0.05')
})
testthat::test_that('interval reordering follows the selected draft row and keeps invalid order out of the figure', {
 a <- axis_settings();a$intervals <- parse_intervals('0,1,1\n10,100,1\n1000,10000,2')
 state <- shiny::reactiveValues(graph=graph_defaults(),axes=a,restore=0L)
 shiny::testServer(axes_server,args=list(state=state),{
  session$setInputs(interval_table_rows_selected=3,interval_up=1)
  testthat::expect_equal(selected_interval(),2L);testthat::expect_equal(state$axes$intervals,a$intervals)
  testthat::expect_match(status(),'must be ordered')
  session$setInputs(interval_down=1)
  testthat::expect_equal(selected_interval(),3L);testthat::expect_equal(draft(),a$intervals)
 })
})
testthat::test_that('legacy positional colors migrate to stable study and subgroup keys', {
 p <- editor_fixture();d <- comparison_data(p,p$selected,'Relative liver weight');g <- graph_defaults();g$colors <- '#ff0000,#00ff00,#0000ff,#111111'
 g <- migrate_positional_colors(g,d,legacy_order=TRUE);testthat::expect_equal(g$colors,'')
 small <- comparison_data(p,c('study_4','study_1'),'Relative liver weight');colors <- resolve_graph_colors(prepare_graph_data(small,g),g)
 testthat::expect_equal(unname(colors),c('#111111','#ff0000'))
 g <- graph_defaults();g$color <- c('study','sex');g$colors <- paste(grDevices::hcl.colors(8),collapse=',');g <- migrate_positional_colors(g,d,legacy_order=TRUE)
 all <- resolve_graph_colors(prepare_graph_data(d,g),g);small <- comparison_data(p,c('study_1','study_4'),'Relative liver weight',list(sex='Male'))
 cols <- resolve_graph_colors(prepare_graph_data(small,g),g);testthat::expect_equal(cols,all[names(cols)])
 g$unknown_future <- list(retain=42);testthat::expect_equal(reset_known_settings(g,graph_defaults())$unknown_future,g$unknown_future)
})
testthat::test_that('unknown scientific fields are rejected explicitly rather than ignored or pooled', {
 p <- editor_fixture();p$data$future_stratum <- 'unmapped genotype';path <- tempfile();save_project(p,path)
 original <- readBin(path,'raw',n=file.info(path)$size)
 testthat::expect_error(load_project(path),'Unrecognized observation fields')
 testthat::expect_identical(readBin(path,'raw',n=file.info(path)$size),original)
})
testthat::test_that('automatic metadata titles do not overwrite retained manual labels', {
 p <- editor_fixture();d <- comparison_data(p,p$selected,'Relative liver weight');g <- graph_defaults();g$title <- 'Retained manual title';g$title_auto <- TRUE
 f <- build_figure(d,g=g);testthat::expect_match(f$title,'Relative liver weight');testthat::expect_equal(g$title,'Retained manual title')
 g$title_auto <- FALSE;g$type <- 'Mean ± SD';testthat::expect_equal(build_figure(d,g=g)$title,'Retained manual title')
})
testthat::test_that('zero-control label offsets stay in the linear control panel, not the positive log panel', {
 p <- editor_fixture(1);d <- comparison_data(p,p$selected,'Relative liver weight');a <- axis_settings();a$x$scale <- 'log10';a$zero <- 'separate';g <- graph_defaults()
 g$annotations <- new_annotation(id='control_offset',dataset_id='study_1',endpoint='Relative liver weight',sex='Male',treatment='Control',text='a',dx=.2,y=6)
 f <- build_figure(d,a,g);testthat::expect_equal(f$annotations$x1,0);testthat::expect_equal(f$annotations$display_x1,.2)
 labels <- vapply(f$panels,function(p) sum(vapply(p$layers,function(layer) inherits(layer$geom,'GeomText'),logical(1))),integer(1))
 testthat::expect_equal(labels,c(1L,0L));testthat::expect_equal(f$annotations$original_dose1,0)
})
testthat::test_that('ambiguous doses cannot receive a time-axis label merely because X is shared', {
 p <- editor_fixture(1);d <- comparison_data(p,p$selected,'Relative liver weight');d$exposure <- '28';d$treatment[d$dose>0] <- 'Active'
 g <- graph_defaults();g$x_variable <- 'exposure';g$annotations <- new_annotation(id='ambiguous',dataset_id='study_1',endpoint='Relative liver weight',sex='Male',exposure='28',treatment='Active')
 f <- build_figure(d,g=g);testthat::expect_equal(nrow(f$annotations),0L);testthat::expect_true(any(grepl('multiple doses',f$notes)))
 g$annotations$dose <- .01;testthat::expect_equal(nrow(build_figure(d,g=g)$annotations),1L)
})
testthat::test_that('brackets with omitted endpoints cannot masquerade as comparisons between visible groups', {
 p <- analysis_fixture();d <- comparison_data(p,p$selected,'Relative liver weight');g <- graph_defaults();g$annotations <- automatic_annotations(p)
 a <- axis_settings();a$intervals <- parse_intervals('10,100,1\n1000,2000,1')
 f <- build_figure(d,a,g);testthat::expect_equal(nrow(f$annotations),0L);testthat::expect_true(any(grepl('both anchors',f$notes)))
})
testthat::test_that('editing an automatic p-value makes its updated label manual and rejects invalid probabilities', {
 p <- analysis_fixture();g <- graph_defaults();g$annotations <- automatic_annotations(p,'pvalue')
 state <- shiny::reactiveValues(project=p,graph=g,axes=axis_settings(),restore=0L,undo=list(),redo=list())
 shiny::testServer(annotations_server,args=list(state=state),{
  original <- state$project$data;column <- match('p',setdiff(names(state$graph$annotations),'evidence'))-1L
  session$setInputs(table_cell_edit=list(row=1,col=column,value='0.02'))
  testthat::expect_equal(state$graph$annotations$source,'manual');testthat::expect_equal(state$graph$annotations$p,.02)
  testthat::expect_equal(state$graph$annotations$text,format_p_label(.02,state$graph));testthat::expect_equal(state$graph$annotations$evidence,'')
  session$setInputs(table_cell_edit=list(row=1,col=column,value='1.5'))
  testthat::expect_equal(state$graph$annotations$p,.02);testthat::expect_equal(state$project$data,original)
 })
})
testthat::test_that('preparation keys exclude presentation order and typography but include actual scientific edits', {
 p <- editor_fixture();d <- comparison_data(p,p$selected,'Relative liver weight');g <- graph_defaults();original <- preparation_fingerprint(d)
 d$study <- factor(as.character(d$study),levels=rev(levels(d$study)));testthat::expect_equal(preparation_fingerprint(d),original)
 g2 <- g;g2$title <- 'Cosmetic';g2$type <- 'Mean ± SD';testthat::expect_equal(graph_analysis_config(g),graph_analysis_config(g2))
 d$value[1] <- d$value[1]+1;testthat::expect_false(identical(preparation_fingerprint(d),original))
})
testthat::test_that('sex symbols and line patterns retain identity when other groups are filtered out', {
 p <- editor_fixture();d <- comparison_data(p,p$selected,'Relative liver weight');g <- graph_defaults();g$line <- 'sex'
 all <- prepare_graph_data(d,g);male <- prepare_graph_data(comparison_data(p,p$selected,'Relative liver weight',list(sex='Male')),g)
 testthat::expect_equal(unname(resolve_shape_values(all,g)['Male']),17);testthat::expect_equal(unname(resolve_shape_values(male,g)['Male']),17)
 testthat::expect_equal(unname(resolve_line_values(all,g)['Male']),'dashed');testthat::expect_equal(unname(resolve_line_values(male,g)['Male']),'dashed')
 g$shapes <- '15,18';g$line_types <- 'dotted,longdash';g <- migrate_legacy_symbols(g,male,d)
 testthat::expect_equal(g$shapes,'');testthat::expect_equal(g$line_types,'')
 testthat::expect_equal(unname(resolve_shape_values(male,g)['Male']),18);testthat::expect_equal(unname(resolve_line_values(male,g)['Male']),'longdash')
})
testthat::test_that('publication labels can omit technical IDs without merging identically named studies', {
 p <- editor_fixture(2);d <- comparison_data(p,p$selected,'Relative liver weight');g <- graph_defaults()
 g$dataset_styles <- data.frame(dataset_id=c('study_1','study_2'),label=c('Smith 2024','Jones 2025'),color=c('',''))
 z <- prepare_graph_data(d,g);testthat::expect_equal(levels(z$study),c('Smith 2024','Jones 2025'))
 g$dataset_styles$label <- 'Same label';z <- prepare_graph_data(d,g)
 testthat::expect_length(levels(z$study),2L);testthat::expect_length(unique(unname(resolve_graph_colors(z,g))),2L)
})
testthat::test_that('v1 automatic palette appearances migrate once and then remain stable by study ID', {
 p <- editor_fixture(2);p$version <- 1L;path <- tempfile();save_project(p,path);q <- load_project(path)
 expected <- palette_values(2,'okabe');d <- comparison_data(q,q$selected,'Relative liver weight');colors <- resolve_graph_colors(prepare_graph_data(d,q$settings$graph),q$settings$graph)
 testthat::expect_equal(unname(colors),expected)
 d <- comparison_data(q,'study_2','Relative liver weight');testthat::expect_equal(unname(resolve_graph_colors(prepare_graph_data(d,q$settings$graph),q$settings$graph)),expected[2])
})

testthat::test_that('faceted split Plotly figures retain independent shared axis typography', {
 p <- editor_fixture(4);d <- comparison_data(p,p$selected,'Relative liver weight');g <- graph_defaults();g$facet <- 'sex'
 a <- axis_settings();a$x$title <- 'Custom administered dose';a$y$title <- 'Custom response';a$x$title_size <- 19;a$y$title_color <- '#145b73'
 a$intervals <- data.frame(start=c(0,10,1000),end=c(1,100,10000),width=c(1,1,2))
 w <- plotly::plotly_build(interactive_figure(build_figure(d,a,g)));annotations <- w$x$layout$annotations
 x <- Filter(function(x) grepl('Custom administered dose',x$text %||% '',fixed=TRUE),annotations)
 y <- Filter(function(x) grepl('Custom response',x$text %||% '',fixed=TRUE),annotations)
 testthat::expect_length(x,1);testthat::expect_length(y,1);testthat::expect_equal(x[[1]]$font$size,19);testthat::expect_equal(y[[1]]$font$color,'#145b73')
})
