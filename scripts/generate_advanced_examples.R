for(f in list.files('R',full.names=TRUE,pattern='\\.R$')) source(f)
p <- example_project();g <- graph_defaults();a <- axis_settings()
g$type <- 'Mean ± SD';g$title <- 'Liver response across independent studies';g$title_size <- 22;g$title_color <- '#145b73';g$title_face <- 'bold'
g$subtitle <- 'Synthetic data · means ± SD · user-provided letters and validated Welch comparison';g$subtitle_size <- 12;g$facet <- 'sex';g$facet_columns <- 1;g$font_size <- 11
g$export_width <- 14;g$export_height <- 8;g$export_dpi <- 300;g$point_size <- 2.6;g$legend_size <- 9
g$dataset_styles <- data.frame(dataset_id=c('study_1','study_2','study_3','study_4'),label=c('Low-dose study','Intermediate study','High-dose study','Broad-dose study'),color=c('#0072b2','#d55e00','#009e73','#a655a6'))
a$x$scale <- 'pseudo_log';a$y$scale <- 'log10';a$x$title <- 'Administered dose';a$y$title <- 'Relative liver weight';a$y$unit_position <- 'separate'
a$x$title_color <- '#145b73';a$y$title_color <- '#145b73';a$x$title_face <- 'bold';a$y$title_face <- 'italic'
a$intervals <- parse_intervals('0,1,1\n10,100,1\n1000,10000,2')
g$annotations <- dplyr::bind_rows(
 new_annotation(id='manual_female_a',dataset_id='study_1',endpoint='Relative liver weight',sex='Female',treatment='Control',type='letter',text='a',y=6.1,color='#0072b2'),
 new_annotation(id='manual_female_b',dataset_id='study_1',endpoint='Relative liver weight',sex='Female',treatment='Dose 0.1',type='letter',text='b',y=6.3,color='#0072b2'),
 new_annotation(id='manual_male_ab',dataset_id='study_3',endpoint='Relative liver weight',sex='Male',treatment='Dose 1000',type='letter',text='ab',y=6.2,color='#009e73'),
 new_annotation(id='manual_male_star',dataset_id='study_2',endpoint='Relative liver weight',sex='Male',treatment='Dose 10',type='asterisk',text='**',y=6.2,color='#d55e00'),
 new_annotation(id='unicode_alpha',dataset_id='study_3',endpoint='Relative liver weight',sex='Female',treatment='Dose 1000',type='letter',text='α',face='italic',y=6.2,color='#009e73'))
s <- list(dataset='study_4',endpoint='Relative liver weight',method='Welch t-test',groups=c('Control','Dose 10000'),control='Control',alpha=.05,adjust='holm',strata=stats::setNames(as.list(rep('Unspecified',length(factors))),factors));s$strata$sex <- 'Male'
d <- p$data[p$data$dataset_id=='study_4' & p$data$sex=='Male' & p$data$treatment %in% s$groups,,drop=FALSE]
p$results <- run_analysis(d,s$method);p$analysis_context <- analysis_context(d,s,p$results)
auto <- automatic_annotations(p,'asterisk');auto$id <- 'validated_welch';auto$y <- 6.7;auto$color <- '#7c3692'
g$annotations <- dplyr::bind_rows(g$annotations,auto)
p$settings <- list(graph=g,axes=a,view=list(endpoint='Relative liver weight',filters=list(),hidden=character()),statistics=s)
p$presets$Annotated_example <- list(graph=g,axes=a)
save_project(p,'examples/advanced-21.statlab.json')
fig <- build_figure(comparison_data(p,p$selected,'Relative liver weight'),a,g)
for(fmt in c('png','svg','pdf')) export_figure(fig,paste0('artifacts/advanced-21.',fmt),fmt,g$export_width,g$export_height,g$export_dpi)
utils::write.csv(fig$annotations,'artifacts/advanced-21-anchors.csv',row.names=FALSE)
# Compare the exact same figure before and after serialization.
q <- load_project('examples/advanced-21.statlab.json');restored <- build_figure(comparison_data(q,q$selected,'Relative liver weight'),q$settings$axes,q$settings$graph)
export_figure(restored,'artifacts/advanced-21-restored.png','png',g$export_width,g$export_height,g$export_dpi)
stopifnot(identical(fig$annotations,restored$annotations))
cat('Complex multi-study, male/female, three-interval annotated exports generated and restored anchors verified.\n')
