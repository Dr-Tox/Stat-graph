for(f in list.files('R',full.names=TRUE)) source(f)
p <- example_project(25)
for(i in seq_len(25)) {
 d <- p$data[p$data$dataset_id==paste0('study_',i),names(empty_data())]
 utils::write.csv(d,file.path('examples',sprintf('study_%02d.csv',i)),row.names=FALSE,na='')
}
s <- summarise_groups(p$data[p$data$dataset_id=='study_1',]);utils::write.csv(s,'examples/summary.csv',row.names=FALSE)
openxlsx::write.xlsx(list(Study_A=p$data[p$data$dataset_id=='study_1',],Study_B=p$data[p$data$dataset_id=='study_2',]),'examples/studies.xlsx')
wide <- data.frame(sex=c('Female','Female','Male','Male'),'0'=c(4,4.2,4.4,4.5),'0.01'=c(4.2,4.3,4.5,4.8),'1000'=c(4.9,5,5.2,5.4),check.names=FALSE)
utils::write.csv(wide,'examples/wide_doses.csv',row.names=FALSE)
p <- example_project();p$settings <- list(axes=axis_settings(),graph=graph_defaults(),view=list(endpoint='Relative liver weight',filters=list()),statistics=list())
p$settings$axes$x$scale <- 'pseudo_log';save_project(p,'examples/four_studies.statlab.json')

legacy <- data.frame(study_name=rep(c('Legacy A','Legacy B'),each=4),dose_mg_kg_day=c(0,.001,.01,.1,0,100,1000,10000),sex='Female',generation='F0',days_exposed=28,collection_time=28,mean_serumconc_ng_ML=c(1,2,3,4,1,4,7,10),sd_serumconc=.3,mean_body_weight_g=c(200,201,202,203,201,204,207,210))
utils::write.csv(legacy,'examples/legacy_original_schema.csv',row.names=FALSE)
