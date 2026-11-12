for(f in list.files('R',full.names=TRUE)) source(f)
rows <- list()
for(studies in c(1,2,5,14,25)) for(repeats in c(1,10,100)) {
 gc();p <- example_project(studies)
 p$data <- dplyr::bind_rows(rep(list(p$data),repeats))
 path <- tempfile();Rprofmem(path)
 timing <- system.time({d <- comparison_data(p,p$selected,'Relative liver weight');fig <- build_figure(d)})
 Rprofmem(NULL);alloc <- suppressWarnings(as.numeric(sub(' .*','',readLines(path))));unlink(path)
 rows[[length(rows)+1]] <- data.frame(studies=studies,observations=nrow(p$data),elapsed_seconds=unname(timing['elapsed']),
   project_MB=as.numeric(object.size(p))/1024^2,figure_MB=as.numeric(object.size(fig))/1024^2,
   allocated_MB=sum(alloc,na.rm=TRUE)/1024^2)
}
utils::write.csv(dplyr::bind_rows(rows),'artifacts/performance.csv',row.names=FALSE)
print(dplyr::bind_rows(rows))
