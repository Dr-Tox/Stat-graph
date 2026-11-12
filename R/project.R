new_project <- function() list(format='StatLab', version=2L, next_id=1L, data=empty_data(),
 datasets=list(),selected=character(), settings=list(), presets=list(), results=data.frame(),
 reproducibility=list(created=format(Sys.time(),tz='UTC'),R=R.version.string))
save_project <- function(project,path) {
  project$reproducibility$saved <- format(Sys.time(),tz='UTC')
  project$reproducibility$packages <- as.list(vapply(c('shiny','ggplot2','plotly','patchwork'),function(p) as.character(utils::packageVersion(p)),character(1)))
  jsonlite::write_json(project,path,pretty=TRUE,auto_unbox=TRUE,na='null',dataframe='rows',digits=17)
}
load_project <- function(path) {
  if (file.info(path)$size > 500*1024^2) fail('Project exceeds 500 MB import safety threshold; split the file or raise the documented loader threshold locally.')
  p <- jsonlite::read_json(path,simplifyVector=FALSE)
  if (!identical(p$format,'StatLab') || !p$version %in% c(1,2)) fail('Unsupported project format/version.')
  required <- c('datasets','data','next_id','settings','selected','presets','results','reproducibility')
  if (!all(required %in% names(p))) fail('Project structure is incomplete.')
  original_version <- p$version;p$version <- 2L
  if (length(p$data)) {
    unknown <- setdiff(unique(unlist(lapply(p$data,names))),c(names(empty_data()),unique(unlist(lapply(p$data,function(row) grep('^xaux_',names(row),value=TRUE))))))
    if(length(unknown)) fail(paste('Unrecognized observation fields:',paste(unknown,collapse=', '),'. The project was not opened. Map these fields explicitly or use a compatible app; the source file is unchanged.'))
    fields <- unique(c(names(empty_data()),unlist(lapply(p$data,function(row) grep('^xaux_',names(row),value=TRUE)))))
    p$data <- as.data.frame(lapply(fields,function(nm) {
      proto <- if(startsWith(nm,'xaux_')) numeric() else empty_data()[[nm]]
      v <- vapply(p$data,function(row) row[[nm]] %||% if(is.numeric(proto)) NA_real_ else NA_character_,
                  if(is.numeric(proto)) numeric(1) else character(1))
      v
    }),stringsAsFactors=FALSE); names(p$data) <- fields; validate_data(p$data)
  } else p$data <- empty_data()
  p$selected <- unlist(p$selected,use.names=FALSE) %||% character()
  if (!all(unique(p$data$dataset_id) %in% names(p$datasets)) || !all(p$selected %in% names(p$datasets))) fail('Project references unknown dataset IDs.')
  if (!is.numeric(p$next_id) || length(p$next_id)!=1 || !is.finite(p$next_id) || p$next_id < 1) fail('Invalid project identifier counter.')
  existing <- suppressWarnings(as.integer(sub('study_','',names(p$datasets),fixed=TRUE)))
  if (anyNA(existing) || any(existing >= p$next_id)) fail('Project counter could overwrite study identities.')
  p$results <- restore_result_rows(p$results)
  if(length(p$settings$graph)) p$settings$graph <- normalize_settings(p$settings$graph,graph_defaults())
  if(length(p$settings$axes)) p$settings$axes <- normalize_settings(p$settings$axes,axis_settings())
  p$reproducibility$loaded_project_version <- original_version
  if(original_version==1L && length(p$settings$graph) && !nzchar(p$settings$graph$colors) && !nrow(p$settings$graph$dataset_styles) && !nrow(p$settings$graph$legacy_group_colors) && length(p$selected)) {
   view <- p$settings$view
   old_data <- tryCatch(comparison_data(p,p$selected,unlist(view$endpoint),lapply(view$filters,unlist)),error=function(e) NULL)
   if(!is.null(old_data)) {
    old_groups <- sort(unique(map_identity(old_data,p$settings$graph$color)))
    p$settings$graph$colors <- paste(palette_values(length(old_groups),p$settings$graph$palette),collapse=',')
   }
  }
  if(length(p$settings$graph) && nzchar(p$settings$graph$colors)) {
   view <- p$settings$view
   conversion <- tryCatch(migrate_positional_colors(p$settings$graph,comparison_data(p,p$selected,unlist(view$endpoint),lapply(view$filters,unlist)),legacy_order=original_version==1L),error=function(e) e)
   if(inherits(conversion,'error')) p$extensions$color_migration_notice <- conditionMessage(conversion) else p$settings$graph <- conversion
  }
  if(length(p$settings$graph) && (nzchar(p$settings$graph$shapes) || nzchar(p$settings$graph$line_types))) {
   view <- p$settings$view
   conversion <- tryCatch(migrate_legacy_symbols(p$settings$graph,comparison_data(p,p$selected,unlist(view$endpoint),lapply(view$filters,unlist)),comparison_data(p,p$selected,unlist(view$endpoint))),error=function(e) e)
   if(inherits(conversion,'error')) p$extensions$color_migration_notice <- conditionMessage(conversion) else p$settings$graph <- conversion
  }
  p
}
example_project <- function(count=4L) {
  p <- new_project(); set.seed(2026)
  ranges <- list(c(0,.001,.01,.1),c(0,.1,1,10),c(0,10,100,1000),c(0,100,1000,10000))
  for (i in seq_len(count)) {
    doses <- ranges[[(i-1)%%4+1]]
    tab <- expand.grid(dose=doses,sex=c('Female','Male'),replicate=seq_len(8),stringsAsFactors=FALSE)
    tab$value <- 4 + .3*log10(1+tab$dose) + ifelse(tab$sex=='Male',.4,0) + rnorm(nrow(tab),0,.25)
    tab$treatment <- ifelse(tab$dose==0,'Control',paste('Dose',tab$dose))
    d <- map_table(tab,list(dose='dose',value='value',sex='sex',treatment='treatment'),
      endpoint='Relative liver weight',dose_unit='mg/kg/day',response_unit='%')
    p <- add_dataset(p,d,paste('Study',i),list(species='Synthetic rat',generation='F0',notes='Synthetic data; not experimental findings.'))
  }; p
}

restore_result_rows <- function(rows) {
 if(is.data.frame(rows)) return(rows)
 if(!length(rows)) return(data.frame())
 fields <- unique(unlist(lapply(rows,names)));out <- as.data.frame(dplyr::bind_rows(rows))
 for(nm in setdiff(fields,names(out))) out[[nm]] <- rep(NA_real_,length(rows))
 out[fields]
}
