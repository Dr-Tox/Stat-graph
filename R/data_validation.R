`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
fail <- function(message) stop(message, call. = FALSE)
factors <- c('sex', 'generation', 'exposure', 'collection_time', 'subgroup')
empty_data <- function() data.frame(dataset_id=character(), endpoint=character(), dose=numeric(),
  dose_unit=character(), response_unit=character(), mode=character(), value=numeric(),
  mean=numeric(), sd=numeric(), n=numeric(), treatment=character(), sex=character(),
  generation=character(), exposure=character(), collection_time=character(), subgroup=character())
validate_data <- function(d) {
  if (!is.data.frame(d) || !nrow(d)) fail('No observations. Import a nonempty table.')
  missing <- setdiff(names(empty_data()), names(d))
  if (length(missing)) fail(paste('Missing canonical columns:', paste(missing, collapse=', ')))
  for (nm in c('dataset_id','endpoint','dose_unit','response_unit','mode','treatment', factors)) {
    if (anyNA(d[[nm]]) || any(!nzchar(trimws(as.character(d[[nm]]))))) fail(paste('Missing text in', nm))
  }
  if (any(!d$mode %in% c('raw','summary'))) fail('Input mode must be raw or summary.')
  for (nm in c('dose','value','mean','sd','n')) if (!is.numeric(d[[nm]])) fail(paste(nm, 'must be numeric.'))
  if (any(!is.finite(d$dose))) fail('Dose contains missing or nonnumeric values.')
  raw <- d$mode == 'raw'; s <- !raw
  if (any(!is.finite(d$value[raw]))) fail('Raw responses contain missing or nonnumeric values. Correct them explicitly.')
  if (any(!is.finite(d$mean[s]))) fail('Summary rows require finite means.')
  if (any(!is.na(d$sd[s]) & !is.finite(d$sd[s])) || any(!is.na(d$n[s]) & !is.finite(d$n[s]))) fail('Summary SD and n must be finite when supplied.')
  if (any(d$sd[s] < 0 | d$n[s] < 2 | d$n[s] != floor(d$n[s]),na.rm=TRUE)) fail('Summary SD must be nonnegative; n must be an integer >= 2.')
  keys <- c('dataset_id','endpoint','dose','treatment',factors)
  if (anyDuplicated(d[s,keys,drop=FALSE])) fail('Duplicate summary groups: distinguish experimental strata or combine them explicitly upstream.')
  invisible(d)
}
add_dataset <- function(project, data, name, metadata=list(), mapping=list()) {
  id <- paste0('study_', project$next_id)
  data$dataset_id <- id
  validate_data(data)
  project$data <- dplyr::bind_rows(project$data, data)
  project$datasets[[id]] <- list(id=id, name=name, metadata=metadata, mapping=mapping)
  project$selected <- c(project$selected, id)
  project$next_id <- project$next_id + 1L
  project
}
remove_dataset <- function(project, id) {
  project$data <- project$data[!project$data$dataset_id %in% id,,drop=FALSE]
  project$datasets[id] <- NULL
  project$selected <- setdiff(project$selected,id)
  project
}
comparison_data <- function(project, selected, endpoint, filters=list()) {
  d <- project$data
  d <- d[d$dataset_id %in% selected & d$endpoint == endpoint,,drop=FALSE]
  for (nm in intersect(names(filters), factors)) if (length(filters[[nm]])) d <- d[d[[nm]] %in% filters[[nm]],,drop=FALSE]
  if (!nrow(d)) fail('No observations match this selection.')
  if (length(unique(d$dose_unit)) != 1 || length(unique(d$response_unit)) != 1)
    fail('Incompatible units. Explicitly map all selected studies to the same dose and response units in Data Manager.')
  d$study <- factor(vapply(d$dataset_id, function(id) paste0(project$datasets[[id]]$name,' [',id,']'), character(1)),
    levels=unique(vapply(selected, function(id) paste0(project$datasets[[id]]$name,' [',id,']'), character(1))))
  d
}
summarise_groups <- function(d, confidence=.95) {
  keys <- c('dataset_id','endpoint','dose','dose_unit','response_unit','treatment',factors)
  raw <- d[d$mode=='raw',,drop=FALSE]
  raw <- dplyr::summarise(dplyr::group_by(raw, dplyr::across(dplyr::all_of(keys))),
    mean=mean(value), sd=stats::sd(value), n=dplyr::n(), .groups='drop')
  raw$mode <- rep('raw', nrow(raw))
  sm <- d[d$mode=='summary',c(keys,'mean','sd','n','mode'),drop=FALSE]
  out <- dplyr::bind_rows(raw,sm)
  # A raw and a summary representation of one group must not be double counted.
  if (anyDuplicated(out[keys])) fail('Raw and summary rows describe the same group. Select one representation.')
  out$se <- out$sd / sqrt(out$n)
  out$lower <- out$mean - stats::qt((1+confidence)/2, pmax(out$n-1,1))*out$se
  out$upper <- out$mean + stats::qt((1+confidence)/2, pmax(out$n-1,1))*out$se
  out$confidence <- rep(confidence,nrow(out))
  out
}
