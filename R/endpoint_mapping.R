# Unit conversion is explicit, multiplicative, and restricted to compatible dimensions.
unit_table <- data.frame(unit=c('mg/kg/day','ug/kg/day','ng/kg/day','g','mg','ug','kg','U/L','ng/mL','ug/mL','mg/mL','%','g/100g','unitless'),
 dimension=c(rep('dose',3),rep('mass',4),'enzyme',rep('concentration',3),'relative','relative','unitless'),
 base=c(1,1e-3,1e-6,1,1e-3,1e-6,1000,1,1,1000,1e6,1,1,1))
unit_factor <- function(from,to) {
  if (identical(from,to)) return(1)
  a <- match(from,unit_table$unit); b <- match(to,unit_table$unit)
  if (is.na(a) || is.na(b) || unit_table$dimension[a] != unit_table$dimension[b])
    fail(paste('Cannot convert',from,'to',to,'as compatible units.'))
  unit_table$base[a]/unit_table$base[b]
}
map_table <- function(tab, mapping, mode='raw', endpoint, dose_unit, response_unit,
                      target_dose=dose_unit, target_response=response_unit, wide_columns=NULL) {
  if (!is.data.frame(tab) || !nrow(tab) || anyDuplicated(names(tab))) fail('Input must be nonempty with unique column names.')
  if (!nzchar(endpoint) && (is.null(mapping$endpoint) || mapping$endpoint=='(none)')) fail('Name or map the biological endpoint explicitly.')
  if (length(wide_columns)) {
    if (mode!='raw') fail('Wide response columns currently support raw observations only.')
    tab <- tidyr::pivot_longer(tab, cols=dplyr::all_of(wide_columns), names_to='.wide_dose', values_to='.wide_value')
    mapping$dose <- '.wide_dose'; mapping$value <- '.wide_value'
  }
  get_col <- function(key, default=NA) {
    col <- mapping[[key]]
    if (is.null(col) || !nzchar(col) || col=='(none)') return(rep(default,nrow(tab)))
    if (!col %in% names(tab)) fail(paste('Column not found:',col))
    tab[[col]]
  }
  numeric_col <- function(key, required=TRUE) {
    v <- get_col(key)
    z <- suppressWarnings(as.numeric(as.character(v)))
    if(any(!is.na(v) & nzchar(as.character(v)) & !is.finite(z))) fail(paste(key,'contains nonnumeric or infinite entries.'))
    if (required && any(!is.finite(z))) fail(paste('Map',key,'to finite numeric values; missing values must be resolved explicitly.'))
    z
  }
  d <- empty_data()[rep(NA_integer_,nrow(tab)),,drop=FALSE]
  d$dataset_id <- 'pending'; d$endpoint <- as.character(get_col('endpoint',endpoint)); d$mode <- mode
  source_dose <- as.character(get_col('dose_unit',dose_unit)); source_response <- as.character(get_col('response_unit',response_unit))
  dose_factor <- if(nzchar(target_dose)) vapply(source_dose,unit_factor,numeric(1),to=target_dose,USE.NAMES=FALSE) else rep(1,nrow(tab))
  d$dose <- numeric_col('dose')*dose_factor
  d$dose_unit <- if(nzchar(target_dose)) target_dose else source_dose
  d$response_unit <- if(nzchar(target_response)) target_response else source_response
  f <- if(nzchar(target_response)) vapply(source_response,unit_factor,numeric(1),to=target_response,USE.NAMES=FALSE) else rep(1,nrow(tab))
  d$value <- if (mode=='raw') numeric_col('value')*f else NA_real_
  d$mean <- if (mode=='summary') numeric_col('mean')*f else NA_real_
  d$sd <- if (mode=='summary') numeric_col('sd',FALSE)*abs(f) else NA_real_
  d$n <- if (mode=='summary') numeric_col('n',FALSE) else NA_real_
  for (nm in factors) {
    v <- as.character(get_col(nm,'Unspecified')); v[is.na(v) | !nzchar(v)] <- 'Unspecified'; d[[nm]] <- v
  }
  d$treatment <- as.character(get_col('treatment',NA))
  absent <- is.na(d$treatment) | !nzchar(d$treatment)
  d$treatment[absent] <- paste0('Dose ',format(d$dose[absent],trim=TRUE))
  numeric_names <- names(tab)[vapply(tab,is.numeric,logical(1))]
  for(nm in numeric_names) d[[paste0('xaux_',make.names(nm))]] <- as.numeric(tab[[nm]])
  rownames(d) <- NULL; validate_data(d); d
}
read_table_file <- function(path, name=path, sheet=1) {
  ext <- tolower(tools::file_ext(name))
  if (ext=='xlsx') return(as.data.frame(readxl::read_excel(path,sheet=sheet,.name_repair='check_unique')))
  if (ext=='csv') return(as.data.frame(readr::read_csv(path,show_col_types=FALSE,name_repair='check_unique')))
  fail('Supported files: CSV and XLSX.')
}
