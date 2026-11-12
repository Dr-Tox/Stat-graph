packages <- c('shiny','bslib','ggplot2','plotly','dplyr','tidyr','readr','readxl','DT','patchwork','scales','jsonlite','openxlsx','multcomp','svglite','RColorBrewer','rlang','testthat','renv','digest')
missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) install.packages(missing,repos='https://cloud.r-project.org')
