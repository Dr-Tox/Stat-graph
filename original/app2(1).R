library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(RColorBrewer)
library(shinyBS)
library(plotly)

# ── Data ─────────────────────────────────────────────────────────────────────
df <- read_csv("animal_data_pfhxs.csv")
df <- df %>% mutate(
 dose_micrograms_kg_day=as.numeric(1000*dose_mg_kg_day),
 dose_ng_kg_day=as.numeric(1000000*dose_mg_kg_day),
 grp_study=study_name,
 grp_study_dose=paste(study_name,dose_mg_kg_day,sep="_"),
 grp_study_dose_time=paste(study_name,dose_mg_kg_day,collection_time,sep="_"),
 grp_study_sex_time=paste(study_name,sex,collection_time,sep="_"),
 grp_study_dose_sex=paste(study_name,dose_mg_kg_day,sex,sep="_"),
 grp_study_sex=paste(study_name,sex,sep="_"),
 grp_study_time_dose=paste(study_name,collection_time,dose_mg_kg_day,sep="_"),
 grp_full=paste(study_name,sex,dose_mg_kg_day,sep="_"),
 grp_study_time=paste(study_name,collection_time,sep="_"),
 grp_study_dose_generation=paste(study_name,dose_mg_kg_day,generation,sep="_"),
 grp_study_generation=paste(study_name,generation,sep="_"))
num_cols <- names(df)[sapply(df,is.numeric)]
sd_cols <- names(df)[grepl("^sd",names(df),ignore.case=TRUE)]
x_value_cols <- setdiff(num_cols,sd_cols)
y_value_cols <- setdiff(num_cols,sd_cols)
LEGEND_CHOICES <- c("Study"="grp_study","Study + Dose"="grp_study_dose",
 "Study + Dose + Time"="grp_study_dose_time","Study + Sex + Time"="grp_study_sex_time",
 "Study + Dose + Sex"="grp_study_dose_sex","Study + Sex"="grp_study_sex",
 "Study + Time + Dose"="grp_study_time_dose","Study + Time"="grp_study_time",
 "Study + Dose + Generation"="grp_study_dose_generation","Study + Generation"="grp_study_generation",
 "Full (Study + Sex + Dose)"="grp_full")
PALETTES <- c("Default ggplot2"="default","Colorblind-safe (Okabe-Ito)"="okabe",
 "Brewer: Set1"="Set1","Brewer: Set2"="Set2","Brewer: Dark2"="Dark2","Brewer: Paired"="Paired",
 "Viridis"="viridis","Plasma"="plasma")
okabe_ito <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#000000")
ui <- fluidPage(tags$head(tags$style(HTML("
 .panel-heading { padding: 6px 10px; }.panel-title { font-size: 12px; font-weight: bold; }
 .panel-body { padding: 8px 10px; }.panel { margin-bottom: 4px; }
 .form-group { margin-bottom: 6px; }label { font-size: 12px; margin-bottom: 1px; }
 .shiny-input-container { margin-bottom: 4px; }.panel-title .glyphicon { margin-right: 4px; }
 "))),titlePanel("Serum Concentration Plotter"),sidebarLayout(sidebarPanel(width=3,
 bsCollapse(id="sidebar_panels",open=c("Filters","Axes"),multiple=TRUE,
 bsCollapsePanel(title=tagList(icon("filter"),"Data Filters"),value="Filters",
 selectInput("study","Study:",choices=sort(unique(df$study_name)),multiple=TRUE),
 uiOutput("generation_ui"),uiOutput("sex_ui"),uiOutput("time_ui")),
 bsCollapsePanel(title=tagList(icon("arrows-alt"),"Axes & Scale"),value="Axes",
 selectInput("xcol","X-axis:",choices=x_value_cols,selected="dose_micrograms_kg_day"),
 selectInput("ycol","Y-axis:",choices=y_value_cols,selected="mean_serumconc_ng_ML"),
 selectInput("sdcol","Error bar (SD):",choices=c("None",sd_cols),selected="sd_serumconc"),
 radioButtons("yscale","Y scale:",choices=c("Linear"="linear","Log10"="log10"),inline=TRUE),
 radioButtons("xscale","X scale:",choices=c("Linear"="linear","Log10"="log10"),inline=TRUE),
 fluidRow(column(6,numericInput("ymin","Y min:",value=NA)),column(6,numericInput("ymax","Y max:",value=NA))),
 fluidRow(column(6,numericInput("xmin","X min:",value=NA)),column(6,numericInput("xmax","X max:",value=NA)))),
 bsCollapsePanel(title=tagList(icon("scissors"),"Split X-Axis (Dual Plot)"),value="XSplit",
 checkboxInput("use_xsplit","Enable split view",FALSE),
 fluidRow(column(6,numericInput("split_left_min","Left X min:",value=NA)),column(6,numericInput("split_left_max","Left X max:",value=NA))),
 fluidRow(column(6,numericInput("split_right_min","Right X min:",value=NA)),column(6,numericInput("split_right_max","Right X max:",value=NA))),
 sliderInput("split_width_left","Left panel width (%):",min=20,max=80,value=40,step=5)),
 bsCollapsePanel(title=tagList(icon("palette"),"Appearance"),value="Appearance",
 selectInput("legend_group","Colour grouping:",choices=LEGEND_CHOICES,selected="grp_study_dose_sex"),
 selectInput("palette","Colour palette:",choices=PALETTES,selected="okabe"),
 numericInput("pt_size","Point size:",value=3,min=1,max=10),
 selectInput("facet_by","Facet by:",choices=c("None","Study Name","Sex")),
 checkboxInput("show_lines","Connect points with lines",TRUE),
 selectInput("timepoint","Line grouping:",choices=c("collection_time","days_exposed")),
 selectInput("legend_position","Legend Position:",choices=c("Right"="right","Left"="left","Top"="top","Bottom"="bottom",
 "Inside Top-Right"="inside_tr","Inside Bottom-Right"="inside_br","Inside Top-Left"="inside_tl","Inside Bottom-Left"="inside_bl"),selected="right")),
 bsCollapsePanel(title=tagList(icon("download"),"Export"),value="Export",
 fluidRow(column(6,numericInput("dl_width","Width (in):",value=8)),column(6,numericInput("dl_height","Height (in):",value=5))),
 numericInput("dl_dpi","DPI:",value=300),fluidRow(column(6,downloadButton("dl_png","PNG")),column(6,downloadButton("dl_svg","SVG")))))),
 mainPanel(width=9,uiOutput("plot_ui"))))
server <- function(input,output,session) {
 output$generation_ui <- renderUI({req(input$study);avail <- df %>% filter(study_name %in% input$study) %>% pull(generation) %>% unique() %>% sort();selectInput("generation","Generation:",choices=avail,multiple=TRUE)})
 output$sex_ui <- renderUI({req(input$study,input$generation);avail <- df %>% filter(study_name %in% input$study,generation %in% input$generation) %>% pull(sex) %>% unique() %>% sort();selectInput("sex","Sex:",choices=avail,multiple=TRUE)})
 output$time_ui <- renderUI({req(input$study,input$generation,input$sex);avail <- df %>% filter(study_name %in% input$study,generation %in% input$generation,sex %in% input$sex) %>% pull(collection_time) %>% unique() %>% sort();selectInput("collection_time","Collection time:",choices=avail,multiple=TRUE)})
 filtered <- reactive({req(input$study,input$generation,input$sex,input$collection_time);df %>% filter(study_name %in% input$study,generation %in% input$generation,sex %in% input$sex,collection_time %in% input$collection_time)})
 build_plot <- function(data=NULL,xlim_low=NA,xlim_high=NA,show_legend=TRUE,legend_position="right") {
 if(is.null(data)) data <- filtered();validate(need(nrow(data)>0,"No data for selected filters."))
 x_lo <- if(!is.na(xlim_low)) xlim_low else input$xmin;x_hi <- if(!is.na(xlim_high)) xlim_high else input$xmax
 legend_var <- input$legend_group;n_groups <- dplyr::n_distinct(data[[legend_var]])
 tooltip_text <- paste0("Study: ",data$study_name,"<br>","Sex: ",data$sex,"<br>","Generation: ",data$generation,"<br>","Dose (mg/kg/day): ",data$dose_mg_kg_day,"<br>","Y = ",data[[input$ycol]],"<br>","SD = ",ifelse(input$sdcol=="None","N/A",data[[input$sdcol]]))
 p <- ggplot(data,aes(x=.data[[input$xcol]],y=.data[[input$ycol]],color=.data[[legend_var]],text=tooltip_text))+geom_point(size=input$pt_size)
 if(input$sdcol!="None") p <- p+geom_errorbar(aes(ymin=pmax(.data[[input$ycol]]-.data[[input$sdcol]],.Machine$double.eps),ymax=.data[[input$ycol]]+.data[[input$sdcol]]),width=.1)
 if(input$show_lines) {
 if(input$xcol=="days_exposed") p <- p+geom_line(aes(group=interaction(study_name,sex,generation,dose_mg_kg_day)))
 if(input$xcol %in% c("dose_mg_kg_day","dose_micrograms_kg_day")) p <- p+geom_line(aes(group=interaction(study_name,sex,generation,.data[[input$timepoint]])))
 }
 if(input$yscale=="log10") p <- p+scale_y_log10(limits=c(if(is.na(input$ymin)) NA else max(input$ymin,.Machine$double.eps),input$ymax)) else p <- p+scale_y_continuous(limits=c(input$ymin,input$ymax))
 if(input$xscale=="log10") p <- p+scale_x_log10(limits=c(x_lo,x_hi)) else p <- p+scale_x_continuous(limits=c(x_lo,x_hi))
 pal <- input$palette
 if(pal=="okabe") p <- p+scale_color_manual(values=rep_len(okabe_ito,n_groups)) else if(pal %in% c("viridis","plasma")) p <- p+scale_color_viridis_d(option=pal) else if(pal %in% rownames(brewer.pal.info)) {
 maxc <- brewer.pal.info[pal,"maxcolors"];base <- brewer.pal(min(n_groups,maxc),pal);if(n_groups>maxc) base <- colorRampPalette(base)(n_groups);p <- p+scale_color_manual(values=base)
 }
 if(input$facet_by=="Study Name") p <- p+facet_wrap(~study_name)
 if(input$facet_by=="Sex") p <- p+facet_wrap(~sex)
 if(!show_legend) p <- p+theme(legend.position="none") else {
 pos <- legend_position
 if(pos=="inside_tr") pos <- c(.95,.95);if(pos=="inside_br") pos <- c(.95,.05);if(pos=="inside_tl") pos <- c(.05,.95);if(pos=="inside_bl") pos <- c(.05,.05)
 p <- p+theme(legend.position=pos)
 };p+theme_bw(base_size=13)
 }
 output$plot_ui <- renderUI({if(input$use_xsplit) {lw <- input$split_width_left;rw <- 100-lw;fluidRow(column(12,div(style="display:flex;",div(style=paste0("width:",lw,"%"),plotlyOutput("plot_left",height="620px")),div(style=paste0("width:",rw,"%"),plotlyOutput("plot_right",height="620px")))))} else plotlyOutput("plot_single",height="650px")})
 output$plot_single <- renderPlotly({ggplotly(build_plot(show_legend=TRUE,legend_position=input$legend_position),tooltip="text")})
 output$plot_left <- renderPlotly({ggplotly(build_plot(xlim_low=input$split_left_min,xlim_high=input$split_left_max,show_legend=TRUE,legend_position=input$legend_position),tooltip="text")})
 output$plot_right <- renderPlotly({ggplotly(build_plot(xlim_low=input$split_right_min,xlim_high=input$split_right_max,show_legend=FALSE,legend_position=input$legend_position),tooltip="text")})
 output$dl_png <- downloadHandler(filename=function() paste0("serumconc_",Sys.Date(),".png"),content=function(file) ggsave(file,build_plot(),width=input$dl_width,height=input$dl_height,dpi=input$dl_dpi))
 output$dl_svg <- downloadHandler(filename=function() paste0("serumconc_",Sys.Date(),".svg"),content=function(file) ggsave(file,build_plot(),width=input$dl_width,height=input$dl_height,device="svg"))
}
shinyApp(ui,server)
