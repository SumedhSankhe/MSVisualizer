library(shiny)
library(shinydashboard)
library(plotly)
library(shinyBS)
library(DT)
library(data.table)
library(shinyjs)
library(readxl)
library(shinycssloaders)
library(colourpicker)

#library(ggplot2)
#library(svglite)
#library(caret)
#library(qvalue)
#library(factoextra)
#library(scales)
#library("FactoMineR")
#library("org.Hs.eg.db")
#library("cowplot")
#library(biomaRt)
#library(clusterProfiler)
#library(enrichplot)
#library(DOSE)

source("R/loadPanel.R")
source("R/heatmapPanel.R")
source("R/cosinePanel.R")
source("R/upsetPlotPanel.R")
source("R/diffExprPanel.R")
source("R/univariatePanel.R")
#source("R/dimReduction.R")
#source("R/pcaPanel.R")
#source("R/tsnePanel.R")
#source("R/umapPanel.R")


shinyjs::useShinyjs()
graphics.off()

create_annotation_col <- function(ann, col){
  ann[, Sample := gsub('-','.', Sample)]
  sample_col <- ann[, .(get(col))]
  names(sample_col) <- col
  row.names(sample_col) <- ann[,Sample]
  sample_col
}

get_upset_by_sample <- function(matrix_stats) {
  dt <- rapply(matrix_stats, floor, classes = 'numeric', how = 'replace')
  idx <- which(names(dt) == 'UniProtID')
  dt[, names(dt)[-idx] := lapply(.SD, function(x) as.integer(x!=0)), .SDcols = seq(names(dt))[-idx]]
  dt
}

get_upset_by_group <- function(dt, dt2, group) {
  dt$Sample <- gsub("-", ".", dt$Sample)
  groups <- unique(dt[,get(group)])
  upset_by_sample <- get_upset_by_sample(matrix_stats = dt2)

  dt_list <- lapply(groups, function(x){
    col_names <- dt[get(group) == x, Sample]
    ubg <- upset_by_sample[, rowSums(.SD), .SDcols = col_names, by = UniProtID]
    setkeyv(ubg, 'UniProtID')
    ubg[, V1:= ifelse(V1>0,1,0)]
    names(ubg)[2] <- x
    ubg
  })

  upset_by_group <- Reduce(function(...) merge(..., all = TRUE), dt_list)
  upset_by_group
}


