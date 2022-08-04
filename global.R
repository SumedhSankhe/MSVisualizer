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
#library(dendextend)
#library(caret)
#library(ComplexUpset)
#library(EnhancedVolcano)
#library(lsa)
#library(Rtsne)
#library(umap)
#library(qvalue)
#library(stringr)
#library(factoextra)
#library(scales)
#library(dplyr)
#library(limma)
#library("FactoMineR")
#library("org.Hs.eg.db")
#library("cowplot")
#library(biomaRt)
#library(clusterProfiler)
#library(enrichplot)
#library(DOSE)
#library(ggiraph)

source("R/dimReduction.R")
source("R/pcaPanel.R")
source("R/tsnePanel.R")
source("R/umapPanel.R")
source("R/loadPanel.R")
source("R/univariatePanel.R")
source("R/upsetPlotPanel.R")
source("R/cosinePanel.R")
source("R/upsetPlotPanel.R")
source("R/diffExprPanel.R")

shinyjs::useShinyjs()
graphics.off()

create_annotation_col <- function(ann, col){
  ann[, Sample := gsub('-','.', Sample)]
  sample_col <- ann[, .(get(col))]
  names(sample_col) <- col
  row.names(sample_col) <- ann[,Sample]
  sample_col
}
