diffExprPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Differential Expression Plots"),
    fluidRow(
      box( title = "Differential Expression", width = 4, status = "warning",
           tabBox(id = "dimred", height = "100%", width = "100%",
                  tabPanel("Volcano Plot",
                           uiOutput(ns("selectCondition1Cols")),
                           uiOutput(ns("selectCondition2Cols")),
                           #uiOutput(ns("classificationChoice")),
                           actionButton(ns("volcanoPlotButton"), 
                                        label = "Submit", 
                                        icon("paper-plane"), 
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           )
                  ),
                  tabPanel(tagList(shiny::icon("gear"), "Settings"),
                           numericInput(inputId = ns('pCutoff'), label = 'P-val Cutoff: ', value='1e-5'),
                           #numericInput(inputId = ns('AdjpCutoff'), label = '-Log10_Adj P-val Cutoff: ', value='2'),
                           numericInput(inputId = ns('FCcutoff'), label = 'Log2FC Cutoff: ', value='2.0'),
                           numericInput(inputId = ns('plot_width'), label = 'Plot Width: ', value='15'),
                           numericInput(inputId = ns('plot_height'), label = 'Plot Height: ', value='10'),
                           selectInput(inputId = ns('plot_type'),
                                       label = 'Plot Type: ',
                                       choices = c('svg', 'png', 'pdf')
                           )
                  )
           )
      ),
      box(
        status = "warning", width = 8,
        tabBox(id = "displayTab", height = "100%", width = "100%",
          tabPanel(title="Volcano Plot",
            (div(style='width:800px;overflow-x: scroll;height:800px;overflow-y: scroll;',
            shinycssloaders::withSpinner(
                    plotOutput(ns("volcanoPlot"), width = "150%", height = "600px"))
            )),
            HTML("<br>"),
            downloadButton(ns('downloadVolcanoPlot'), "Download Plot")
          ),
          tabPanel("Diff Expr Results",
                   dataTableOutput(ns("diffExprResults")),
                   HTML("<br>"),
                   downloadButton(ns("downloadDiffExprReport"), "Download csv")
          )#,
          # tabPanel("Gene Ontology",
          #          (div(style='width:800px;overflow-x: scroll;height:800px;overflow-y: scroll;',
          #               shinycssloaders::withSpinner(
          #                 plotOutput(ns("geneOntologyPlot"), width = "150%", height = "1200px"))
          #          ))
          # )
        )
      )
    )
  )
}

diffExprPanel <- function(input, output, session, getData) {
  ns <- session$ns
  
  getClassificationData <- reactive({
    data.frame(getData()[[2]])
  })
  
  getMatrixData <- reactive({
    data.frame(getData()[[1]])
  })
  
  ########################
  ##select numeric columns
  ########################
  getNumericCols <- reactive({
    data <- getMatrixData()
    cols <- colnames(data)
    classes <- sapply(data, class)
    univ <- names(classes[classes == "numeric"])
    cols <- cols[cols %in% univ]
    cols
  })
  
  output$selectCondition1Cols <- renderUI({
    cols <- getNumericCols()
    selectInput(ns("condition1samples"), "Condition1 Samples",
                choices = cols,
                multiple=TRUE,
                selected = NULL)
  })

  output$selectCondition2Cols <- renderUI({
    #req(input$condition1samples)
    cols <- getNumericCols()
    cols <- cols[! cols %in% input$condition1samples]
    selectInput(ns("condition2samples"), "Condition2 Samples",
                choices = cols,
                multiple=TRUE,
                selected = NULL)
  })
  
  # get_classification_choices <- reactive({
  #   return( names(getClassificationData())[-1] )
  # })
  # 
  # output$classificationChoice <- renderUI({
  #   selectInput(inputId = ns('classificationmethod'),
  #               label = 'Classification: ',
  #               choices = get_classification_choices()
  #   )
  # })
  
  # output$classificationOutput <- DT::renderDataTable({
  #   DT::datatable(data.frame(getMatrixData() %>% select(c(input$condition1samples, input$condition2samples))) , options = list(scrollX=TRUE, scrollCollapse=TRUE))
  # })

  # compute_deseq_results <- eventReactive( input$volcanoPlotButton, {
  #   req(input$condition1samples)
  #   req(input$condition2samples)
  #   req(input$classificationmethod)
  #   countTable <- data.frame(getMatrixData() %>% dplyr::select(all_of(c(input$condition1samples, input$condition2samples))), row.names=getMatrixData()[,1])
  #   colnames(countTable) <- gsub("-", ".", colnames(countTable))
  #   #------------ converting to counts for now, will delete later.
  #   countTable <- ceiling(countTable)
  #   #------------
  #   
  #   phenoTable <- getClassificationData()
  #   phenoTable$Sample <- gsub("-", ".", phenoTable$Sample)
  #   phenoTable$Condition <- gsub("-", ".", phenoTable$Condition)
  #   
  #   phenoTable <- subset(phenoTable, Sample %in% c(input$condition1samples, input$condition2samples))
  #   row.names(phenoTable) <- phenoTable$Sample
  #   browser()
  #   dds0 <- DESeqDataSetFromMatrix(countData = countTable, colData = phenoTable, design = as.formula(paste("~", input$classificationmethod)))
  #   dds.norm <-  estimateSizeFactors(dds0)
  #   # Performing estimation of dispersion parameter
  #   dds.disp <- estimateDispersions(dds.norm)
  #   alpha <- 0.0001
  #   waldTestResult <- nbinomWaldTest(dds.disp)
  #   resultDESeq2 <- results(waldTestResult, alpha=alpha, pAdjustMethod="BH")
  #   resultDESeq2 <- resultDESeq2[order(resultDESeq2$padj),]
  #   uniprotids <- sapply(rownames(resultDESeq2), function(x) unlist(strsplit(x, ";"))[1])
  #   resultDESeq2$GeneSymbol <- AnnotationDbi::mapIds(org.Hs.eg.db, uniprotids, "SYMBOL", "UNIPROT", multiVals="first")
  #   resultDESeq2
  # })
  
  # # Quantile normalisation : the aim is to give different distributions the
  # # same statistical properties
  # quantile_normalisation <- function(df){
  #   
  #   # Find rank of values in each column
  #   df_rank <- map_df(df,rank,ties.method="average")
  #   # Sort observations in each column from lowest to highest 
  #   df_sorted <- map_df(df,sort)
  #   # Find row mean on sorted columns
  #   df_mean <- rowMeans(df_sorted)
  #   
  #   # Function for substiting mean values according to rank 
  #   index_to_mean <- function(my_index, my_mean){
  #     return(my_mean[my_index])
  #   }
  #   
  #   # Replace value in each column with mean according to rank 
  #   df_final <- map_df(df_rank,index_to_mean, my_mean=df_mean)
  #   
  #   return(df_final)
  # }
  # 
  # # T-test function for multiple experiments
  # t_test <- function(dt,grp1,grp2){
  #   # Subset control group and convert to numeric
  #   x <- dt[grp1] %>% unlist %>% as.numeric()
  #   # Subset treatment group and convert to numeric
  #   y <- dt[grp2] %>% unlist %>% as.numeric()
  #   # Perform t-test using the mean of x and y
  #   result <- t.test(x, y)
  #   # Extract p-values from the results
  #   p_vals <- tibble(p_val = result$p.value)
  #   # Return p-values
  #   return(p_vals)
  # }
  
  compute_diffexpr_results <- eventReactive( input$volcanoPlotButton, {
    req(input$condition1samples)
    req(input$condition2samples)
    #req(input$classificationmethod)
    countTable <- data.frame(getMatrixData() %>% dplyr::select(all_of(c(input$condition1samples, input$condition2samples))), row.names=getMatrixData()[,1])
    colnames(countTable) <- gsub("-", ".", colnames(countTable))
    pheno_data_df <- data.frame(sample_names=c(input$condition1samples, input$condition2samples), condition=c(rep("condition1", length(input$condition1samples)), rep("condition2", length(input$condition2samples))))
    design = model.matrix(~condition, data=pheno_data_df)
    colnames(design) = c("condition1", "condition2")
    fit1  = lmFit(countTable, design)
    efit1 = eBayes(fit1)
    return( topTable(efit1, coef="condition2", genelist=row.names(countTable), n=Inf, sort.by="p") )
    # norm_counts_table <- countTable #%>% quantile_normalisation()
    # dat_pvals <- plyr::adply(norm_counts_table, 
    #                          .margins = 1, 
    #                          .fun = t_test, 
    #                          grp1 = input$condition1samples, 
    #                          grp2 = input$condition2samples)
    # 
    # dat_log2 <- norm_counts_table %>% log2()
    # dat_combine <- bind_cols(dat_log2, p_val=dat_pvals$p_val) %>% as.data.frame()
    # 
    # dat_combine$mean_control <- rowMeans(dat_combine[,input$condition1samples])
    # dat_combine$mean_treatment <- rowMeans(dat_combine[,input$condition2samples])
    # dat_combine$log2fc = dat_combine$mean_treatment - dat_combine$mean_control
    # dat_combine$padj <- p.adjust(dat_combine$p_val, method = "BH")
    # dat_combine$neg_log10_pval <- -1*log10(dat_combine$p_val)
    # dat_combine$neg_log10_adj_pval <- -1*log10(dat_combine$padj)
    # dat_combine$protid <- row.names(countTable)
    # uniprotids <- sapply(dat_combine$protid, function(x) unlist(strsplit(x, ";"))[1])
    # dat_combine$GeneSymbol <- AnnotationDbi::mapIds(org.Hs.eg.db, uniprotids, "SYMBOL", "UNIPROT", multiVals="first")
    # dat_combine[, c("protid", "GeneSymbol", "log2fc", "p_val", "neg_log10_pval", "padj", "neg_log10_adj_pval")]
  })
  
  gen_volcano_plot <- reactive({
    req(compute_diffexpr_results())
    res <- compute_diffexpr_results()
    # p <- ggplot(res, aes(x = log2fc, y =neg_log10_pval)) + 
    #   geom_point(size=0.5 )+
    #   theme_bw(base_size = 16) + # change theme
    #   xlab(expression("log2FC")) + # x-axis label
    #   ylab(expression(" -log10(P-value)")) + # y-axis label
    #   geom_vline(xintercept = c(-1*input$FCcutoff, input$FCcutoff), colour = "red") + # Add fold change cutoffs
    #   geom_hline(yintercept = input$pCutoff, colour = "red") + # Add significance cutoffs
    #   geom_vline(xintercept = 0, colour = "black") + # Add 0 lines
    #   scale_colour_gradient(low = "black", high = "black", guide = FALSE) +
    #   geom_text_repel(data=subset(res, abs(log2fc) > input$FCcutoff & neg_log10_pval > input$pCutoff),
    #                   aes( log2fc, neg_log10_pval ,label=GeneSymbol)) 
    # p <- EnhancedVolcano(res,
    #                 lab = rownames(res),
    #                 x = 'log2FoldChange',
    #                 y = 'pvalue',
    #                 pCutoff = input$pCutoff,
    #                 FCcutoff = input$FCcutoff,
    #                 drawConnectors = TRUE)
    p <- EnhancedVolcano(res,
                    lab = rownames(res),
                    x = 'logFC',
                    y = 'P.Value',
                    pCutoff = input$pCutoff,
                    FCcutoff = input$FCcutoff,
                    drawConnectors = TRUE)
    return(p)
  })
  
  output$volcanoPlot <- renderPlot({
    p <- gen_volcano_plot()
    p
  })
  
  # output$downloadVolcanoPlot <- downloadHandler(
  #   filename = "volcano_plot.svg",
  #   contentType = "image/svg",
  #   content = function(file) {
  #     svg(file, width = 15, height = 10)
  #     plot = gen_volcano_plot()
  #     print(plot)
  #     dev.off()
  #   }
  # )
  
  output$downloadVolcanoPlot <- downloadHandler(
    filename = function() {
      paste("volcano_plot", input$plot_type, sep=".")
    },
    content = function(file) {
      switch (input$plot_type,
              svg = {
                svg(file, width = input$plot_width, height = input$plot_height)
              },
              png = {
                png(file, width = input$plot_width, height = input$plot_height)
              },
              pdf = {
                pdf(file, width = input$plot_width, height = input$plot_height)
              }
      )
      plot = gen_volcano_plot()
      print(plot)
      dev.off()
    }
  )
  
  output$diffExprResults <- DT::renderDataTable({
    req(compute_diffexpr_results()) 
    #[, c("protid", "GeneSymbol", "log2fc", "p_val", "padj")]
    datatable(compute_diffexpr_results(),  filter = "top")
  }, server = FALSE)
  
  output$downloadDiffExprReport <- downloadHandler(
    filename = function() {
      "Differential_Expression.csv"
    },
    content = function(file) {
      diffexpr_resutls <- as.data.frame(compute_diffexpr_results())
      write.csv(diffexpr_resutls, file, row.names=TRUE, quote=FALSE)
    }
  )
  
  # get_go_mapping <- reactive({
  #   req(compute_diffexpr_results())
  #   res <- as.data.frame(compute_diffexpr_results())
  #   res$UniProtID <- rownames(res)
  #   datamart <- useMart(biomart="ENSEMBL_MART_ENSEMBL", dataset="hsapiens_gene_ensembl")
  #   
  #   all_genes_acc_ids <- res %>% dplyr::select(UniProtID) %>% unlist()
  #   all_genes_acc_ids <- sapply(all_genes_acc_ids, function(x) unlist(strsplit(x, ";"))[1])
  #   all_genes_annot <- getBM(mart=datamart, attributes=c("uniprotswissprot", "entrezgene_id"), filter="uniprotswissprot", values=all_genes_acc_ids, uniqueRows=TRUE)
  #   all_genes_annot <- all_genes_annot %>% filter(!is.na(entrezgene_id)) # Drop genes which cannot be mapped to a GO or Entrez-id
  #   
  #   diff_expr_gene_acc_ids <- res %>% filter(log2FoldChange > input$FCcutoff & padj < input$AdjpCutoff) %>% dplyr::select(UniProtID) %>% unlist()
  #   diff_expr_gene_acc_ids <- sapply(diff_expr_gene_acc_ids, function(x) unlist(strsplit(x, ";"))[1])
  #   sig_genes_annot <- getBM(mart=datamart, attributes=c("uniprotswissprot", "entrezgene_id"), filter="uniprotswissprot", values=diff_expr_gene_acc_ids, uniqueRows=TRUE)
  #   sig_genes_annot <- sig_genes_annot %>% filter(!is.na(entrezgene_id)) # Drop genes which cannot be mapped to a GO or Entrez-id 
  #   return( list(all_genes = all_genes_annot, sig_genes = sig_genes_annot) )
  # })
  # 
  # gen_bar_plot <- function(df){
  #   p <- ggplot(df, aes(x=reorder(Description, Count), y=Count, fill=p.adjust)) + 
  #     geom_bar(stat="identity", alpha=0.75) +  
  #     geom_text(aes(label=Count), size=5, hjust=-0.25) +
  #     scale_fill_gradientn(colours = c(muted("blue"), "white", muted("red"))) +
  #     xlab("") + 
  #     ylab("") +
  #     coord_flip() +
  #     theme(text = element_text(size = 18))
  #   return(p)
  # }
  # 
  # output$geneOntologyPlot <- renderPlot({
  #   annots <- get_go_mapping()
  #   
  #   ggo_cc <- enrichGO(
  #                  gene  = as.character(annots$sig_genes$entrezgene_id),
  #                  universe = as.character(annots$all_genes$entrezgene_id),
  #                  OrgDb    = org.Hs.eg.db,
  #                  ont      = "CC",
  #                  pAdjustMethod = "BH",
  #                  pvalueCutoff  = 0.01,
  #                  qvalueCutoff  = 0.05,
  #                  readable = TRUE)
  #   ggo_cc <- ggo_cc %>% arrange(desc(Count)) #%>% head(10)
  #   p1 <- gen_bar_plot(ggo_cc)
  # 
  #   ggo_bp <- enrichGO(
  #                  gene  = as.character(annots$sig_genes$entrezgene_id),
  #                  universe = as.character(annots$all_genes$entrezgene_id),
  #                  OrgDb    = org.Hs.eg.db,
  #                  ont      = "BP",
  #                  pAdjustMethod = "BH",
  #                  pvalueCutoff  = 0.01,
  #                  qvalueCutoff  = 0.05,
  #                  readable = TRUE)
  #   ggo_bp <- ggo_bp %>% arrange(desc(Count)) #%>% head(10)
  #   p2 <- gen_bar_plot(ggo_bp)
  #   
  #   ggo_mf <- enrichGO(
  #                 gene  = as.character(annots$sig_genes$entrezgene_id),
  #                 universe = as.character(annots$all_genes$entrezgene_id),
  #                 OrgDb    = org.Hs.eg.db,
  #                 ont      = "MF",
  #                 pAdjustMethod = "BH",
  #                 pvalueCutoff  = 0.01,
  #                 qvalueCutoff  = 0.05,
  #                 readable = TRUE)
  #   
  #   ggo_mf <- ggo_mf %>% arrange(desc(Count)) #%>% head(10)
  #   p3 <- gen_bar_plot(ggo_mf)
  #   
  #   edo <- enrichDGN(annots$sig_genes$entrezgene_id)
  #   p4 <- barplot(edo, showCategory=20)
  #   
  #   p <- plot_grid(
  #     p1, p2, p3, p4,
  #     labels = c("CC", "BP", "MF", "DGN"), 
  #     ncol = 1
  #   )
  #   p
  #   # edo <- enrichDGN(annots$entrezgene_id[!is.na(annots$entrezgene_id)], readable = TRUE)
  #   # barplot(edo, showCategory=20) 
  # 
  #   #GeneOntologyObj <- GetProteinGOInfo(diff_expr_gene_acc_ids)
  #   #PlotGOBiological(GeneOntologyObj, Top = 10)
  # })
  
}
