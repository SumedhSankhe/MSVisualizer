
##perhaps keep users in separate table

shinyServer(
    function(input, output, session) {
        countsData <- callModule(loadPanel, "loadPanel")
        callModule(heatmapPanel, "heatmapPanel", countsData)
        callModule(cosinePanel, "cosinePanel", countsData)
        #callModule(upsetPlotPanel, "upsetPlotPanel", countsData)
        #callModule(diffExprPanel, "diffExprPanel", countsData)
        #callModule(univariatePlotPanel, "univariatePanel", countsData)
        #callModule(dimReduction, "dimReduction", countsData)
    }
)
