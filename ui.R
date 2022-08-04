
css <<- "
.chart-wrapper {
  overflow-x: scroll;
}
.shiny-progress .progress-text{
background-color: #FF0000;
}
"

sidebar <- dashboardSidebar(
    width = 150,
    sidebarMenu(
        menuItem(tagList(icon("tachometer-alt")," Dashboard"), tabName = "home"),
        menuItem(tagList(icon("upload")," Load"), tabName = "load"),
        menuItem(tagList(icon("fire")," Heat Map"), tabName = "heatmap"),
        menuItem(tagList(icon("chart-line")," Cosine Plot"), tabName = "cosine"),
        menuItem(tagList(icon("chart-bar")," Upset Plot"), tabName = "upsetplot"),
        menuItem(tagList(icon("chart-area")," Volcano Plot"), tabName = "diffExpr"),
        menuItem(tagList(icon("boxes")," Univariate Plots"), tabName = "univariate"),
        menuItem(tagList(icon("chart-pie")," Dim Reduction"), tabName="dimred")
    )
)

bodyHome <- tabItem(tabName = "home", homePanelUI("home"))
bodyLoad <- tabItem(tabName = "load", loadPanelUI("loadPanel"))
bodyHeatmap <- tabItem(tabName = "heatmap", heatmapPanelUI("heatmapPanel"))
bodyCosine <- tabItem(tabName = "cosine", cosinePanelUI("cosinePanel"))
bodyUpsetPlot <- tabItem(tabName = "upsetplot", upsetPlotPanelUI("upsetPlotPanel"))
bodyDiffExpr <- tabItem(tabName = "diffExpr", diffExprPanelUI("diffExprPanel"))
#bodyUnivariate <- tabItem(tabName = "univariate", univariatePlotPanelUI("univariatePanel"))
#bodyDimRed <- tabItem(tabName = "dimred",  dimReductionUI("dimReduction"))

shinyUI(dashboardPage(
    title="MassSpec Dashboard",
    skin = "black",
    dashboardHeader(
        # Set height of dashboardHeader
        #title = tags$a(href='http://rssshiny.roche.com/users/paladugs/qbcExplorerApp/',
        #               tags$img(src='logo-qbc3.jpg', height = "90%")),
        title = tags$a(href='http://localhost/',
                       tags$img(src='MassSpec.png', height = "90%")),
        titleWidth = 150
    ),
    sidebar,
    dashboardBody(
        tabItems(
            bodyHome,
            #Upload Data
            bodyLoad,
            #Heatmap
            bodyHeatmap,
            #CosineSimilarity
            bodyCosine,
            #Upset Plot
            bodyUpsetPlot,
            #Differential Expression
            bodyDiffExpr
            #Univariate
            #bodyUnivariate,
            #Dimensionality Reduction
            #bodyDimRed
        )
    )
))
