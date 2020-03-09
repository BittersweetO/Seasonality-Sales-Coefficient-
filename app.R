
library(shiny)
library(shinydashboard)
library(RODBC)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)

SQLcon64 <- odbcDriverConnect('driver={SQL Server}; server=s-kv-center-s64')
EKT <- sqlQuery(SQLcon64, paste('SELECT * FROM [SILPOAnalitic].[dbo].[Bittersweet_Seasonality_EKT]'))
EKT2017 <- sqlQuery(SQLcon64, 'SELECT * FROM [SILPOAnalitic].[dbo].[Bittersweet_Seasonality_EKT2017]')
EKT2018 <- sqlQuery(SQLcon64, 'SELECT * FROM [SILPOAnalitic].[dbo].[Bittersweet_Seasonality_EKT2018]')
EKT2019 <- sqlQuery(SQLcon64, 'SELECT * FROM [SILPOAnalitic].[dbo].[Bittersweet_Seasonality_EKT2019]')
Commodity <- sqlQuery(SQLcon64, 'SELECT * FROM [SILPOAnalitic].[dbo].[Bittersweet_Seasonality_CommodityGroup]')
Commodity2017 <- sqlQuery(SQLcon64, 'SELECT * FROM [SILPOAnalitic].[dbo].[Bittersweet_Seasonality_CommodityGroup2017]')
Commodity2018 <- sqlQuery(SQLcon64, 'SELECT * FROM [SILPOAnalitic].[dbo].[Bittersweet_Seasonality_CommodityGroup2018]')
Commodity2019 <- sqlQuery(SQLcon64, 'SELECT * FROM [SILPOAnalitic].[dbo].[Bittersweet_Seasonality_CommodityGroup2019]')
Classifier <- sqlQuery(SQLcon64, "SELECT LagerID, ClassifierID, ClassifierName, LinkCommodityGroupLagerSapClassifier.commodityGroupId, ListAssortmentCommodityGroups.commodityGroupName
                  								                        FROM [SILPOAnalitic].[dbo].[UL_data_Lagers] with (nolock)
                  								                        JOIN [MasterData].[dbo].[LinkCommodityGroupLagerSapClassifier] with (nolock) on ClassifierID = lagerSapClassifierId
                  								                        JOIN [MasterData].[dbo].[ListAssortmentCommodityGroups] with(nolock) on LinkCommodityGroupLagerSapClassifier.commodityGroupId =  ListAssortmentCommodityGroups.commodityGroupId
                  								                        WHERE disableRow = 0
                  								                        ORDER by LagerID")
odbcClose(SQLcon64)

ui <- dashboardPage(
  
  
  dashboardHeader(title =  "Seasonality"),
  
  dashboardSidebar(
    numericInput(inputId = "Lager", 
                 label = "Put your LagerID!", 
                 value = 1) 
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel( title = "EKT",
                plotlyOutput(outputId = "EKT"),
                plotlyOutput(outputId = "EKTyear")
                
      ),
      tabPanel( title = "CommodityGroup",
                plotlyOutput(outputId = "CommodityGroup"),
                plotlyOutput(outputId = "CommodityGroupYear")
      )
      
    )
  )
  
)

server <- function(input, output) {
  output$EKT <- renderPlotly({
    ID <- as.character(Classifier$ClassifierID[Classifier$LagerID == input$Lager])
    NAME <- Classifier$ClassifierName[Classifier$LagerID == input$Lager]
    p <- EKT %>%
      gather("Week", "Coefficient", -EKTid) %>%
      filter(EKTid == ID) %>%
      mutate(Week = as.numeric(Week)) %>%
      ggplot(aes(x = Week, y = Coefficient)) + geom_line()
    ggplotly(p) %>% layout(title = print(NAME), yaxis = list(title = 'Coefficient'), xaxis = list(title = 'Week'))
  })
  output$EKTyear <- renderPlotly({
    ID <- as.character(Classifier$ClassifierID[Classifier$LagerID == input$Lager])
    NAME <- Classifier$ClassifierName[Classifier$LagerID == input$Lager]
    p2019 <- EKT2019 %>%
      gather("Week", "Coefficient", -EKTid) %>%
      filter(EKTid == ID) %>%
      mutate(Week = as.numeric(Week)) 
    p2018 <- EKT2018 %>%
      gather("Week", "Coefficient", -EKTid) %>%
      filter(EKTid == ID) %>%
      mutate(Week = as.numeric(Week)) 
    p2017 <- EKT2017 %>%
      gather("Week", "Coefficient", -EKTid) %>%
      filter(EKTid == ID) %>%
      mutate(Week = as.numeric(Week)) 
    pp <- rbind(p2017, p2018, p2019)  
    pl <- ggplot() + geom_line(data = p2017, aes( x = Week, y = Coefficient, color='2017')) + geom_line(data = p2018, aes( x = Week, y = Coefficient, color = "2018")) + geom_line(data = p2019, aes( x = Week, y = Coefficient, color = "2019"))
    ggplotly(pl) %>% layout( yaxis = list(title = 'Coefficient'), xaxis = list(title = 'Week'))
    
  })
  output$CommodityGroup <- renderPlotly({
    ID <- as.character(Classifier$commodityGroupId[Classifier$LagerID == input$Lager])
    NAME <- Classifier$commodityGroupName[Classifier$LagerID == input$Lager]
    p <- Commodity %>%
      gather("Week", "Coefficient", -GroupID) %>%
      filter(GroupID == ID) %>%
      mutate(Week = as.numeric(Week)) %>%
      ggplot(aes(x = Week, y = Coefficient)) + geom_line()
    ggplotly(p) %>% layout(title = print(NAME), yaxis = list(title = 'Coefficient'), xaxis = list(title = 'Week'))
  })
  output$CommodityGroupYear <- renderPlotly({
    ID <- as.character(Classifier$commodityGroupId[Classifier$LagerID == input$Lager])
    NAME <- Classifier$commodityGroupName[Classifier$LagerID == input$Lager]
    p2019 <- Commodity2019 %>%
      gather("Week", "Coefficient", -GroupID) %>%
      filter(GroupID == ID) %>%
      mutate(Week = as.numeric(Week)) 
    p2018 <- Commodity2018 %>%
      gather("Week", "Coefficient", -GroupID) %>%
      filter(GroupID == ID) %>%
      mutate(Week = as.numeric(Week))
    p2017 <- Commodity2017 %>%
      gather("Week", "Coefficient", -GroupID) %>%
      filter(GroupID == ID) %>%
      mutate(Week = as.numeric(Week)) 
    
    pp <- rbind(p2017, p2018, p2019)  
    pl <- ggplot() + geom_line(data = p2017, aes( x = Week, y = Coefficient, color='2017')) + geom_line(data = p2018, aes( x = Week, y = Coefficient, color = "2018")) + geom_line(data = p2019, aes( x = Week, y = Coefficient, color = "2019"))
    ggplotly(pl) %>% layout( yaxis = list(title = 'Coefficient'), xaxis = list(title = 'Week'))
  })
  
  
}

shinyApp(ui = ui, server = server)
