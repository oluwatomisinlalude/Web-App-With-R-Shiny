library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(DT)
library(forecast)
library(cluster)
library(ggthemes)
library(leaflet.extras)

# Load Dataset
data <- read.csv("superstore_sales.csv")

# Data Preprocessing
data$Order.Date <- as.Date(data$Order.Date, "%Y-%m-%d")
data$Year <- format(data$Order.Date, "%Y")
data$Month <- format(data$Order.Date, "%Y-%m")

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Global Superstore Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Insights", tabName = "insights", icon = icon("chart-line")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("calendar-alt")),
      menuItem("Geographic Analysis", tabName = "maps", icon = icon("globe")),
      menuItem("Data View", tabName = "data", icon = icon("table")),
      menuItem("Advanced Analysis", tabName = "advanced", icon = icon("tools"))
    ),
    hr(),
    sliderInput("salesRange", "Filter by Sales Range:",
                min = min(data$Sales), max = max(data$Sales),
                value = c(min(data$Sales), max(data$Sales))),
    selectInput("regionSelect", "Select Region:",
                choices = c("All", unique(data$Region)), selected = "All"),
    selectInput("categorySelect", "Select Category:",
                choices = c("All", unique(data$Category)), selected = "All")
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            title = "Key Metrics",
            width = 12,
            valueBoxOutput("totalSales"),
            valueBoxOutput("totalProfit"),
            valueBoxOutput("topCategory")
          )
        ),
        fluidRow(
          box(
            status = "primary",
            width = 6,
            plotlyOutput("profitVsSales")
          ),
          box(
            status = "primary",
            width = 6,
            plotlyOutput("salesByCategory")
          )
        )
      ),
      
      # Insights Tab
      tabItem(
        tabName = "insights",
        fluidRow(
          box(
            title = "Profit by Region",
            status = "primary",
            width = 6,
            plotlyOutput("profitRegion")
          ),
          box(
            title = "Category Performance",
            status = "primary",
            width = 6,
            plotlyOutput("categoryPerformance")
          )
        ),
        fluidRow(
          box(
            title = "Correlation Matrix",
            status = "primary",
            width = 12,
            plotlyOutput("correlationMatrix")
          )
        )
      ),
      
      # Forecasting Tab
      tabItem(
        tabName = "forecasting",
        fluidRow(
          box(
            title = "Region-wise Sales Forecast",
            status = "primary",
            width = 6,
            plotlyOutput("regionSalesForecast")
          ),
          box(
            title = "Category-wise Sales Forecast",
            status = "primary",
            width = 6,
            plotlyOutput("categorySalesForecast")
          )
        ),
        fluidRow(
          box(
            title = "Sub-Category-wise Sales Forecast",
            status = "primary",
            width = 6,
            plotlyOutput("subCategorySalesForecast")
          ),
          box(
            title = "Yearly Sales Forecast",
            status = "primary",
            width = 6,
            plotlyOutput("yearlySalesForecast")
          )
        )
      ),
      # Maps Tab
      tabItem(
        tabName = "maps",
        fluidRow(
          box(
            title = "Sales by Region",
            status = "primary",
            width = 12,
            leafletOutput("salesMap")
          ),
          box(
            title = "Sales Heatmap",
            status = "primary",
            width = 12,
            leafletOutput("salesHeatmap")
          )
        )
      ),
      
      # Data Tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Interactive Data Table",
            status = "primary",
            width = 12,
            DTOutput("dataTable")
          )
        )
      ),
      
      # Advanced Analysis Tab
      tabItem(
        tabName = "advanced",
        fluidRow(
          box(
            title = "Clustering Analysis",
            status = "primary",
            width = 12,
            plotlyOutput("clusteringAnalysis")
          )
        ),
        fluidRow(
          box(
            title = "Profit Prediction",
            status = "primary",
            width = 12,
            plotlyOutput("profitPrediction")
          )
        ),
        fluidRow(
          box(
            title = "Sub-Category Sales Forecast",
            status = "primary",
            width = 12,
            plotlyOutput("subCategoryForecast")
          )
        ),
        fluidRow(
          box(
            title = "Actionable Insights",
            status = "primary",
            width = 12,
            uiOutput("actionableInsights")
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Filtered Data
  filteredData <- reactive({
    data %>%
      filter(Sales >= input$salesRange[1],
             Sales <= input$salesRange[2],
             if (input$regionSelect != "All") Region == input$regionSelect else TRUE,
             if (input$categorySelect != "All") Category == input$categorySelect else TRUE)
  })
  
  # Key Metrics
  output$totalSales <- renderValueBox({
    valueBox(
      formatC(sum(filteredData()$Sales), format = "d", big.mark = ","),
      subtitle = "Total Sales",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$totalProfit <- renderValueBox({
    valueBox(
      formatC(sum(filteredData()$Profit), format = "d", big.mark = ","),
      subtitle = "Total Profit",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$topCategory <- renderValueBox({
    topCat <- filteredData() %>% group_by(Category) %>% 
      summarise(TotalSales = sum(Sales)) %>% 
      arrange(desc(TotalSales)) %>% 
      slice(1)
    valueBox(
      topCat$Category,
      subtitle = "Top Performing Category",
      icon = icon("trophy"),
      color = "yellow"
    )
  })
  
  # Profit vs. Sales
  output$profitVsSales <- renderPlotly({
    gg <- ggplot(filteredData(), aes(x = Sales, y = Profit)) +
      geom_point(alpha = 0.6, color = "darkorange") +
      theme_minimal() +
      labs(
        title = "Profit vs. Sales",
        x = "Sales",
        y = "Profit"
      )
    
    ggplotly(gg)
  })
  
  # Category Contribution to Sales
  output$salesByCategory <- renderPlotly({
    categoryData <- filteredData() %>%
      group_by(Category) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE))
    
    plot_ly(
      categoryData,
      labels = ~Category,
      values = ~TotalSales,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value+percent"
    ) %>%
      layout(title = "Category Contribution to Total Sales")
  })
  
  # Profit by Region
  output$profitRegion <- renderPlotly({
    region <- filteredData() %>% group_by(Region) %>% 
      summarise(TotalProfit = sum(Profit))
    gg <- ggplot(region, aes(x = reorder(Region, TotalProfit), y = TotalProfit, fill = Region)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Profit by Region", x = "Region", y = "Total Profit")
    ggplotly(gg)
  })
  
  # Category Performance
  output$categoryPerformance <- renderPlotly({
    category <- filteredData() %>% group_by(Category) %>% 
      summarise(TotalSales = sum(Sales))
    gg <- ggplot(category, aes(x = Category, y = TotalSales, fill = Category)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Category Performance", x = "Category", y = "Total Sales")
    ggplotly(gg)
  })
  
  # Correlation Matrix
  output$correlationMatrix <- renderPlotly({
    corrData <- filteredData() %>% select(Sales, Profit, Quantity)
    corr <- cor(corrData, use = "complete.obs")
    plot_ly(z = corr, type = "heatmap", x = colnames(corr), y = rownames(corr))
  })
  
  # Sales Forecast
  output$regionSalesForecast <- renderPlotly({
    regionData <- filteredData() %>%
      group_by(Region, Month) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(Region)) %>%
      arrange(Month)
    
    if (nrow(regionData) == 0) return(NULL)
    
    tsRegionSales <- ts(regionData$TotalSales, frequency = 12)  # Assuming monthly data
    regionForecast <- forecast(auto.arima(tsRegionSales), h = 12)  # Forecasting next 12 months
    
    plot_ly(x = time(regionForecast$mean), y = regionForecast$mean, type = 'scatter', mode = 'lines', name = 'Forecast') %>%
      layout(title = "Region-wise Sales Forecast", xaxis = list(title = "Month"), yaxis = list(title = "Sales"))
  })
  
  output$categorySalesForecast <- renderPlotly({
    categoryData <- filteredData() %>%
      group_by(Category, Month) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(Category)) %>%
      arrange(Month)
    
    if (nrow(categoryData) == 0) return(NULL)
    
    tsCategorySales <- ts(categoryData$TotalSales, frequency = 12)  # Assuming monthly data
    categoryForecast <- forecast(auto.arima(tsCategorySales), h = 12)  # Forecasting next 12 months
    
    plot_ly(x = time(categoryForecast$mean), y = categoryForecast$mean, type = 'scatter', mode = 'lines', name = 'Forecast') %>%
      layout(title = "Category-wise Sales Forecast", xaxis = list(title = "Month"), yaxis = list(title = "Sales"))
  })
  
  output$subCategorySalesForecast <- renderPlotly({
    subCategoryData <- filteredData() %>%
      group_by(Sub.Category, Month) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(Sub.Category)) %>%
      arrange(Month)
    
    if (nrow(subCategoryData) == 0) return(NULL)
    
    tsSubCategorySales <- ts(subCategoryData$TotalSales, frequency = 12)  # Assuming monthly data
    subCategoryForecast <- forecast(auto.arima(tsSubCategorySales), h = 12)  # Forecasting next 12 months
    
    plot_ly(x = time(subCategoryForecast$mean), y = subCategoryForecast$mean, type = 'scatter', mode = 'lines', name = 'Forecast') %>%
      layout(title = "Sub-Category-wise Sales Forecast", xaxis = list(title = "Month"), yaxis = list(title = "Sales"))
  })
  
  output$yearlySalesForecast <- renderPlotly({
    yearlySales <- filteredData() %>%
      group_by(Year) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE), .groups = "drop")
    
    if (nrow(yearlySales) == 0) return(NULL)
    
    tsYearlySales <- ts(yearlySales$TotalSales, frequency = 1)  # Assuming yearly data
    yearlyForecast <- forecast(auto.arima(tsYearlySales), h = 5)  # Forecasting next 5 years
    
    plot_ly(x = time(yearlyForecast$mean), y = yearlyForecast$mean, type = 'scatter', mode = 'lines', name = 'Forecast') %>%
      layout(title = "Yearly Sales Forecast", xaxis = list(title = "Year"), yaxis = list(title = "Sales"))
  })
  
  # Sales Map
  
  output$salesMap <- renderLeaflet({
    leaflet(data = filteredData()) %>%
      addTiles() %>%
      addCircles(
        lng = ~Longitude, lat = ~Latitude,
        radius = ~Sales / 1000,
        color = "blue", fill = TRUE, fillOpacity = 0.5,
        popup = ~paste0(
          "<strong>Region:</strong> ", Region,
          "<br><strong>Total Sales:</strong> $", round(Sales, 2),
          "<br><strong>Total Profit:</strong> $", round(Profit, 2)
        )
      ) %>%
      addLegend(
        "bottomright", 
        pal = colorNumeric(palette = "Blues", domain = filteredData()$Sales),
        values = ~Sales,
        title = "Total Sales"
      )
  })
  
  # Sales Heatmap
  
  output$salesHeatmap <- renderLeaflet({
    heatmapData <- filteredData() %>%
      group_by(Latitude, Longitude) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE), .groups = "drop")
    
    # Remove invalid or zero sales data
    heatmapData <- heatmapData %>%
      filter(!is.na(Latitude) & !is.na(Longitude) & TotalSales > 0)
    
    if (nrow(heatmapData) == 0) {
      leaflet() %>%
        addTiles() %>%
        addPopups(
          lng = 0, lat = 0,
          popup = "No data available for heatmap."
        )
    } else {
      # Determine maximum sales for scaling
      max_sales <- max(heatmapData$TotalSales, na.rm = TRUE)
      
      # Render heatmap
      leaflet(heatmapData) %>%
        addTiles() %>%
        setView(
          lng = mean(heatmapData$Longitude, na.rm = TRUE),
          lat = mean(heatmapData$Latitude, na.rm = TRUE),
          zoom = 5
        ) %>%
        addHeatmap(
          lng = ~Longitude, lat = ~Latitude, intensity = ~TotalSales,
          blur = 25, radius = 20, max = max_sales,
          gradient = c("blue", "green", "yellow", "red")
        )
    }
  })
  
  # Advanced Analysis
  
  # Clustering Analysis
  output$clusteringAnalysis <- renderPlotly({
    clusterData <- filteredData() %>%
      select(Sales, Profit) %>%
      na.omit()
    clusterDataScaled <- scale(clusterData)
    kmeansResult <- kmeans(clusterDataScaled, centers = 3)
    clusterData$Cluster <- as.factor(kmeansResult$cluster)
    
    gg <- ggplot(clusterData, aes(x = Sales, y = Profit, color = Cluster)) +
      geom_point(size = 3, alpha = 0.8) +
      theme_minimal() +
      labs(title = "Clustering Analysis (K-Means)", x = "Sales", y = "Profit")
    ggplotly(gg)
  })
  
  # Profit Prediction
  output$profitPrediction <- renderPlotly({
    lmModel <- lm(Profit ~ Sales + Quantity, data = filteredData())
    predictionData <- filteredData()
    predictionData$PredictedProfit <- predict(lmModel, newdata = filteredData())
    
    gg <- ggplot(predictionData, aes(x = Sales, y = Profit)) +
      geom_point(color = "blue", alpha = 0.6) +
      geom_line(aes(y = PredictedProfit), color = "red") +
      theme_minimal() +
      labs(title = "Profit Prediction", x = "Sales", y = "Profit")
    ggplotly(gg)
  })
  
  # Sub-Category Sales Forecast
  output$subCategoryForecast <- renderPlotly({
    subCategorySales <- filteredData() %>%
      group_by(Sub.Category) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      arrange(desc(TotalSales)) %>%
      slice(1)
    forecastData <- filteredData() %>%
      filter(Sub.Category == subCategorySales$Sub.Category) %>%
      group_by(Order.Date) %>%
      summarise(DailySales = sum(Sales, na.rm = TRUE))
    
    tsData <- ts(forecastData$DailySales, frequency = 365)
    forecastResult <- forecast(auto.arima(tsData), h = 30)
    plot_ly() %>%
      add_lines(x = time(forecastResult$fitted), y = forecastResult$fitted, name = "Fitted") %>%
      add_lines(x = time(forecastResult$mean), y = forecastResult$mean, name = "Forecast") %>%
      layout(title = paste("Sales Forecast for Sub-Category:", subCategorySales$Sub.Category))
  })
  
  #Data View
  
  # Filtered Data
  filteredData <- reactive({
    data %>%
      filter(Sales >= input$salesRange[1],
             Sales <= input$salesRange[2],
             if (input$regionSelect != "All") Region == input$regionSelect else TRUE,
             if (input$categorySelect != "All") Category == input$categorySelect else TRUE)
  })
  
  # Interactive Data Table
  output$dataTable <- renderDT({
    df <- filteredData()
    
    # Check if there's any data to show
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No data to display"), options = list(scrollX = TRUE)))
    }
    
    datatable(df, options = list(scrollX = TRUE))
  })
  
  # Actionable Insights
  output$actionableInsights <- renderUI({
    insights <- paste(
      "1. The top-performing region is ", filteredData() %>%
        group_by(Region) %>%
        summarise(TotalSales = sum(Sales)) %>%
        arrange(desc(TotalSales)) %>%
        slice(1) %>%
        pull(Region), ".",
      "2. The most profitable sub-category is ", filteredData() %>%
        group_by(Sub.Category) %>%
        summarise(TotalProfit = sum(Profit)) %>%
        arrange(desc(TotalProfit)) %>%
        slice(1) %>%
        pull(Sub.Category), ".",
      "3. Discounts significantly affect sales volume.",
      sep = "\n"
    )
    HTML(insights)
  })
}

# Run the app
shinyApp(ui, server)