library(shiny)
library(leaflet)
library(RColorBrewer)
library(sf)
library(data.table)
library(stringr)
library(readr)
library(foreign)
library(readxl)
library(haven)
library(tidyverse)
library(htmltools)
library(randomForest)
library(rpart)
library(caret)
library(plyr)
library(plotly)

load("plotly.RData")
load("full.RData")
load("bubble.RData")
ui <- bootstrapPage(
  navbarPage("Group2's Report", id="nav",
             
    tabPanel("互動PM2.5地圖",
      div(class="outer",
        tags$head(
          # Include our custom CSS
          includeCSS("type.css")
        ),
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 130, left = "auto", right = 42, bottom = "auto",
                      width = 340, height = "auto",fillOpacity = 0.7,
                      
                      h4("Select Bar"),
                      
                      selectInput("year", "年",
                                  c("2011","2012","2013","2014","2015","2016")
                      ),
                      selectInput("month", "月",
                                  c("01","02","03","04","05","06","07","08","09","10","11","12")
                      ),
                      selectInput("day", "日",
                                  c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21",
                                    "22","23","24","25","26","27","28","29","30","31")
                      ),
                      selectInput("choice","要看哪像結果呢～",
                                  c("預測誤差","目標日的PM25","當日的PM25","預測結果"))
                      
        ),
        absolutePanel(top = 20, right = 42,
                      titlePanel(h2("2011~2016年PM2.5預測結果"))
        )
      )
   ),
   tabPanel("互動式圖表",
            plotlyOutput("plot"),
            verbatimTextOutput("event"),
            plotlyOutput("plot2")
   )
  )
)

server <- function(input, output, session) {
  load("plotly.RData")
  load("full.RData")
  load("bubble.RData")
  pal <- colorFactor(
    palette = c("darkblue","darkred","blue","red","green"),
    domain = nd3$errorrange
  )
  pal2 <- colorFactor(
    palette = c('red','orange','green','yellow'),
    domain = nd3$targetlevel
  )
  pal3 <- colorFactor(
    palette = c('red','orange','green','yellow'),
    domain = nd3$PM25level
  )
  pal4 <- colorFactor(
    palette = c('red','orange','green','yellow'),
    domain = nd3$predictlevel
  )
  filteredData <- reactive({
    nd3[nd3$監測日期==paste0(input$year,"-",input$month,"-",input$day),]
  })
  
  col <-reactive({
    if(input$choice=="預測誤差")return(pal(filteredData()$errorrange))
    if(input$choice=="目標日的PM25")return(pal2(filteredData()$targetlevel))
    if(input$choice=="當日的PM25")return(pal3(filteredData()$PM25level))
    if(input$choice=="預測結果")return(pal4(filteredData()$predictlevel))
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.

  var <- reactive({
    if(input$choice=="預測誤差")return(filteredData()$errorrange)
    if(input$choice=="目標日的PM25")return(filteredData()$targetlevel)
    if(input$choice=="當日的PM25")return(filteredData()$PM25level)
    if(input$choice=="預測結果")return(filteredData()$predictlevel)
  })
  
  value <- reactive({
    if(input$choice=="預測誤差")return(filteredData()$error)
    if(input$choice=="目標日的PM25")return(filteredData()$target)
    if(input$choice=="當日的PM25")return(filteredData()$PM25)
    if(input$choice=="預測結果")return(filteredData()$predict)
    
  })
  cha <- reactive({
    if(input$choice=="預測誤差")return("預測誤差")
    if(input$choice=="目標日的PM25")return("目標日的PM25")
    if(input$choice=="當日的PM25")return("PM25指數")
    if(input$choice=="預測結果")return("預測結果")
  })
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(nd3) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      fitBounds(120,22,123.5,25)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  
  labels <- reactive({
    sprintf(
      "<strong>%s</strong><br/>%s<br/>%s<br/>%s:%g<br/>%s:%s",
      filteredData()$COUNTY,filteredData()$TOWN,filteredData()$測站,cha(),value(),"程度",var()) %>% 
      lapply(htmltools::HTML)
    
  })
  observe({
    leafletProxy("map", data = filteredData())%>% clearShapes() %>%  addTiles() %>%
      addCircles(data = filteredData(), lat = ~ 緯度, lng = ~ 經度,
                 weight = 1, radius = 5000, popup = ~as.character(var()), 
                 label = labels(), 
                 color = ~col(), fillOpacity = 0.5)
  
  })

  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = nd3)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()

  })
  output$plot <- renderPlotly({
    plot_ly(data2, x = ~year, y = ~高雄市, name = '高雄市', type = 'scatter', mode = 'lines',
            line = list(color = '#F596AA', width = 1)) %>%
      add_trace(y = ~花蓮縣, name = '花蓮縣', line = list(color = '#E9CD4C', width = 1)) %>%
      add_trace(y = ~基隆市, name = '基隆市', line = list(color = '#FBE251', width = 1)) %>%
      add_trace(y = ~嘉義市, name = '嘉義市', line = list(color = '#D0104C', width = 1)) %>%
      add_trace(y = ~嘉義縣, name = '嘉義縣', line = list(color = '#CB4042', width = 1)) %>%
      add_trace(y = ~連江縣, name = '連江縣', line = list(color = '#C73E3A', width = 1)) %>%
      add_trace(y = ~苗栗縣, name = '苗栗縣', line = list(color = '#E79460', width = 1)) %>%
      add_trace(y = ~南投縣, name = '南投縣', line = list(color = '#B54434', width = 1)) %>%
      add_trace(y = ~澎湖縣, name = '澎湖縣', line = list(color = '#FFC408', width = 1)) %>%
      add_trace(y = ~屏東縣, name = '屏東縣', line = list(color = '#EB7A77', width = 1)) %>%
      add_trace(y = ~臺北市, name = '臺北市', line = list(color = '#E98B2A', width = 1)) %>%
      add_trace(y = ~臺東縣, name = '臺東縣', line = list(color = '#FB966E', width = 1)) %>%
      add_trace(y = ~臺南市, name = '臺南市', line = list(color = '#D05A6E', width = 1)) %>%
      add_trace(y = ~臺中市, name = '臺中市', line = list(color = '#F05E1C', width = 1)) %>%
      add_trace(y = ~新北市, name = '新北市', line = list(color = '#F7C242', width = 1)) %>%
      add_trace(y = ~新竹縣, name = '新竹縣', line = list(color = '#FFB11B', width = 1)) %>%
      add_trace(y = ~宜蘭縣, name = '宜蘭縣', line = list(color = '#D9AB42', width = 1)) %>%
      add_trace(y = ~雲林縣, name = '雲林縣', line = list(color = '#CC543A', width = 1)) %>%
      add_trace(y = ~彰化縣, name = '彰化縣', line = list(color = '#E83015', width = 1)) %>%
      add_trace(y = ~金門縣, name = '金門縣', line = list(color = '#DB4D6D', width = 1)) %>%
      layout(title = "2011~2016各地區PM2.5變化狀況",
             xaxis = list(
               rangeslider = list(type = "年份")),
             yaxis = list (title = "PM2.5指標（細懸浮微粒(μg/m 3 )）"),
             font= list(family="黑體-繁 中黑"))
  })
  colors <- c('#4AC6B7', '#1972A4', '#965F8A','#707C74', '#FF7070', '#C61951','#227D51','#F7D94C')
  output$plot2 <- renderPlotly({
    plot_ly(full3, x = ~相對濕度, y = ~大氣溫度, color = ~空品區, size = ~size, colors = colors,frame = ~year,hoverinfo = "text",
            type = 'scatter', mode = 'markers', sizes = c(min(full3$size), max(full3$size)),
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')),
            text = ~paste('County:', COUNTY, '<br>Town:', TOWN, '<br>測站:', 測站,
                          '<br>PM25:', PM25,'<br>相對濕度:', 相對濕度,'<br>大氣溫度:', 大氣溫度)) %>%
      layout(title = 'PM2.5與天氣關係變化',
             xaxis = list(title = '相對濕度',
                          gridcolor = 'rgb(255, 255, 255)',
                          range = c(0, 75),
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
             yaxis = list(title = '大氣溫度',
                          gridcolor = 'rgb(255, 255, 255)',
                          range = c(0, 19),
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwith = 2),
             paper_bgcolor = 'rgb(243, 243, 243)',
             plot_bgcolor = 'rgb(243, 243, 243)')
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
}

shinyApp(ui, server)

