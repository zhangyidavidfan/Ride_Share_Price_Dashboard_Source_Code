#loading relevant libraries
library(shiny)
library(shinydashboard)
library(shinyalert)
library(leaflet)
library(rvest)
library(shinythemes)
library(ggmap)
library(chron)
library(stringr)
library(dplyr)
library(readr)

#loading relevant datasets
holiday=read_csv("Holiday.csv")
areaInfo=read_csv("areaInfo.csv")
Community_location_df = read_csv("community_location_df.csv")

#loading previously trained model
load("price_model.rda")
load("time_model.rda")

#Live scrape weather information to guide user
link = read_html("https://weather.com/weather/today/l/USIL0225:1:US")
#Aggregating weather information
humid = link %>% html_nodes(xpath = '//*[@id="dp0-details"]/ul/li[2]/span[2]/span[2]/span/span')%>%html_text()
temp = link %>% html_nodes(xpath = '//*[@id="daypart-0"]/div/div[4]/span')%>%html_text()
wind = link %>% html_nodes(xpath = '//*[@id="dp0-details-wind"]/span')%>%html_text()
precip = link %>% html_nodes(xpath = '//*[@id="daypart-0"]/div/div[5]/span[2]/span')%>%html_text()
humid = as.numeric(gsub("([0-9]+).*$", "\\1", humid))
temp = as.numeric(gsub("([0-9]+).*$", "\\1", temp))
precip = as.numeric(gsub("([0-9]+).*$", "\\1", precip))/100
wind = as.numeric(gsub("..([0-9]+).*$", "\\1", "N 14 mph"))

#register for google API
register_google(key = 'AIzaSyDl0v77_lnHXrgqa_O4XGZuPDmcZLQfQWs')

# Server Side
server <- shinyServer(function(input, output,session) {
  #Extraxt relevant weather summary from website
  text = link %>% html_nodes(xpath = '//*[@id="dp0-details-narrative"]')%>% html_text()
  #Include Humidtity measure
  text = paste(text, "with",
               link %>% html_nodes(xpath = '//*[@id="dp0-details"]/ul/li[2]/span[2]/span[2]/span/span')%>% html_text(),"percent Humidity")
  #Initialize weather information box
  output$weatherBox <- renderInfoBox({
    infoBox(
      "Weather",value = tags$p(style = "font-size: 11px;", text) , icon = icon("sun"),
      color = "blue", fill = TRUE
    )
  })
  #Initialize time information box
  output$timeBox <- renderInfoBox({
    infoBox(
      "Time", value =  "Click to predict", 
      icon = icon("calendar"),
      color = "blue", fill = TRUE
    )
  })
  #Initialize price information box
  output$priceBox <- renderInfoBox({
    infoBox(
      "Price",value =  "Click to predict", icon = icon("dollar-sign"),
      color = "blue", fill = TRUE
    )
  })
  #Initialize map
  map <- get_map(location = "Chicago",maptype="roadmap",scale=2, zoom =11)
  output$mapOut = renderPlot({
    mapPoints <-  ggmap(map)+ theme(plot.background = element_rect(fill = "transparent"));
    mapPoints
  })
  
  #wrapping base functions in try-catch
  numeric_f <- function(x){
    tryCatch(
      # This is what I want to do...
      {
        y = as.numeric(x)
        return(y)
      },
      # ... but if an error occurs, tell me what happened: 
      error=function(cond) {
        shinyalert("Error", "Something went wrong.",type = "error")
        return(0)
      },
      warning=function(cond) {
        shinyalert("Error", "Something went wrong.",type = "error")
        return(0)
      }
    )
  }
  time_f <- function(x){
    tryCatch(
      # This is what I want to do...
      {
        y = chron(times = x)
        return(y)
      },
      # ... but if an error occurs, tell me what happened: 
      error=function(cond) {
        shinyalert("Error", "Something went wrong.",type = "error")
        return(chron(times="12:00:00"))
      }
    )
  }
  time_disp <- function(x){
    tryCatch(
      # This is what I want to do...
      {
        y = chron(times = x)
        return(y)
      },
      # ... but if an error occurs, tell me what happened: 
      error=function(cond) {
        return(chron(times="12:00:00"))
      },
      warning=function(cond) {
        return(chron(times="12:00:00"))
      }
    )
  }
  date_f <- function(x){
    tryCatch(
      # This is what I want to do...
      {
        y = as.Date(x)
        return(y)
      },
      # ... but if an error occurs, tell me what happened: 
      error=function(cond) {
        shinyalert("Error", "Something went wrong.",type = "error")
        return(as.Date("1900-1-1"))
      }
    )
  }
  
  # dynamic dataset processing based on user input
  dataset_clean <- reactive({
    prep = data.frame("TripsPooled" = input$pooled, 
                      "Start_Time" = time_f(str_split(input$start_time, " ")[[1]][1]),
                      "Start_AM_PM" = str_split(input$start_time, " ")[[1]][2],
                      "AvgTemp" = input$temp,
                      "MaxWindSpeed" = input$wind,
                      "MaxHumidity" = input$humid,
                      "AvgPrecipation" = input$precip,
                      "is_holiday" = date_f(input$date) %in% holiday$Date,
                      "Month" = months(date_f(input$date)),
                      "Weekdays" = weekdays(date_f(input$date)))
    prep$Start_AM_PM = as.character(prep$Start_AM_PM)
    prep[prep$Start_Time < chron(times ="6:00:00"),"Start_AM_PM"] = sapply(prep[prep$Start_Time < chron(times ="6:00:00"),"Start_AM_PM"], function(x) paste(x,"early")) 
    prep[prep$Start_Time >= chron(times ="6:00:00"),"Start_AM_PM"] = sapply(prep[prep$Start_Time >= chron(times ="6:00:00"),"Start_AM_PM"], function(x) paste(x,"late")) 
    prep$Start_AM_PM = as.factor(prep$Start_AM_PM)
    part = areaInfo[areaInfo$Name == input$comm_area_start,]
    part = part[,3:7] %>% mutate_all(numeric_f)
    final = cbind(part, prep)
    final$TripsPooled = numeric_f(as.character(final$TripsPooled))
    final$is_holiday = numeric_f(final$is_holiday)
    final$AvgTemp = numeric_f(as.character(final$AvgTemp))
    final$MaxWindSpeed = numeric_f(as.character(final$MaxWindSpeed)) 
    final$MaxHumidity = numeric_f(as.character(final$MaxHumidity))
    final$AvgPrecipation = numeric_f(as.character(final$AvgPrecipation))
    final
  })
  #extract reactive coordinates
  outVar_x = reactive({
    if(!is.null(input$plot_click)){
      x = input$plot_click$x
    }
    else{
      x = input$s_longitude
    }
    x
  })
  outVar_y = reactive({
    if(!is.null(input$plot_click)){
      y = input$plot_click$y
    }
    else{
      y = input$s_longitude
    }
    y
  })
  #upatde input space with user clicked coordinates
  observeEvent(eventExpr = input$plot_click, {
    updateTextInput(session, "s_longitude",
                    value = outVar_x())
    updateTextInput(session,"s_latitude",
                    value = outVar_y())
  })
  outVar_x_2 = reactive({
    if(!is.null(input$plot_dblclick)){
      x = input$plot_dblclick$x
    }
    else{
      x = input$e_longitude
    }
    x
  })
  outVar_y_2 = reactive({
    if(!is.null(input$plot_dblclick)){
      y = input$plot_dblclick$y
    }
    else{
      y = input$e_longitude
    }
    y
  })
  #upatde input space with user clicked coordinates
  observeEvent(eventExpr = input$plot_dblclick, {
    updateTextInput(session, "e_longitude",
                    value = outVar_x_2())
    updateTextInput(session,"e_latitude",
                    value = outVar_y_2())
  })
  
  #Generate Prediction
  observeEvent( input$go_1,{
    #gather data
    data_used <-dataset_clean()
    #define distance function
    distance = function(s_x,s_y,e_x,e_y){
      return(sqrt((s_x- e_x)^2 
                  + (s_y- e_y)^2))
    }
    #calculate distance
    data_used$distance = distance(Community_location_df[Community_location_df$Name == input$comm_area_start,]$latitude,
                                  Community_location_df[Community_location_df$Name == input$comm_area_start,]$longitude,
                                  Community_location_df[Community_location_df$Name == input$comm_area_end,]$latitude,
                                  Community_location_df[Community_location_df$Name == input$comm_area_end,]$longitude)
    data_used = as.data.frame(data_used)
    #Predict and render price information
    predicted_price = predict(prelim_model,data_used)
    output$price = renderText({paste("the price will be around $",predicted_price )})
    #Predict and render time information
    predicted_time = predict(prelim_model_time,data_used)/86400
    output$time = renderText({paste("the arrival time will be around", chron(times=str_split(input$start_time, " ")[[1]][1]) + predicted_time)})
    #Output map with the location specified
    lat1 = Community_location_df[Community_location_df$Name == input$comm_area_start,]$latitude
    lat2 = Community_location_df[Community_location_df$Name == input$comm_area_end,]$latitude
    lat = (lat1 + lat2)/2
    lon1 = Community_location_df[Community_location_df$Name == input$comm_area_start,]$longitude
    lon2 = Community_location_df[Community_location_df$Name == input$comm_area_end,]$longitude
    lon = (lon1 + lon2)/2
    map <- get_map(location = c(lon = lon,lat = lat),maptype="roadmap",scale=2, zoom =11)
    df = data.frame(lon = c(lon1,lon2), lat = c(lat1,lat2))
    df2 = data.frame(lon_start = lon1,lon_end = lon2,lat_start = lat1, lat_end = lat2)
    #Output information in info boxes
    output$timeBox <- renderInfoBox({
      infoBox(
        "Time", value = toString(time_disp(str_split(input$start_time, " ")[[1]][1]) + predicted_time), 
        icon = icon("calendar"),
        color = "blue", fill = TRUE
      )
    })
    output$priceBox <- renderInfoBox({
      infoBox(
        "Price",value = toString(predicted_price) , icon = icon("dollar-sign"),
        color = "blue", fill = TRUE
      )
    })
    #output Map
    output$mapOut = renderPlot({
      mapPoints <-  ggmap(map) + geom_point(size=8,colour="black",alpha=0.5, data=df)+
        geom_segment(aes(x = lon_start, y = lat_start, xend = lon_end, yend = lat_end, colour = "segment"),size = 2,
                     linetype = 2,arrow=arrow(length=unit(0.4,"cm")),alpha = 0.6, data = df2);
      mapPoints
      
    })
  })
  
  #Generate Prediction second form
  observeEvent( input$go_2,{
    #Extract data
    data_used <-dataset_clean()
    #distance function
    distance = function(s_x,s_y,e_x,e_y){
      return(sqrt((s_x- e_x)^2 
                  + (s_y- e_y)^2))
    }
    #take user prompted cooridnates
    lat1 = numeric_f(input$s_latitude)
    lat2 = numeric_f(input$e_latitude)
    lon1 = numeric_f(input$s_longitude)
    lon2 = numeric_f(input$e_longitude)
    data_used$distance = distance(lon1,lat1,lon2,lat2)
    #Rest of the procedure follows above!
    data_used = as.data.frame(data_used)
    predicted_price = predict(prelim_model,data_used)
    output$price = renderText({paste("the price will be around $",predicted_price )})
    predicted_time = predict(prelim_model_time,data_used)/86400
    output$time = renderText({paste("the arrival time will be around", chron(times=str_split(input$start_time, " ")[[1]][1]) + predicted_time)})
    register_google(key = 'AIzaSyDl0v77_lnHXrgqa_O4XGZuPDmcZLQfQWs')
    lat = (lat1 + lat2)/2
    lon = (lon1 + lon2)/2
    map <- get_map(location = c(lon = lon,lat = lat),maptype="roadmap",scale=2, zoom =11)
    df = data.frame(lon = c(lon1,lon2), lat = c(lat1,lat2))
    df2 = data.frame(lon_start = lon1,lon_end = lon2,lat_start = lat1, lat_end = lat2)
    output$timeBox <- renderInfoBox({
      infoBox(
        "Time", value = toString(time_disp(str_split(input$start_time, " ")[[1]][1]) + predicted_time), 
        icon = icon("calendar"),
        color = "blue", fill = TRUE
      )
    })
    output$priceBox <- renderInfoBox({
      infoBox(
        "Price",value = toString(predicted_price) , icon = icon("dollar-sign"),
        color = "blue", fill = TRUE
      )
    })
    output$mapOut = renderPlot({
      mapPoints <-  ggmap(map) + geom_point(size=8,colour="black",alpha=0.5, data=df)+
        geom_segment(aes(x = lon_start, y = lat_start, xend = lon_end, yend = lat_end, colour = "segment"),size = 2,
                     linetype = 2,arrow=arrow(length=unit(0.4,"cm")),alpha = 0.6, data = df2);
      mapPoints
      
    })
  })
  #Instructions
  observeEvent(input$instruction1, {
    # Show a modal when the button is pressed
    shinyalert("Description", "Hi welcome to the ride share interactive panel designed by David Fan
    
               This Panel can help you predict the price and the approximate trip time of a general ride share service in Chicago at a certain point in the future, with only minimal information such as weather forecast and the locations
               
               Within this user interface,it will allow you to predict the price in two ways. The first allows you to input the starting and ending community area in Chicago, and will generate the corresponding price and time of trip for it. The second prompts you to input the longtitude and latitude of your starting point and ending point to make the prediction. In the second mode specifically, you could also tap on the map to generate the coordinates of your trip. One tap will give you the starting location, while 2 will you the ending location.
               
               I hope you enjoy this project, Thanks
               
               Data Sources: Chicago City Data, Weather Underground, Rob Paral, Illinois State")
  })
  #instructions
  observeEvent(input$instruction2, {
    # Show a modal when the button is pressed
    shinyalert("Description", "Hi welcome to the ride share interactive panel designed by David Fan
    
               This Panel can help you predict the price and the approximate trip time of a general ride share service in Chicago at a certain point in the future, with only minimal information such as weather forecast and the locations
               
               Within this user interface,it will allow you to predict the price in two ways. The first allows you to input the starting and ending community area in Chicago, and will generate the corresponding price and time of trip for it. The second prompts you to input the longtitude and latitude of your starting point and ending point to make the prediction. In the second mode specifically, you could also tap on the map to generate the coordinates of your trip. One tap will give you the starting location, while 2 will you the ending location.
               
               I hope you enjoy this project, Thanks
               
               Data Sources: Chicago City Data, Weather Underground, Rob Paral, Illinois State")
  })
  
})
