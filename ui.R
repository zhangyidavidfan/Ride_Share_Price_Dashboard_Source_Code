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
#Live scrape weather information to guide user
link = read_html("https://weather.com/weather/today/l/USIL0225:1:US")
humid = link %>% html_nodes(xpath = '//*[@id="dp0-details"]/ul/li[2]/span[2]/span[2]/span/span')%>%html_text()
temp = link %>% html_nodes(xpath = '//*[@id="daypart-0"]/div/div[4]/span')%>%html_text()
wind = link %>% html_nodes(xpath = '//*[@id="dp0-details-wind"]/span')%>%html_text()
precip = link %>% html_nodes(xpath = '//*[@id="daypart-0"]/div/div[5]/span[2]/span')%>%html_text()
humid = as.numeric(gsub("([0-9]+).*$", "\\1", humid))
temp = as.numeric(gsub("([0-9]+).*$", "\\1", temp))
precip = as.numeric(gsub("([0-9]+).*$", "\\1", precip))/100
wind = as.numeric(gsub("..([0-9]+).*$", "\\1", "N 14 mph"))

# Define UI for the interactive Panel
ui <- dashboardPage(
  # Panel title
  dashboardHeader(title = "Ride Share Price Interaction Panel",titleWidth = 450),
  # Sidebar with controls for user input and execution
  dashboardSidebar(
    tabsetPanel(
      tabPanel("Comm Area", 
               useShinyalert(),
               tags$head(
                 tags$style(HTML("
                  #instruction1 {
                    display:block;
                    height: 40px;
                    width: 200px;
                    }
                    "))
               ),
               actionButton("instruction1", "Instruction"),
               selectInput("comm_area_start", "Comm Area Start:",
                           c("EDISON PARK"="EDISON PARK","NEAR NORTH SIDE"="NEAR NORTH SIDE","EDGEWATER"="EDGEWATER","OHARE"="OHARE","MORGAN PARK"="MORGAN PARK","MOUNT GREENWOOD"="MOUNT GREENWOOD","WASHINGTON HEIGHTS"="WASHINGTON HEIGHTS","BEVERLY"="BEVERLY","AUBURN GRESHAM"="AUBURN GRESHAM","ASHBURN"="ASHBURN","LINCOLN PARK"="LINCOLN PARK","GREATER GRAND CROSSING"="GREATER GRAND CROSSING","ENGLEWOOD"="ENGLEWOOD","WEST ENGLEWOOD"="WEST ENGLEWOOD","CHICAGO LAWN"="CHICAGO LAWN","WEST LAWN"="WEST LAWN","CLEARING"="CLEARING","GAGE PARK"="GAGE PARK","WEST ELSDON"="WEST ELSDON","NEW CITY"="NEW CITY","BRIDGEPORT"="BRIDGEPORT","LAKE VIEW"="LAKE VIEW","MCKINLEY PARK"="MCKINLEY PARK","BRIGHTON PARK"="BRIGHTON PARK","ARCHER HEIGHTS"="ARCHER HEIGHTS","GARFIELD RIDGE"="GARFIELD RIDGE","HEGEWISCH"="HEGEWISCH","RIVERDALE"="RIVERDALE","WEST PULLMAN"="WEST PULLMAN","EAST SIDE"="EAST SIDE","SOUTH DEERING"="SOUTH DEERING","PULLMAN"="PULLMAN","NORTH CENTER"="NORTH CENTER","ROSELAND"="ROSELAND","CALUMET HEIGHTS"="CALUMET HEIGHTS","BURNSIDE"="BURNSIDE","SOUTH CHICAGO"="SOUTH CHICAGO","AVALON PARK"="AVALON PARK","CHATHAM"="CHATHAM","SOUTH SHORE"="SOUTH SHORE","WOODLAWN"="WOODLAWN","HYDE PARK"="HYDE PARK","WASHINGTON PARK"="WASHINGTON PARK","LINCOLN SQUARE"="LINCOLN SQUARE","KENWOOD"="KENWOOD","GRAND BOULEVARD"="GRAND BOULEVARD","FULLER PARK"="FULLER PARK","OAKLAND"="OAKLAND","DOUGLAS"="DOUGLAS","ARMOUR SQUARE"="ARMOUR SQUARE","NEAR SOUTH SIDE"="NEAR SOUTH SIDE","LOOP"="LOOP","LOWER WEST SIDE"="LOWER WEST SIDE","SOUTH LAWNDALE"="SOUTH LAWNDALE","UPTOWN"="UPTOWN","NORTH LAWNDALE"="NORTH LAWNDALE","NEAR WEST SIDE"="NEAR WEST SIDE","EAST GARFIELD PARK"="EAST GARFIELD PARK","WEST GARFIELD PARK"="WEST GARFIELD PARK","AUSTIN"="AUSTIN","WEST TOWN"="WEST TOWN","HUMBOLDT PARK"="HUMBOLDT PARK","LOGAN SQUARE"="LOGAN SQUARE","AVONDALE"="AVONDALE","HERMOSA"="HERMOSA","WEST RIDGE"="WEST RIDGE","BELMONT CRAGIN"="BELMONT CRAGIN","MONTCLARE"="MONTCLARE","DUNNING"="DUNNING","IRVING PARK"="IRVING PARK","PORTAGE PARK"="PORTAGE PARK","ALBANY PARK"="ALBANY PARK","NORTH PARK"="NORTH PARK","FOREST GLEN"="FOREST GLEN","JEFFERSON PARK"="JEFFERSON PARK","NORWOOD PARK"="NORWOOD PARK","ROGER"="ROGER")), 
               selectInput("comm_area_end", "Comm Area End:",
                           c("EDISON PARK"="EDISON PARK","NEAR NORTH SIDE"="NEAR NORTH SIDE","EDGEWATER"="EDGEWATER","OHARE"="OHARE","MORGAN PARK"="MORGAN PARK","MOUNT GREENWOOD"="MOUNT GREENWOOD","WASHINGTON HEIGHTS"="WASHINGTON HEIGHTS","BEVERLY"="BEVERLY","AUBURN GRESHAM"="AUBURN GRESHAM","ASHBURN"="ASHBURN","LINCOLN PARK"="LINCOLN PARK","GREATER GRAND CROSSING"="GREATER GRAND CROSSING","ENGLEWOOD"="ENGLEWOOD","WEST ENGLEWOOD"="WEST ENGLEWOOD","CHICAGO LAWN"="CHICAGO LAWN","WEST LAWN"="WEST LAWN","CLEARING"="CLEARING","GAGE PARK"="GAGE PARK","WEST ELSDON"="WEST ELSDON","NEW CITY"="NEW CITY","BRIDGEPORT"="BRIDGEPORT","LAKE VIEW"="LAKE VIEW","MCKINLEY PARK"="MCKINLEY PARK","BRIGHTON PARK"="BRIGHTON PARK","ARCHER HEIGHTS"="ARCHER HEIGHTS","GARFIELD RIDGE"="GARFIELD RIDGE","HEGEWISCH"="HEGEWISCH","RIVERDALE"="RIVERDALE","WEST PULLMAN"="WEST PULLMAN","EAST SIDE"="EAST SIDE","SOUTH DEERING"="SOUTH DEERING","PULLMAN"="PULLMAN","NORTH CENTER"="NORTH CENTER","ROSELAND"="ROSELAND","CALUMET HEIGHTS"="CALUMET HEIGHTS","BURNSIDE"="BURNSIDE","SOUTH CHICAGO"="SOUTH CHICAGO","AVALON PARK"="AVALON PARK","CHATHAM"="CHATHAM","SOUTH SHORE"="SOUTH SHORE","WOODLAWN"="WOODLAWN","HYDE PARK"="HYDE PARK","WASHINGTON PARK"="WASHINGTON PARK","LINCOLN SQUARE"="LINCOLN SQUARE","KENWOOD"="KENWOOD","GRAND BOULEVARD"="GRAND BOULEVARD","FULLER PARK"="FULLER PARK","OAKLAND"="OAKLAND","DOUGLAS"="DOUGLAS","ARMOUR SQUARE"="ARMOUR SQUARE","NEAR SOUTH SIDE"="NEAR SOUTH SIDE","LOOP"="LOOP","LOWER WEST SIDE"="LOWER WEST SIDE","SOUTH LAWNDALE"="SOUTH LAWNDALE","UPTOWN"="UPTOWN","NORTH LAWNDALE"="NORTH LAWNDALE","NEAR WEST SIDE"="NEAR WEST SIDE","EAST GARFIELD PARK"="EAST GARFIELD PARK","WEST GARFIELD PARK"="WEST GARFIELD PARK","AUSTIN"="AUSTIN","WEST TOWN"="WEST TOWN","HUMBOLDT PARK"="HUMBOLDT PARK","LOGAN SQUARE"="LOGAN SQUARE","AVONDALE"="AVONDALE","HERMOSA"="HERMOSA","WEST RIDGE"="WEST RIDGE","BELMONT CRAGIN"="BELMONT CRAGIN","MONTCLARE"="MONTCLARE","DUNNING"="DUNNING","IRVING PARK"="IRVING PARK","PORTAGE PARK"="PORTAGE PARK","ALBANY PARK"="ALBANY PARK","NORTH PARK"="NORTH PARK","FOREST GLEN"="FOREST GLEN","JEFFERSON PARK"="JEFFERSON PARK","NORWOOD PARK"="NORWOOD PARK","ROGER"="ROGER")),
               textInput("start_time", "Start Time (Follow h:m:S AM/PM)", value = "12:00:00 AM"),
               textInput("pooled","How many people will pool with you?(if none type 1)", value = '1'),
               textInput("date","Date of trip (yyyy-mm-dd)", value = Sys.Date()),
               tags$head(
                 tags$style(HTML("
                  #go_1 {
                    display:block;
                    height: 40px;
                    width: 200px;
                    }
                    "))
               ),
               actionButton("go_1", "Calculate and Show")),
      #the second mode of inputs
      tabPanel("Long & Lat", 
               useShinyalert(),
               tags$head(
                 tags$style(HTML("
                  #instruction2 {
                    display:block;
                    height: 40px;
                    width: 200px;
                    }
                    "))
               ),
               actionButton("instruction2", "Instruction"),
               selectInput("comm_area_start", "Comm Area Start:",
                           c("EDISON PARK"="EDISON PARK","NEAR NORTH SIDE"="NEAR NORTH SIDE","EDGEWATER"="EDGEWATER","OHARE"="OHARE","MORGAN PARK"="MORGAN PARK","MOUNT GREENWOOD"="MOUNT GREENWOOD","WASHINGTON HEIGHTS"="WASHINGTON HEIGHTS","BEVERLY"="BEVERLY","AUBURN GRESHAM"="AUBURN GRESHAM","ASHBURN"="ASHBURN","LINCOLN PARK"="LINCOLN PARK","GREATER GRAND CROSSING"="GREATER GRAND CROSSING","ENGLEWOOD"="ENGLEWOOD","WEST ENGLEWOOD"="WEST ENGLEWOOD","CHICAGO LAWN"="CHICAGO LAWN","WEST LAWN"="WEST LAWN","CLEARING"="CLEARING","GAGE PARK"="GAGE PARK","WEST ELSDON"="WEST ELSDON","NEW CITY"="NEW CITY","BRIDGEPORT"="BRIDGEPORT","LAKE VIEW"="LAKE VIEW","MCKINLEY PARK"="MCKINLEY PARK","BRIGHTON PARK"="BRIGHTON PARK","ARCHER HEIGHTS"="ARCHER HEIGHTS","GARFIELD RIDGE"="GARFIELD RIDGE","HEGEWISCH"="HEGEWISCH","RIVERDALE"="RIVERDALE","WEST PULLMAN"="WEST PULLMAN","EAST SIDE"="EAST SIDE","SOUTH DEERING"="SOUTH DEERING","PULLMAN"="PULLMAN","NORTH CENTER"="NORTH CENTER","ROSELAND"="ROSELAND","CALUMET HEIGHTS"="CALUMET HEIGHTS","BURNSIDE"="BURNSIDE","SOUTH CHICAGO"="SOUTH CHICAGO","AVALON PARK"="AVALON PARK","CHATHAM"="CHATHAM","SOUTH SHORE"="SOUTH SHORE","WOODLAWN"="WOODLAWN","HYDE PARK"="HYDE PARK","WASHINGTON PARK"="WASHINGTON PARK","LINCOLN SQUARE"="LINCOLN SQUARE","KENWOOD"="KENWOOD","GRAND BOULEVARD"="GRAND BOULEVARD","FULLER PARK"="FULLER PARK","OAKLAND"="OAKLAND","DOUGLAS"="DOUGLAS","ARMOUR SQUARE"="ARMOUR SQUARE","NEAR SOUTH SIDE"="NEAR SOUTH SIDE","LOOP"="LOOP","LOWER WEST SIDE"="LOWER WEST SIDE","SOUTH LAWNDALE"="SOUTH LAWNDALE","UPTOWN"="UPTOWN","NORTH LAWNDALE"="NORTH LAWNDALE","NEAR WEST SIDE"="NEAR WEST SIDE","EAST GARFIELD PARK"="EAST GARFIELD PARK","WEST GARFIELD PARK"="WEST GARFIELD PARK","AUSTIN"="AUSTIN","WEST TOWN"="WEST TOWN","HUMBOLDT PARK"="HUMBOLDT PARK","LOGAN SQUARE"="LOGAN SQUARE","AVONDALE"="AVONDALE","HERMOSA"="HERMOSA","WEST RIDGE"="WEST RIDGE","BELMONT CRAGIN"="BELMONT CRAGIN","MONTCLARE"="MONTCLARE","DUNNING"="DUNNING","IRVING PARK"="IRVING PARK","PORTAGE PARK"="PORTAGE PARK","ALBANY PARK"="ALBANY PARK","NORTH PARK"="NORTH PARK","FOREST GLEN"="FOREST GLEN","JEFFERSON PARK"="JEFFERSON PARK","NORWOOD PARK"="NORWOOD PARK","ROGER"="ROGER")),
               textInput("s_longitude", "Start Longtitude",value = -87),
               textInput("s_latitude", "Start Latitude", value = 42),
               textInput("e_longitude", "End Longtitude",value = -87),
               textInput("e_latitude", "End Latitude", value = 42),
               textInput("start_time", "Start Time (Follow h:m:S AM/PM)", value = "12:00:00 AM"),
               textInput("pooled","How many people will pool with you?(if none type 1)", value = '1'),
               textInput("date","Date of trip (yyyy-mm-dd)", value = Sys.Date()),
               tags$head(
                 tags$style(HTML("
                  #go_2 {
                    display:block;
                    height: 40px;
                    width: 200px;
                    }
                    "))
               ),
               actionButton("go_2", "Calculate and Show")
      )
    )),
  # Dashboard body
  dashboardBody(
    fluidRow(
      #Weather info box
    tags$head(tags$style(HTML('.info-box {min-height: 65px;} .info-box-icon {height: 65px; line-height: 65px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
    tabItems(
      infoBoxOutput("weatherBox",width = 12)
    ),
    #Weather related slide bar
    box(
      div(style="height:80px;",sliderInput("humid","Expected Humidity",
                                           min = 0, max = 110, value = humid
      )),
      div(style="height: 80px;",sliderInput("precip","Expected Percipitation",
                                            min = 0, max = 1, value = precip
      ))
    ),
    box(
      div(style="height: 80px;",sliderInput("temp","Expected Temperature",
                                            min = 0, max = 110, value = temp
      )),
      div(style="height: 80px;",sliderInput("wind","Expected Wind Strength",
                                            min = 0, max = 50, value = wind
      ))
    ),
    #Other Info Box
    infoBoxOutput("priceBox",width = 6),
    infoBoxOutput("timeBox",width = 6),
    #Map
    box(title = "Map Output", status = "primary", 
        column( 12,align="center" ,plotOutput(outputId="mapOut",
                                                width="560", height="450", click = "plot_click",
                                                dblclick = "plot_dblclick"
        )),solidHeader = TRUE,
    collapsible = TRUE, width = 12)
    
  )
))
