#path <- "/Users/sebastianmontero/Dropbox/IE MBD/Programming R/workgroup_project/"
#setwd(path)

library(shiny)
library(data.table)
library(tidyverse)
library(DT)
library(leaflet)

options(shiny.maxRequestSize = 30*1024^2)

#### FUNCTIONS ####
clean_data <- function(x){
  raw_data <- x
  viz_data <- raw_data[,1:99]
  
  viz_data <- gather(viz_data,key = "Station", value = "Value", -"Date")
  
  viz_data$Day <- substr(viz_data$Date,7,8)
  viz_data$Month <- substr(viz_data$Date,5,6)
  viz_data$Year <- substr(viz_data$Date,1,4)
  viz_data$Date_Clean <- paste(viz_data$Day,"/",viz_data$Month,"/",viz_data$Year, sep = "")
  
  viz_data$Date <- as.POSIXct(strptime(viz_data$Date_Clean, "%d/%m/%Y"))
  viz_data <- viz_data[,c(-7)]
  viz_data <- filter(viz_data, Value != "NA")
  viz_data$Value  <- as.numeric(viz_data$Value)
  return(viz_data)
}

#### IMPORT AND CLEAN ####

#raw_data <- data.table(readRDS("solar_dataset.RData"))
#viz_data <- data.table(clean_data(raw_data))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  data <- reactive({
      f1 <- input$file_upload
      if (is.null(f1)){
      }else{
      raw_data <- data.table(readRDS(f1$datapath))
      viz_data <- data.table(clean_data(raw_data))
      
      station_use <- input$s_selection
      
      viz_data2  <- if (station_use == "" || is.null(station_use)){
        return(viz_data)
      }else{
        viz_data_filter <- viz_data[viz_data$Station == station_use ,]
      }
      return(viz_data2)
      }
    })
  

  output$line <- renderPlot({
    x <- input$time_type
    viz_data <- data()
    
    if (is.null(viz_data)){
      ggplot() + labs(title = paste("Load .RData to see Time Series Analysis")) + theme_void()
    } else {
  
    if (x=="Day") {
      total_by_date <- viz_data[,.(Total_Value = sum(Value)), by = c("Date", "Day", "Month", "Year")]
    
      ggplot(total_by_date, aes(x = Date, y = Total_Value)) +
        geom_line(color = "#FA8072")+
        theme_classic()+
        labs(x = "Day", y="Production", title = paste("Total Production by Day"))
    } else if (x=="Month") {
      total_by_date <- viz_data[,.(Total_Value = sum(Value)), by = c("Month", "Year")]
      total_by_date$Date <- paste("01","/",total_by_date$Month,"/",total_by_date$Year,sep="")
      total_by_date$Date <- as.POSIXct(strptime(total_by_date$Date, "%d/%m/%Y"))
      
      ggplot(total_by_date, aes(x = Date, y = Total_Value)) +
        geom_line(color = "#FA8072",size = 1.5)+
        theme_classic()+
        labs(x = "Month", y="Production", title = paste("Total Production by Month"))
    } else {
      total_by_date <- viz_data[,.(Total_Value = sum(Value)), by = c("Year")]
      total_by_date$Year <- as.numeric(as.character(total_by_date$Year))
      
      ggplot(total_by_date, aes(x = Year, y = Total_Value)) +
        geom_point(color = "#FA8072")+
        geom_line(color = "#FA8072",size = 1.5) +
        theme_classic() +
        labs(x = "Year", y="Production", title = paste("Total Production by Year"))
    }
    }
  })
  
  output$bar <- renderPlot({
    n = input$station_n
    viz_data <- data()
    
    if (is.null(viz_data)){
      ggplot() + labs(title = paste("Load .RData to see Bar Chart Analysis")) + theme_void()
    } else {
    
    total_by_station <- top_n(arrange(viz_data[,.(Total_Value = sum(Value)), by=Station], desc(Total_Value)),n)
    total_by_station$Station <- factor(total_by_station$Station, levels = total_by_station$Station[order(total_by_station$Total_Value)])
    ggplot(total_by_station, aes(y=Total_Value, x=Station,label = Total_Value)) +
      geom_bar(stat="identity", fill = "#FA8072") +
      coord_flip() +
      theme_classic() +
      labs(x = "Station Name", y="Production", title = paste("Production for Top",n,"Stations"))
    }
  })
  
  output$map <- renderLeaflet({
    #### DPUT ####
    station_location <- structure(list(stid = structure(1:98, .Label = c("ACME", "ADAX", 
                                                                         "ALTU", "APAC", "ARNE", "BEAV", "BESS", "BIXB", "BLAC", "BOIS", 
                                                                         "BOWL", "BREC", "BRIS", "BUFF", "BURB", "BURN", "BUTL", "BYAR", 
                                                                         "CAMA", "CENT", "CHAN", "CHER", "CHEY", "CHIC", "CLAY", "CLOU", 
                                                                         "COOK", "COPA", "DURA", "ELRE", "ERIC", "EUFA", "FAIR", "FORA", 
                                                                         "FREE", "FTCB", "GOOD", "GUTH", "HASK", "HINT", "HOBA", "HOLL", 
                                                                         "HOOK", "HUGO", "IDAB", "JAYX", "KENT", "KETC", "LAHO", "LANE", 
                                                                         "MADI", "MANG", "MARE", "MAYR", "MCAL", "MEDF", "MEDI", "MIAM", 
                                                                         "MINC", "MTHE", "NEWK", "NINN", "NOWA", "OILT", "OKEM", "OKMU", 
                                                                         "PAUL", "PAWN", "PERK", "PRYO", "PUTN", "REDR", "RETR", "RING", 
                                                                         "SALL", "SEIL", "SHAW", "SKIA", "SLAP", "SPEN", "STIG", "STIL", 
                                                                         "STUA", "SULP", "TAHL", "TALI", "TIPT", "TISH", "VINI", "WASH", 
                                                                         "WATO", "WAUR", "WEAT", "WEST", "WILB", "WIST", "WOOD", "WYNO"
    ), class = "factor"), nlat = c(34.80833, 34.79851, 34.58722, 
                                   34.91418, 36.07204, 36.80253, 35.40185, 35.96305, 36.75443, 36.69256, 
                                   35.17156, 36.41201, 35.7805, 36.83129, 36.63459, 33.89376, 35.5915, 
                                   34.8497, 36.02866, 34.60896, 35.65282, 36.74813, 35.54615, 35.03236, 
                                   34.65657, 34.22321, 35.68001, 36.90987, 33.92075, 35.54848, 35.20494, 
                                   35.30324, 36.26353, 36.84053, 36.72562, 35.14887, 36.60183, 35.84891, 
                                   35.74798, 35.48439, 34.98971, 34.6855, 36.85518, 34.03084, 33.83013, 
                                   36.4821, 36.82937, 34.52887, 36.38435, 34.30876, 34.03579, 34.83592, 
                                   36.06434, 36.98707, 34.88231, 36.79242, 34.72921, 36.88832, 35.27225, 
                                   34.31072, 36.8981, 34.96774, 36.74374, 36.03126, 35.43172, 35.58211, 
                                   34.7155, 36.36114, 35.99865, 36.36914, 35.89904, 36.3559, 35.12275, 
                                   34.19365, 35.43815, 36.19033, 35.36492, 36.4153, 36.59749, 35.54208, 
                                   35.26527, 36.12093, 34.87642, 34.5661, 35.97235, 34.7107, 34.43972, 
                                   34.33262, 36.77536, 34.98224, 35.84185, 34.16775, 35.5083, 36.011, 
                                   34.90092, 34.98426, 36.42329, 36.51806), elon = c(-98.02325, 
                                                                                     -96.66909, -99.33808, -98.29216, -99.90308, -100.53012, -99.05847, 
                                                                                     -95.86621, -97.25452, -102.49713, -96.63121, -97.69394, -96.35404, 
                                                                                     -99.64101, -96.81046, -97.26918, -99.27059, -97.0033, -99.34652, 
                                                                                     -96.33309, -96.80407, -98.36274, -99.7279, -97.91446, -95.32596, 
                                                                                     -95.2487, -94.84896, -95.88553, -96.32027, -98.03654, -99.80344, 
                                                                                     -95.65707, -98.49766, -96.42777, -99.14234, -98.46607, -101.6013, 
                                                                                     -97.47978, -95.64047, -98.48151, -99.05283, -99.83331, -101.22547, 
                                                                                     -95.54011, -94.8803, -94.78287, -102.8782, -97.76484, -98.11139, 
                                                                                     -95.99716, -96.94394, -99.42398, -97.21271, -99.01109, -95.78096, 
                                                                                     -97.74577, -98.56936, -94.84437, -97.95553, -94.82275, -96.91035, 
                                                                                     -97.95202, -95.60795, -96.49749, -96.26265, -95.91473, -97.22924, 
                                                                                     -96.76986, -97.04831, -95.27138, -98.96038, -97.15306, -99.36001, 
                                                                                     -97.58812, -94.79805, -99.0403, -96.94822, -96.03706, -100.26192, 
                                                                                     -97.34146, -95.18116, -97.09527, -96.06982, -96.95048, -94.98671, 
                                                                                     -95.01152, -99.13755, -96.67895, -95.22094, -97.52109, -98.52615, 
                                                                                     -97.98815, -98.77509, -94.64496, -95.34805, -94.68778, -99.41682, 
                                                                                     -96.34222), elev = c(397L, 295L, 416L, 440L, 719L, 758L, 511L, 
                                                                                                          184L, 304L, 1267L, 281L, 352L, 239L, 559L, 301L, 228L, 520L, 
                                                                                                          345L, 589L, 208L, 291L, 362L, 694L, 328L, 186L, 221L, 299L, 250L, 
                                                                                                          197L, 419L, 603L, 200L, 405L, 330L, 530L, 422L, 997L, 330L, 183L, 
                                                                                                          493L, 478L, 497L, 912L, 175L, 110L, 304L, 1322L, 341L, 396L, 
                                                                                                          181L, 232L, 460L, 327L, 555L, 230L, 332L, 487L, 247L, 430L, 284L, 
                                                                                                          366L, 356L, 206L, 255L, 263L, 205L, 291L, 283L, 292L, 201L, 589L, 
                                                                                                          293L, 538L, 283L, 157L, 545L, 328L, 282L, 774L, 373L, 173L, 272L, 
                                                                                                          256L, 320L, 290L, 204L, 387L, 268L, 236L, 345L, 517L, 283L, 538L, 
                                                                                                          348L, 199L, 143L, 625L, 269L)), .Names = c("stid", "nlat", "elon", 
                                                                                                                                                     "elev"), class = "data.frame", row.names = c(NA, -98L))
    
    
    
    
    #### MAPPING #####
    
    station_use <- input$s_selection
    
    station_location2  <- if (station_use == "" || is.null(station_use)){
      station_location
    }else{
      station_location[station_location$stid == station_use ,]
    }
    
    m <- leaflet() %>%
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = mean(station_location$elon), lat = mean(station_location$nlat), zoom = 6) %>%
      addCircles(color = "#FF0000",lng=station_location2$elon, lat=station_location2$nlat, popup=station_location2$stid)
    return(m)
  })
  
  output$data  = renderDT(

    data()[,-c("Day","Month","Year")], options = list(pageLength = 50)
  
  )
  
})
