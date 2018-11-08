options(shiny.maxRequestSize=50*1024^2)
library(shiny)
library(leaflet)
library(dplyr)
library(plyr)

# Define server
shinyServer(function(input, output, session) {

  # Create an output variable for description about App
  output$text <- renderUI({
    
    HTML("This project uses the dataset 'Global Significant Earthquakes 2000-2018' collected from https://www.ngdc.noaa.gov/.<br/>
<br/>
    The dataset contains information including:<br/>
<ul>
    <li>Earthquake Info:</li>
    date : earthquake date<br/>
    hour/min/sec : specific time(hour, minute and second)<br/>
    <br/>
    <li>Earthquake Parameters:</li>
    loc_name : location<br/>
    dep : focal depth<br/>
    mag : magnitude<br/>
    tsu/vol : associated with tsunami or volcano<br/>
    <br/>
    <li>Earthquake Effects:</li>
    death_num/death_de : number/degree of death<br/>
    injury_num/injury_de : number/degree of injury<br/>
    damage_mil/damage_de : amount(million)/degree of damage<br/>
    house_destroyed_num/house_destroyed_de : number/degree of house destroyed<br/>
    house_damaged_num/house_damaged_de : number/degree of house damaged<br/>
    <br/>
    <li>Degree Details:</li>
    *damage_de*:<br/>
    0 = NONE<br/>
    1 = LIMITED (roughly corresponding to less than $1 million)<br/>
    2 = MODERATE (~$1 to $5 million)<br/>
    3 = SEVERE (~>$5 to $24 million)<br/>
    4 = EXTREME (~$25 million or more)<br/>
    *others*:<br/>
    0 = None<br/>
    1 = Few (~1 to 50)<br/>
    2 = Some (~51 to 100)<br/>
    3 = Many (~101 to 1000)<br/>
    4 = Very Many (~1001 or more)<br/>
</ul>
    Problem Solved: Discover patterns of global significant earthquakes after 2000, see details of earthquakes in a more direct way.<br/>
    This question is a great interest to local governments and environment organizations.")
  })
  
  # 1. Create a descriptive table for data summary
  output$table1 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return("Please Upload A File For Analysis")
    
    tryCatch(
      {
        # Read in the file
        df <- read.csv(inFile$datapath)
        attach(df)
        # Change categorical variables into factor
        df$tsu <- as.factor(df$tsu)
        df$vol <- as.factor(df$vol)
        df$death_de <- as.factor(df$death_de)
        df$injury_de <- as.factor(df$injury_de)
        df$damage_de <- as.factor(df$damage_de)
        df$house_destroyed_de <- as.factor(df$house_destroyed_de)
        df$house_damaged_de <- as.factor(df$house_damaged_de)
        
        # Update sliders' max value
        updateSliderInput(session, "dep", max = max(df$dep,na.rm = T))
        
        # Prepare for subsetting
        date_from <- as.Date(input$date[1])
        date_to <- as.Date(input$date[2])
        mag_max <- input$mag[2]
        mag_min <- input$mag[1]
        dep_max <- input$dep[2]
        dep_min <- input$dep[1]
        event <- c(input$tsu_vol)
        pop_not <- input$pop
        newdf <- filter(df, 
                        as.Date(date) < date_to & as.Date(date) > date_from &
                        mag <= mag_max & mag >= mag_min &
                        dep <= dep_max & dep >= dep_min)
        if ("Tsunami" %in% event & "Volcano" %in% event){
          newdf <- filter(newdf, tsu == 1 & vol == 1)
        } else if ("Volcano" %in% event) {
          newdf <- filter(newdf, vol == 1)
        } else if ("Tsunami" %in% event) {
          newdf <- filter(newdf, tsu == 1)
        }
        
      },
      
      error = function(e) {
        # Return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    cat("Total Earthquakes Selected: ", nrow(newdf), "\n\n")
    cat("Tsunami:", "Associated", summary(newdf$tsu)[1], ", NA",summary(newdf$tsu)[2], "\n")
    cat("Volcano:", "Associated", summary(newdf$vol)[1], ", NA",summary(newdf$vol)[2], "\n\n")
    
    cat("Summary of Earthquake Effect(by degree): \n")
    print(summary(newdf[, c(13,15,17,19,21)]))
    cat("\n")
    cat("Summary of Earthquake Effect(by number): \n")
    print(summary(newdf[, c(12,14,16,18,20)]))
  })
  
  
  # 2. Create a descriptive table for top 5 value data set
  output$table2 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return("Please Upload A File For Analysis")
    
    tryCatch(
      {
        # Read in the file
        df <- read.csv(inFile$datapath)
        attach(df)
        # Change categorical variables into factor
        df$tsu <- as.factor(df$tsu)
        df$vol <- as.factor(df$vol)
        df$death_de <- as.factor(df$death_de)
        df$injury_de <- as.factor(df$injury_de)
        df$damage_de <- as.factor(df$damage_de)
        df$house_destroyed_de <- as.factor(df$house_destroyed_de)
        df$house_damaged_de <- as.factor(df$house_damaged_de)
        
        # Update sliders' max value
        updateSliderInput(session, "dep", max = max(df$dep,na.rm = T))
        
        # Prepare for subsetting
        date_from <- as.Date(input$date[1])
        date_to <- as.Date(input$date[2])
        mag_max <- input$mag[2]
        mag_min <- input$mag[1]
        dep_max <- input$dep[2]
        dep_min <- input$dep[1]
        event <- c(input$tsu_vol)
        pop_not <- input$pop
        newdf <- filter(df, 
                        as.Date(date) < date_to & as.Date(date) > date_from &
                          mag <= mag_max & mag >= mag_min &
                          dep <= dep_max & dep >= dep_min)
        if ("Tsunami" %in% event & "Volcano" %in% event){
          newdf <- filter(newdf, tsu == 1 & vol == 1)
        } else if ("Volcano" %in% event) {
          newdf <- filter(newdf, vol == 1)
        } else if ("Tsunami" %in% event) {
          newdf <- filter(newdf, tsu == 1)
        }
        
      },
      error = function(e) {
        # Return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    cat("Top 5 Frequent Areas: \n")
    top_area <- t(t(head(summary(newdf$loc_name), 5)))
    print(top_area)
    cat("\n")
    
    cat("Top 5 Magnitude: \n")
    print(head(newdf[order(newdf$mag, decreasing = TRUE),][, c(1,2,3,4,7,11)], 5))
    cat("\n")
    
    cat("Top 5 Focal Depth: \n")
    print(head(newdf[order(newdf$dep, decreasing = TRUE),][, c(1,2,3,4,7,10)], 5))
    cat("\n")
    
    cat("Top 5 Death Number: \n")
    print(head(newdf[order(newdf$death_num, decreasing = TRUE),][, c(1,2,3,4,7,12)], 5))
    cat("\n")
    
    cat("Top 5 Injury Number: \n")
    print(head(newdf[order(newdf$injury_num, decreasing = TRUE),][, c(1,2,3,4,7,14)], 5))
    cat("\n")
    
    cat("Top 5 Damage(million): \n")
    print(head(newdf[order(newdf$damage_mil, decreasing = TRUE),][, c(1,2,3,4,7,16)], 5))
    cat("\n")
    
    cat("Top 5 House Destroyed Number: \n")
    print(head(newdf[order(newdf$house_destroyed_num, decreasing = TRUE),][, c(1,2,3,4,7,18)], 5))
    cat("\n")
    
    cat("Top 5 House Damaged Number: \n")
    print(head(newdf[order(newdf$house_damaged_num, decreasing = TRUE),][, c(1,2,3,4,7,20)], 5))
    cat("\n")
  })
  
  # 3. Create plots for visualization
  output$plot <- renderPlot({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return(NULL)
    
    tryCatch(
      {
        # Read in the file
        df <- read.csv(inFile$datapath)
        attach(df)
        # Change categorical variables into factor
        df$tsu <- as.factor(df$tsu)
        df$vol <- as.factor(df$vol)
        df$death_de <- as.factor(df$death_de)
        df$injury_de <- as.factor(df$injury_de)
        df$damage_de <- as.factor(df$damage_de)
        df$house_destroyed_de <- as.factor(df$house_destroyed_de)
        df$house_damaged_de <- as.factor(df$house_damaged_de)
        
        # Update sliders' max value
        updateSliderInput(session, "dep", max = max(df$dep,na.rm = T))
        
        # Prepare for subsetting
        date_from <- as.Date(input$date[1])
        date_to <- as.Date(input$date[2])
        mag_max <- input$mag[2]
        mag_min <- input$mag[1]
        dep_max <- input$dep[2]
        dep_min <- input$dep[1]
        event <- c(input$tsu_vol)
        newdf <- filter(df, 
                        as.Date(date) < date_to & as.Date(date) > date_from &
                          mag <= mag_max & mag >= mag_min &
                          dep <= dep_max & dep >= dep_min)
        if ("Tsunami" %in% event & "Volcano" %in% event){
          newdf <- filter(newdf, tsu == 1 & vol == 1)
        } else if ("Volcano" %in% event) {
          newdf <- filter(newdf, vol == 1)
        } else if ("Tsunami" %in% event) {
          newdf <- filter(newdf, tsu == 1)
        }
        
      },
      error = function(e) {
        # Return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    # Format plots and show plots
    par(mfrow=c(2,2))
    hist(newdf$mag, main = "Histogram of Magnitude", xlab = "Magnitude(0-9.9)", col = "skyblue")
    hist(newdf$dep, main = "Histogram of Focal Depth", xlab = "Focal Depth(0-700km)", col = "skyblue")
    boxplot(newdf$mag ~ newdf$death_de, main = "Boxplot of Death Degree", xlab = "Death Degree", col = "hotpink")
    boxplot(newdf$mag ~ newdf$injury_de, main = "Boxplot of Injury Degree", xlab = "Injury Degree", col = "hotpink")
  })
  
  # 4. Create a map output variable
  output$map <- renderLeaflet({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return(NULL)
    
    # Read input file
    df <- read.csv(inFile$datapath)
    attach(df)
    
    # Prepare for subsetting
    date_from <- as.Date(input$date[1])
    date_to <- as.Date(input$date[2])
    mag_max <- input$mag[2]
    mag_min <- input$mag[1]
    dep_max <- input$dep[2]
    dep_min <- input$dep[1]
    event <- c(input$tsu_vol)
    pop_not <- input$pop
    # Filter the data
    map_df <- filter(df, 
                    as.Date(date) < date_to & as.Date(date) > date_from &
                      mag <= mag_max & mag >= mag_min &
                      dep <= dep_max & dep >= dep_min)
    if ("Tsunami" %in% event & "Volcano" %in% event){
      map_df <- filter(map_df, tsu == 1 & vol == 1)
    } else if ("Volcano" %in% event) {
      map_df <- filter(map_df, vol == 1)
    } else if ("Tsunami" %in% event) {
      map_df <- filter(map_df, tsu == 1)
    }
    
    # Create colors with a continuous color function
    color <- colorBin("Spectral", c(0.0, 9.9), bins = c(0:10), pretty = FALSE,
             na.color = NA, reverse = TRUE,
             right = FALSE)
    
    # Create popup content
    content <- paste(map_df$loc_name,"<br/>",
                     map_df$date,"  ",map_df$hour,":",map_df$min,"<br/>",
                     "Magnitude:",map_df$mag,"<br/>",
                     "Focal Depth:",map_df$dep)
    
    # Create the leaflet function for data
    leaflet(map_df) %>%
      
      # Set the default view
      setView(lng = -20, lat = 37.45, zoom = 2) %>%
      
      # Provide tiles
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      
      # Add circles: radius and color based on magnitude
      addCircleMarkers(
        radius = map_df$mag,
        lng = map_df$long,
        lat = map_df$lat,
        stroke = FALSE,
        fillOpacity = 0.9,
        color=color(mag), popup = ifelse(pop_not,content,FALSE)
      ) %>%
      
      # Add legends
      addLegend(
        "bottomleft",
        pal = color,
        values = ~mag,
        opacity = 1,
        title = "Earthquake Magnitude(0-9.9)"
      )
  })
  
})