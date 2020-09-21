#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggrepel)
library(sf)
library(colourpicker)
library(viridis)
library(fastmap)
library(adegenet)
library(ggsci)
library(ggrepel)
#library(shinyjs)

allowed <- c(greypal(8),
             redpal(8),
             c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026'),
             c('#f7fcfd','#e0ecf4','#bfd3e6','#9ebcda','#8c96c6','#8c6bb1','#88419d','#6e016b'), #colorBrewer-Purple-ish
             c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177'),
             greenpal(8),
             c('#f7fcfd','#e5f5f9','#ccece6','#99d8c9','#66c2a4','#41ae76','#238b45','#005824'), #colorBrewer-Green
             c('#f7fcf0','#e0f3db','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#08589e'), #colorBrewer-Green-ish
             c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#0c2c84'),
             funky(8),
             c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f','#bf5b17','#666666'),
             c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d','#666666'),
             c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00'),
             rev(viridis(8)),
             rev(magma(8)),
             rev(inferno(8)),
             rev(plasma(8)),
             pal_aaas("default")(8),
             pal_d3("category10")(8),
             rev(pal_futurama("planetexpress")(8)),
             pal_npg("nrc")(8),
             pal_rickandmorty("schwifty")(8),
             pal_uchicago("default")(8))

world <- ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))


df <- data.frame(Lat = c(), Lon = c())
ui <- fluidPage(
  
  useShinyjs(),
  
   # Application title
  title = "prettyMap",
  
  headerPanel("prettyMap"),
  "Easily create a minimal, publication-ready map. Built on ggplot2 and other tidyverse packages. Written by ",tags$a(href="https://gibsonmatt.github.io/", "Matt Gibson"),
   hr(),
      

  
  fluidRow(
    
    column(2,
           tabsetPanel(
             tabPanel("Input data",br(),
              numericInput("numPoints", "Number of Points",
                        value = 1),hr(),
              uiOutput("Dynamic")
           ),
           tabPanel("Upload data",br(),
                    uiOutput("file_input_ui"),
                    "Each point should be on its own row. Header required (name, group, latitude, longitude; in any order).", br(),
                    actionButton("reset", "Reset"))
           )),
    column(4, 
           tabsetPanel(selected = "Appearance",
             tabPanel("Groups",
              column(12, 
                textInput(inputId = "catName",label =  "Category Name", value = "Group")),
              column(6,
               colourpicker::colourInput(
                 "groupColor1", "Group 1",
                 palette = "limited",
                 allowedCols = allowed, value = funky(8)[1]),
               colourpicker::colourInput(
                 "groupColor2", "Group 2",
                 palette = "limited",
                 allowedCols = allowed, value = funky(8)[2]),
               colourpicker::colourInput(
                 "groupColor3", "Group 3",
                 palette = "limited",
                 allowedCols = allowed, , value = funky(8)[3]),
               colourpicker::colourInput(
                 "groupColor4", "Group 4",
                 palette = "limited",
                 allowedCols = allowed, , value = funky(8)[4]),
               colourpicker::colourInput(
                 "groupColor5", "Group 5",
                 palette = "limited",
                 allowedCols = allowed, , value = funky(8)[5]),
               colourpicker::colourInput(
                 "groupColor6", "Group 6",
                 palette = "limited",
                 allowedCols = allowed, , value = funky(8)[6]),
               colourpicker::colourInput(
                 "groupColor7", "Group 7",
                 palette = "limited",
                 allowedCols = allowed, , value = funky(8)[7]),
               colourpicker::colourInput(
                 "groupColor8", "Group 8",
                 palette = "limited",
                 allowedCols = allowed, value = funky(8)[8])),
              column(6, textInput("groupName1", value = "Group 1", label = "Name"),
                        textInput("groupName2", value = "Group 2", label = "Name"),
                        textInput("groupName3", value = "Group 3", label = "Name"),
                        textInput("groupName4", value = "Group 4", label = "Name"),
                        textInput("groupName5", value = "Group 5", label = "Name"),
                        textInput("groupName6", value = "Group 6", label = "Name"),
                        textInput("groupName7", value = "Group 7", label = "Name"),
                        textInput("groupName8", value = "Group 8", label = "Name"))
            ),
            
            tabPanel("Appearance",
                     br(),
                     column(6,
                     wellPanel(
                      sliderInput('zoom', 'Window Size', min = 0, max =  25, value =  1.5, step = 0.1),
                      checkboxInput("scale",label = "Show Scale", value = TRUE),
                      checkboxInput("arrow",label = "Show Arrow", value = TRUE),
                      sliderInput("axesSize", "Axes Label Size",min = 10, max  = 24, value = 12, step = 0.5),
                      sliderInput("axesTextSize", "Axes Text Size",min = 5, max  = 24, value = 7, step = 0.5),
                      sliderInput("gridThickness", "Grid Thickness",min = 0, max  = 1, value = 0.3, step = 0.1),
                      sliderInput("outlineWidth", "Landmass Outline Width",min = 0, max  = 1, value = 0.4, step = 0.05)),
                     wellPanel(
                       colourpicker::colourInput(
                         "landColor", "Land Color",
                          value = "antiquewhite"),
                       colourpicker::colourInput(
                         "waterColor", "Water Color",
                         value = "aliceblue"),
                       actionButton("resetColor", "Reset"))
                     ),
                     column(6,
                     wellPanel(
                       sliderInput("pointSize", "Point Size", min = 0.5, max = 12, value = 3,step = 0.1),
                       checkboxInput("pntLabels",label = "Label Points", value = FALSE),
                       sliderInput("pntPadding", "Label Padding", min = 0, max = 4, value = 0.7,step = 0.1),
                       sliderInput("lineSize", "Line Thickness", min = 0, max = 1.5, value = 0.3,step = 0.1),
                       sliderInput("labelBorder", "Label Border", min = 0, max = 1, value = 0.3,step = 0.1),
                       sliderInput("labelTextSize", "Label Size", min = 0, max = 12, value = 3,step = 0.1)),
                     wellPanel(
                       checkboxInput("showLegend","Show Legend", value = T),
                       selectInput("legendPos","Legend Position",choices=c("Top" =  "top", "Bottom"="bottom",  "Right" ="right", "Left" = "left", "Other"="other")),
                       sliderInput("legendPosX", "X Position", min = 0,max=1,value = 0.85),
                       sliderInput("legendPosY", "Y Position", min = 0, max = 1, value = 0.85),
                       checkboxInput("legendBorder","Legend Border", value = T)
                     ))
                     
                     ),
            tabPanel("Download",br(),
                       column(4,
                       downloadButton("downloadPlot","Download PDF")
                     ),
                      column(4, numericInput("width",  "Width", value  = 4.75)),
                      column(4, numericInput("height",  "Height", value  = 4.5)))
           )
           ),
    column(6,
           plotOutput("map")
           #dataTableOutput('table'))
)))




server <- function(input, output, session) {

  output$Dynamic <- renderUI({
      LL <- vector("list",input$numPoints)       
    for(i in 1:input$numPoints){
      LL[[i]] <- list(textInput(inputId = paste0("name",i), label = "Name"),
                      numericInput(inputId = paste0("lat",i), label = "Latitude", value = -0.65), 
                      numericInput(inputId = paste0("lon",i), label = "Longitude",value =-90.4), 
                      selectInput(inputId= paste0('groupP',i), label = "Group", choices=seq(1,8)),
                      hr())
    }
    return(LL)                     
  })
  

  
  
  output$table <- renderDataTable({
    a <- reactiveValuesToList(input)
    name <- t(as.vector(a[paste0("name",1:input$numPoints)]))
    lat <- t(as.vector(a[paste0("lat",1:input$numPoints)]))
    lon <- t(as.vector(a[paste0("lon",1:input$numPoints)]))
    group <- t(as.vector(a[paste0("groupP",1:input$numPoints)]))
    
    df <- data.frame(names = t(name), lat = t(lat), lon = t(lon), group = t(group),  row.names = NULL)
    return(df)
  })
  
  output$map <- renderPlot(res = 150, width= 700, height=700,{makePlot()})
  
  output$downloadPlot <- downloadHandler(
    filename = function() { "prettyMap_output.pdf" },
    content = function(file) {
      pdf(file,input$width, input$height)
      print(makePlot())
      dev.off()
    })
  

  
  output$file_input_ui <- renderUI({
    datastate$data <- T
    fileInput("file1", "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
  })
  
  datastate <- reactiveValues(data = F)
  
  observeEvent(input$reset, {
    reset('file1')
    datastate$data <- F
  })
  
  observeEvent(input$resetColor, {
    colourpicker::updateColourInput(session, "waterColor", value = "aliceblue")
    colourpicker::updateColourInput(session, "landColor", value = "antiquewhite")
  })
  
  makePlot <- reactive({
      a <- reactiveValuesToList(input)
      print(input$file1)
      if (datastate$data == F){
        
        #Get point info
        name <- t(as.vector(a[paste0("name",1:input$numPoints)]))
        lat <- t(as.vector(a[paste0("lat",1:input$numPoints)]))
        lon <- t(as.vector(a[paste0("lon",1:input$numPoints)]))
        group <- t(as.vector(unlist(a[paste0("groupP",1:input$numPoints)])))
        df <- data.frame(name = t(name), lat = as.numeric(t(lat)), lon = as.numeric(t(lon)), group = as.vector(t(group)),groupColor = rep("gray",input$numPoints), row.names = NULL)
        }else{
        df <- read.csv(input$file1$datapath)
        df$groupColor <- rep("gray",dim(df)[1])
        
        if (!"name" %in% names(df) & !"Name" %in% names(df) & !"names" %in% names(df) & !"Names" %in% names(df)){
          stop("No name column?")
        }
        
        if (!"lat" %in% names(df) & !"Lat" %in% names(df) & !"latitude" %in% names(df) & !"Latitude" %in% names(df)){
          stop("No latitude column?")
        }
        
        if (!"lon" %in% names(df) & !"Lon" %in% names(df) & !"longitude" %in% names(df) & !"Longitude" %in% names(df)){
          stop("No longitude column?")
        }
        
        if (!"group" %in% names(df) & !"Group" %in% names(df) & !"groups" %in% names(df) & !"Groups" %in% names(df)){
          stop("No groups column?")
        }
        
        #std col names
        names(df) <- str_replace(names(df), "Name", "name")
        names(df) <- str_replace(names(df), "names", "name")
        names(df) <- str_replace(names(df), "Names", "name")
        
        names(df) <- str_replace(names(df), "Lat", "lat")
        names(df) <- str_replace(names(df), "latitude", "lat")
        names(df) <- str_replace(names(df), "Latitude", "lat")
        
        names(df) <- str_replace(names(df), "Lon", "lon")
        names(df) <- str_replace(names(df), "longitude", "lon")
        names(df) <- str_replace(names(df), "Longitude", "lon")
      
        names(df) <- str_replace(names(df), "Group", "group")
        names(df) <- str_replace(names(df), "Groups", "group")
        names(df) <- str_replace(names(df), "groups", "group")  
        }
    
      #Get group info
      groupNames <- t(as.vector(unlist(a[paste0("groupName",1:8)])))
      groupsUsed <- paste0("groupName",unique(df$group))
      groupNames <- as.data.frame(groupNames)
      names(groupNames) <- paste0("groupName",1:8)
      groupColors <- t(as.vector(unlist(a[paste0("groupColor",1:8)])))
      groupColors <- as.data.frame(groupColors)

      mapColor <- fastmap()
      for(x in 1:8){
        mapColor$set(as.character(x), groupColors[,x])
      }
      mappingColor <- function(x){
        return(mapColor$get(as.character(x)))
      }
      df$groupColor <-  sapply(df$group,FUN = mappingColor)
      
      
      map <- fastmap()
      for(x in 1:8){
        map$set(as.character(x), groupNames[,x])
      }
      mapping <- function(x){
        return(map$get(as.character(x)))
      }
      
      df$group <-  sapply(df$group,FUN = mapping)
      
      
      zoom <- input$zoom
      
      
      centroidX <- mean(df$lon)
      centroidY <- mean(df$lat)
      
      max_dist_to_centroid_x <- max(abs(df$lon - centroidX))
      max_dist_to_centroid_y <- max(abs(df$lat - centroidY))
      
     # updateSliderInput(session,inputId = "zoom", value = max(max_dist_to_centroid_x, max_dist_to_centroid_y))
      
      win <- max(max_dist_to_centroid_x, max_dist_to_centroid_y)
      
      xlim  <- c(centroidX-win-zoom, centroidX+win+zoom)
      ylim <- c(centroidY-win-zoom, centroidY+win+zoom)
      

    p <- ggplot(data = world) + geom_sf(fill= input$landColor, lwd= input$outlineWidth) + coord_sf(xlim = xlim, ylim = ylim)  +
      geom_point(data = df, aes(y= lat, x =  lon, fill=group), size = input$pointSize, colour="black", shape = 21) + scale_fill_manual(values  = unique(as.vector(df$groupColor)))+
      theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = input$gridThickness), panel.background = element_rect(fill = input$waterColor)) +
      ylab("Latitude") +  xlab("Longitude") + theme(axis.title=element_text(size=input$axesSize), axis.text=element_text(size=input$axesTextSize)) + labs(fill = input$catName)
    
    if(input$legendPos!="other"){
      p<- p+ theme(legend.position = input$legendPos, legend.key = element_rect(fill = NA, colour = NA, size = 0.25))
    }else{
      p<- p+ theme(legend.position = c(input$legendPosX,input$legendPosY),legend.key = element_rect(fill = NA, colour = NA, size = 0.25))
    }
    
    if(input$legendBorder){
      p <- p +theme(legend.box.background = element_rect(color="black", size=1))
    }
    
    if (input$scale){p <- p + annotation_scale(location = "br", width_hint = 0.2)}
    if (input$arrow){p <- p + annotation_north_arrow(location = "br", which_north = "true", 
                                                            pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                                                            style = north_arrow_fancy_orienteering)}
    if (!input$showLegend){p<-p+theme(legend.position = 'none')}
    print(df)
    if(input$pntLabels){p <- p +  geom_label_repel(data = df, mapping = aes(y=lat, x=lon, label=name), box.padding = input$pntPadding, segment.size = input$lineSize, size = input$labelTextSize, label.size = input$labelBorder)}
     return(p)
    })
    
    observe({if(!input$pntLabels){
      disable("pntPadding")
      disable("lineSize")
      disable("labelBorder")
      disable("labelTextSize")
    }else{
      enable("pntPadding")
      enable("lineSize")
      enable("labelBorder")
      enable("labelTextSize")
    }
    if (!input$showLegend){
      disable("legendPosX")
      disable("legendPosY")
      disable("legendPos")
      disable("legendBorder")
    }else{
      enable("legendPos")
      enable("legendBorder")
      if (input$legendPos=="other"){
        enable("legendPosX")
        enable("legendPosY")
      }else{
        disable("legendPosX")
        disable("legendPosY")
      }
    }
      
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

