################
# Libraries ####
################

library(bcdata)
library(leaflet)
library(dplyr)
library(ggnewscale)
library(shiny)
library(sf) 
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(rematch)
library(shiny)
library(shinymanager)

cred <- read.csv("cred.csv")

credentials <- data.frame(
  user = c("guest"),
  password = c(names(cred)[1]),
  admin = c(T),
  comment = "Simple and secure authentification mechanism
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE)

all_ws <- st_read("data/watershed_watershed_per_pt_whole_poly_simple.gpkg")
full_ws <- arrange(all_ws, -area_m2)[1,]

#####################
# USER INTERFACE ####
#####################

ui <- navbarPage(theme = "css/bcgov.css", title = "Shiny Watershed DRAFT",
  
  ## PAGE 1 ####
  
  tabPanel("Map", 
           
           ### 1st ROW ####
           fluidRow(
             
             #### 1st COL ####
             
             column(width = 2, 
                    
                    h3("Getting started"),
                    helpText("Click anywhere in the red polygon to begin.."),
                    
                    h3("Performance"),
                    sliderTextInput("speed","Speed" , 
                                    choices = c("Slow (high res)", "Medium", "Fast (low res)"), 
                                    selected = c("Medium")),
                    sliderInput("years","Years",min = 1900, max = as.numeric(format(Sys.Date(),"%Y")), value = c(1985, as.numeric(format(Sys.Date(),"%Y"))), 
                                ticks = F, sep = "", step = 5),
                    h3("Watershed:"),
                    checkboxInput("ws_area", label = "Area", value = T),
                    checkboxInput("ws_lakes", label = "Lakes", value = T),
                    checkboxInput("ws_wetlands", label = "Wetlands", value = T),
                    checkboxInput("ws_glaciers", label = "Glaciers", value = T),
                    h3("Terrain:"),
                    checkboxInput("ter_stats", label = "Terrain statistics", value = T),
                    checkboxInput("ter_prof", label = "Stream profile", value = T),
                    checkboxInput("ter_hyps", label = "Watershed hypsometry", value = T),
                    h3("BEC:"),
                    checkboxInput("bec", label = "BEC Zone Stats", value = T),
                    h3("Forest:"),
                    checkboxInput("cutblocks", label = "Cutblocks", value = T),
                    checkboxInput("wildfire", label = "Wildfire", value = T),
                    checkboxInput("pests", label = "Pests", value = T),
                    h3("Roads"),
                    checkboxInput("dra", label = "Digital Road Atlas", value = T),
                    h3("Equivalent Clearcut Area"),
                    numericInput(inputId = "road_buf", label = "Road buffer (0-100 m)", min = 0, max = 100, value = 10)

                    # selectInput("nr_region",
                    #             label = "Go to natural resource region",
                    #             choices = as.list(c("none", as.character(bcmaps::nr_regions()$ORG_UNIT))),
                    #             selected = "none"),
                    # 
                    # checkboxInput("cutblocks", label = "Cutblocks", value = T),
                    # 
                    # checkboxInput("wildfire", label = "Wildfire", value = FALSE),
                    # 
                    # checkboxInput("roads", label = "Roads", value = FALSE)
                    ),
             
             #### 2nd COL ####
             
             column(width = 8, 
                    
                    h1("BC 'Watershed Summary Tool' DRAFT!!!!"), 
                    
                    leafletOutput("mymap",height = '500px'),
                    
                    tableOutput('table'),
                    
                    plotlyOutput("timeseriesPlot", height = 300),
                    
                    plotlyOutput("timeseriesPlotECA", height = 300),
                    
                    plotOutput("ggMap", height = 600),

                    plotOutput("profile", height = 600),
                    
                    verbatimTextOutput("mymap_click"))#,
             
             #### 3rd COL ####

             # column(width = 2, 
             #        
             #        actionButton("zoom","Zoom!", width = "100%"))
             ),
           
           ### FOOTER #### 
           
           fluidRow(
             
             column(width = 10,
                    
                    style = "background-color:#003366; border-top:2px solid #fcba19;",
                      tags$footer(class="footer",
                                  tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                           tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                           tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                           tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                           tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                           tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                           tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                           tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                           )))))),
           
           ## PAGE 2 #### 
  tabPanel(HTML("About"),
           fluidRow(
             column(10, offset = 1, htmlOutput("about"))
             )
  ))



#############
# SERVER ####
#############

server <- function(input, output, session) {
  
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials))

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)})
  
  # INITIAL LEAFLET MAP ####
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addPolygons(data= full_ws %>% st_transform(4326), fillOpacity = 0, color = "red") %>%
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      addMeasure()
    })

  # ON CLICK... DO... ####
  observeEvent(input$mymap_click, {

    # PROGRESS BAR ####
    withProgress(message = 'Processing', value = 0, max = 5, {

      # CLICK COORDS ####
      point <- input$mymap_click
      print(point)
      
      # dir.create("data")
      # COORDS TO POINT ####
      # point_sf <- st_as_sf(data.frame(lat=54.19882,lng=-121.4054), coords=c("lng","lat"), crs=4326) %>% st_transform(3005)
      point_sf <- st_as_sf(data.frame(lat=point$lat,lng=point$lng), coords=c("lng","lat"), crs=4326) %>% st_transform(3005)
      # st_write(point_sf, "data/point_sf.shp", delete_layer = T)
     
      # GET (SMALLEST) NAMED WATERSHED FOR DEM DOWNLOAD AOI ####
      incProgress(1/5, detail = "Getting watershed")
      new_ws <- all_ws %>% 
        filter(st_intersects(point_sf,all_ws, sparse = F)[1,]) %>% 
        filter(area_m2 == min(.$area_m2))
      
      
      # FRESHWATER ATLAS 
      bcdc_search("stream network")
      ntwk <- bcdc_query_geodata("75299593-3222-40f9-879f-29e9824fc978") %>% 
        filter(INTERSECTS(new_ws)) %>% 
        collect() %>% 
        st_intersection(new_ws)
      
      length <- ntwk %>% 
        st_drop_geometry() %>% 
        filter(!is.na(GAZETTED_NAME)) %>% 
        group_by(GAZETTED_NAME) %>% 
        summarise(length_km = sum(FEATURE_LENGTH_M, na.rm=T)/1000)
      
      output$table <- renderTable(length %>% arrange(-length_km) %>% head())
      
      # CUTBLOCKS ####
      incProgress(2/5, detail = "Download cutblocks...")
      df <- data.frame(type = c("Cutblock","Wildfire"), 
                       year = as.numeric(c(format(as.Date(Sys.Date()), "%Y"),
                                           format(as.Date(Sys.Date()), "%Y"))), 
                       area = c(0,0))
      # if(input.cutblocks == T){
      cb <- bcdata::bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
        filter(INTERSECTS(new_ws)) %>%
        select(HARVEST_YEAR) %>% 
        collect() 
      if(nrow(cb)!=0){
        cb <- st_intersection(cb,new_ws) %>% 
          mutate(type = "Cutblock", 
                 year = HARVEST_YEAR, 
                 area = as.numeric(st_area(.))/(1000*1000)) %>%
          select(type, year, area)
        df = bind_rows(df, cb %>% st_drop_geometry())
          }
          # }
      incProgress(3/5, detail = "Download wildfire")
      # if(input.wildfire == T){
          fire_cur <- bcdata::bcdc_query_geodata("cdfc2d7b-c046-4bf0-90ac-4897232619e1") %>%
              filter(INTERSECTS(new_ws)) %>%
              select(FIRE_YEAR) %>% 
              collect()
          fire_hist <- bcdata::bcdc_query_geodata("22c7cb44-1463-48f7-8e47-88857f207702") %>%
              filter(INTERSECTS(new_ws)) %>%
              select(FIRE_YEAR) %>% 
              collect()
            
          incProgress(4/5, detail = "Join disturbance...")
          if(nrow(fire_cur)!=0 & nrow(fire_hist)!=0){
            out <- bind_rows(fire_cur,fire_hist)}
          if(nrow(fire_cur)==0 & nrow(fire_hist)!=0){
            out <- fire_hist}
          if(nrow(fire_cur)!=0 & nrow(fire_hist)==0){
            out <- fire_cur}
          if(nrow(fire_cur)==0 & nrow(fire_hist)==0){
            out <- st_sf(st_sfc(NA, crs = 3005))
            out$FIRE_YEAR=NA}
          
          if(nrow(out)!=0){
            out <- st_intersection(out,new_ws) %>% 
              mutate(type = "Wildfire", 
                     year = FIRE_YEAR, 
                     area = as.numeric(st_area(.))/(1000*1000)) %>% 
              select(type, year, area)
            df = bind_rows(df, out %>% st_drop_geometry())
          }
          # }

        # OUTPUT UPDATE LEAFLET ####

        # r <- read_stars("data/flow_d8_accum.tif")>5
        # r[r==0] <- NA

          incProgress(5/5, detail = "Make plots...")
          
          colors <- c("red","blue","green","white")
          # qpal <- colorQuantile("Spectral", out$year, n = 7, alpha = F)
          
        output$mymap <- renderLeaflet({
          leaflet() %>%
            addProviderTiles(providers$Esri.WorldImagery) %>%
            addCircleMarkers(data=point_sf %>% st_transform(4326), group = "point_click", color = "yellow") %>%
            addPolygons(data= out %>% st_transform(4326), stroke = F, fillOpacity = 0.8, color = colors[1], group = "wildfire") %>%
            addPolygons(data= cb %>% st_transform(4326), stroke = F, fillOpacity = 0.8, color = colors[3], group = "cutblocks") %>%
            addPolygons(data= new_ws %>% st_buffer(1) %>% st_transform(4326), fillOpacity = 0, color = colors[4], group = "watershed_myclick") %>%
            
            addMeasure() %>%
            addLayersControl(overlayGroups = c("point_snap","watershed_myclick","cutblocks","wildfire"),
                             options = layersControlOptions(collapsed = T))
            })

        # OUTPUT GGPLOT HARVEST SUMMARIES ####
        output$timeseriesPlot <- renderPlotly({
          ggplotly(df %>%
            group_by(type, year) %>%
            summarize(area=sum(area,na.rm=T)) %>%
            mutate(area = signif(area, 3)) %>% 
            ggplot() +
            geom_col(aes(year,area, fill = type)) +
            theme_bw() +
            labs(x = "Year", y = "Harvest Area (km2)"#, 
                 # title = paste(ws$GNIS_NAME, "custom watershed")
                 ) + 
            scale_fill_manual(values = c("green","red")))
            })
        
        t <- df %>% 
          filter(!is.na(type)) %>% 
          mutate(ws_area = as.numeric(st_area(new_ws))/(1000*1000),
                 eca = case_when(type == "Wildfire" ~ 100,
                                 type == "Cutblock" ~ 100))
        
        e <- do.call(bind_rows, lapply(1:nrow(t), function(i=1){
          p <- t[i,]
          if(p$type=="Wildfire"){slope = 2}
          if(p$type=="Cutblock"){slope = 6}
          years <- (p$year+1):(lubridate::year(lubridate::now()))
          bind_rows(p, 
                    data.frame(year = years) %>% 
                      mutate(type = p$type, 
                             eca = p$eca-((year-p$year)*slope),
                             area = p$area, 
                             ws_area = p$ws_area)) %>% 
            mutate(eca = case_when(eca < 0 ~ 0, 
                                   TRUE ~ eca))}))
        
        output$timeseriesPlotECA <- renderPlotly({
          ggplotly(e %>%
            mutate(eca_area = (eca/100)*area,
                   eca_area_ws_perc = 100*(eca_area/ws_area)) %>% 
            group_by(type, year) %>%
            summarize(eca=sum(eca_area_ws_perc,na.rm=T)) %>%
            mutate(eca = signif(eca, 3)) %>% 
            ggplot() +
            geom_col(aes(year,eca, fill = type)) +
            theme_bw() +
            labs(x = "Year", y = "ECA (%)"#, 
                 # title = paste(ws$GNIS_NAME, "custom watershed")
                 )+ 
            scale_fill_manual(values = c("green","red")))
        })
        
        # my_stream_network <- bcdc_query_geodata("freshwater-atlas-stream-network") %>%
        #   filter(INTERSECTS(new_ws)) %>%
        #   collect()
        # 
        # streams <- lapply(rev(1:10), function(i=10){
        #   print(i)
        #   d <- bcdc_query_geodata("freshwater-atlas-stream-network") %>%
        #     filter(INTERSECTS(new_ws), STREAM_ORDER == i) %>% 
        #     collect() 
        #   if(nrow(d)>0){
        #     return(d)
        #     stop()}
        #   })
               
               
        
        output$ggMap <- renderPlot({
            ggplot() + 
              geom_sf(data = new_ws) + 
              geom_sf(data = out, color = NA, aes(fill = year)) + 
              scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "YlOrRd"), name = "Wildfire Year") +
              new_scale_fill() + 
              geom_sf(data = cb, color = NA, aes(fill = year)) + 
              scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "BuGn"), name = "Harvest Year")
          })
        
        
        my_stream_network <- bcdc_query_geodata("freshwater-atlas-stream-network") %>%
          filter(INTERSECTS(new_ws)) %>%
          collect()
        
        
        my_stream_network <- my_stream_network %>%
          filter(STREAM_ORDER %in% c(max(unique(my_stream_network$STREAM_ORDER))-1,
                                     max(unique(my_stream_network$STREAM_ORDER)))) %>% st_as_sf()
        # summary(my_stream_network)
        p <- st_cast(my_stream_network, to = "POINT") %>% 
          rename(newid = BLUE_LINE_KEY) %>% 
          select(newid, geometry)
        
        d <- p %>% 
          mutate(coords = st_coordinates(p)) %>% 
          mutate(X = coords[,"X"],
                 Y = coords[,"Y"],
                 Z = coords[,"Z"]) %>% 
          select(-coords) %>% 
          group_by(newid) %>% 
          arrange(Z) %>% 
          mutate(dist_seg_m = tidyr::replace_na(as.numeric(st_distance(geometry, lag(geometry), by_element = TRUE)),0),
                 dist_tot_m = cumsum(dist_seg_m))
      
      output$profile <- renderPlot({
        d %>% 
          ggplot() + 
          geom_point(aes(dist_tot_m/1000,Z,color=as.character(newid)))})
  
        # }
    })
  })
  output$about <- renderUI({
    HTML(paste("<br><br><br>Contact: <b>Alex Bevington</b> (Research Hydrologist): Alexandre.Bevington@gov.bc.ca<br>"))
  })
  }
   
shinyApp(secure_app(ui), server)
# shinyApp(ui, server)







# all_ws %>%
#   filter(st_contains(new_ws %>% st_buffer(1000),all_ws, sparse = F)[1,]) %>%
#   arrange(-area_m2) %>% 
#   mapview::mapview() + mapview::mapview(new_ws)
# 
# all_ws %>%
#   filter(st_contains(new_ws %>% st_buffer(1000),all_ws, sparse = F)[1,]) %>%
#   arrange(-area_m2) %>% 
#   ggplot() + 
#   geom_sf(aes(color = area_m2), fill = NA)
