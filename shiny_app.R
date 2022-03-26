library(bcdata)
library(leaflet)
library(dplyr)
library(stars)
library(elevatr)
library(shiny)
library(sf) 
library(bcmaps)
library(whitebox)
library(ggplot2)

dir.create("data", showWarnings = F)

# USER INTERFACE ####
ui <- fluidPage(
  titlePanel("Dynamic Watershed Summaries for British Columbia"),
  leafletOutput("mymap",height = '400px'),
  plotOutput("timeseriesPlot", height = 300),
  verbatimTextOutput("mymap_click")
  )

# SERVER ####
server <- function(input, output, session) {
  
  # INITIAL LEAFLET MAP ####
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -125, lat = 55, zoom = 6) %>% 
      addMeasure()})

  # ON CLICK... DO... ####
  observeEvent(input$mymap_click, {

    # PROGRESS BAR ####
    withProgress(message = 'Making plot', value = 0, max = 10, {
      
      # CLICK COORDS ####
      point <- input$mymap_click
      print(point)
      
      # COORDS TO POINT ####
      # point_sf <- st_as_sf(data.frame(lat=54.25676,lng=-121.0374), coords=c("lng","lat"), crs=4326) %>% st_transform(3005)
      point_sf <- st_as_sf(data.frame(lat=point$lat,lng=point$lng), coords=c("lng","lat"), crs=4326) %>% st_transform(3005)
      st_write(point_sf, "data/point_sf.shp", delete_layer = T)
      
      # GET (SMALLEST) NAMED WATERSHED FOR DEM DOWNLOAD AOI ####
      incProgress(1/10, detail = "Getting FWA watershed")
      ws <- bcdc_query_geodata("ea63ea04-eab0-4b83-8729-f8a93ac688a1") %>% filter(INTERSECTS(point_sf)) %>%  collect()
      ws <- arrange(ws, FEATURE_AREA_SQM)[1,]
    
      if(ws$FEATURE_AREA_SQM > 5000000000){ 
        print("WATERSHED TOO LARGE")} else{
            
        # DOWNLOAD DEM ####
        incProgress(2/10, detail = "Getting DEM")
        dem <- st_as_stars(elevatr::get_elev_raster(locations = as_Spatial(ws), z = 11))
        dem <- dem[st_bbox(ws)]
        file.remove("data/terrain_dem.tif")
        write_stars(dem, "data/terrain_dem.tif")
      
        # DOWNLOAD DEM ####
        incProgress(3/10, detail = "Breach depressions...")
        wbt_breach_depressions(
        dem = "data/terrain_dem.tif", 
        output = "data/terrain_dem_breach.tif")
      
        # REPROJECT TO CRS ####
        incProgress(4/10, detail = "Reproject...")
        file.remove("data/terrain_dem_proj.tif")
        gdal_utils(util = "warp",
                   source = "data/terrain_dem_breach.tif",
                   destination = "data/terrain_dem_proj.tif",
                   options = c(
                     "-t_srs","EPSG:3005",
                     "-co", "COMPRESS=DEFLATE",
                     "-r", "cubic",
                     "-dstnodata","NA"))
        
        # FLOW POINTER ####
        incProgress(5/10, detail = "Flow pointer...")
        file.remove("data/flow_d8_pntr.tif")
        wbt_d8_pointer(
          dem = "data/terrain_dem_proj.tif",
          output = "data/flow_d8_pntr.tif")
      
        # FLOW ACCUM ####
        incProgress(5/10, detail = "Flow accum...")
        file.remove("data/flow_d8_accum.tif")
        wbt_d8_flow_accumulation(
          input = "data/terrain_dem_proj.tif",
          output = "data/flow_d8_accum.tif")
      
        # SNAP POINTS ####
        incProgress(6/10, detail = "Snap...")
        file.remove("data/s_out_pt_snap.shp")
        wbt_snap_pour_points(
          pour_pts = "data/point_sf.shp",
          flow_accum = "data/flow_d8_accum.tif",
          output = "data/point_sf_snap.shp",
          snap_dist = 200)
      
        # WATERSHED RASTER ####
        incProgress(7/10, detail = "WS raster...")
        file.remove("data/s_out_ws.tif")
        wbt_watershed(
          d8_pntr = "data/flow_d8_pntr.tif",
          pour_pts = "data/point_sf_snap.shp",
          output = "data/s_out_ws.tif")
      
        # WATERSHED VECTOR ####
        incProgress(7/10, detail = "WS vector...")
        file.remove("data/s_out_ws.shp")
        wbt_raster_to_vector_polygons(
          input = "data/s_out_ws.tif",
          output = "data/s_out_ws.shp")
  
        # READ WS ####
        new_ws <- read_sf("data/s_out_ws.shp")
  
        # CUTBLOCKS ####
        incProgress(8/10, detail = "Download cutblocks...")
        cb <- bcdata::bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
          filter(INTERSECTS(new_ws)) %>%
          collect()
        if(nrow(cb)!=0){cb <- st_intersection(cb,new_ws)}
    
        # OUTPUT UPDATE LEAFLET ####
        
        r <- read_stars("data/flow_d8_accum.tif")>10
        r[r==0] <- NA
  
        output$mymap <- renderLeaflet({
          leaflet() %>%
            addProviderTiles(providers$Esri.WorldImagery) %>%
            leafem::addStarsImage(r %>% st_warp(crs=4326), group = "flow") %>%
            addPolygons(data= ws %>% st_transform(4326), fillOpacity = 0, color = "blue", group = "full ws") %>% 
            addPolygons(data=cb %>% st_transform(4326), fillOpacity = 0, color = "green", group = "cutblocks") %>% 
            addPolygons(data=new_ws %>% st_transform(4326), fillOpacity = 0, color = "red", group = "watershed") %>%
            addMeasure() %>% 
            addLayersControl(overlayGroups = c("flow","full_ws","watershed","cutblock"),
                             options = layersControlOptions(collapsed = FALSE)) %>% 
            hideGroup("flow") %>%
            addLegend(position = "bottomright", colors = c("blue","red","green"), labels = c("full_ws","watershed", "cutblock"), opacity = 1, title = "Polygons")
          })
        
        # OUTPUT GGPLOT HARVEST SUMMARIES ####
        output$timeseriesPlot <- renderPlot({
          cb %>% 
            mutate(area_km2=as.numeric(st_area(.))/(1000*1000)) %>% 
            st_drop_geometry() %>% 
            group_by(HARVEST_YEAR) %>% 
            summarize(area_km2=sum(area_km2,na.rm=T)) %>% 
            ggplot() + 
            geom_col(aes(HARVEST_YEAR,area_km2)) + 
            egg::theme_article() + 
            labs(x = "Year", y = "Harvest Area (km2)")
        })
        }
    })
  })
  
  }
   


shinyApp(ui, server)

