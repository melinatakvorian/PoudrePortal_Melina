#must have this on each setup
library(shiny)

#set up for the shiny app
library(bslib)
library(dplyr)
library(leaflet)
library(plotly)
library(readr)
library(sf)
library(shinyWidgets)
library(stringr)

chem_data <- readRDS("data/tidyResChem.RDS")
sites <- readRDS("data/sites_table.RDS")

chem_vals <-  c("Turbidity", "TSS", "ChlA", "DOC", "DTN", "pH",
  "ANC", "SC", "Na", "NH4", "K", "Mg", "Ca", "F",
  "Cl", "NO3", "PO4", "SO4")

# ui -------------------------------
ui <- fluidPage(fluidRow(
  column(
    5,
    # Application title
    titlePanel("Cameron Peak Burn Area - Upper Poudre Lake Chemistry"),
    
    # Add some informational text using and HTML tag (i.e., a level 5 heading)
    h5(
      "In this app you can filter occurrences by burn status and date range,
    You can also click on individual occurrences to view metadata."
    ),
    #### Layout ####
    tabsetPanel(
      tabPanel(
        "Map",
        
        #Output: interactive map
        leaflet::leafletOutput("map", width = '100%' , height = 500)
      ),
      
      #Output: interactive table NEED TO DEVELOP
      tabPanel(
        "Table",
        
        #Output: interactive plot
        div(DT::dataTableOutput("table"), style = "font-size:80%"))
    ),
    br(),
    em("Click on a site to view time series plots to the right"),
    hr(),
    actionButton("clear", "Clear Plots"),
    br(),
    br(),
    
    # Input: select Campaign shown on map
    pickerInput(
      "campaignChoice", #sourceChoice
      "Filter by Campaign:",
      choices = c(
        "Mainstem",
        "South Fork",
        "Chambers Complex",
        "Longdraw",
        "Tributary"
      ),
      selected = c(
        "Mainstem",
        "South Fork",
        "Chambers Complex",
        "Longdraw",
        "Tributary"
      ),
      multiple = TRUE
    ),
    
    # Input: select burn status shown on map
    checkboxGroupButtons(
      inputId = "burnStatus",
      #varChoice
      label = "Filter by Category:",
      choices = c("Partially Burned",
                  "Burned",
                  "Unburned"),
      
      selected = c("Partially Burned",
                   "Burned",
                   "Unburned"),
      
      direction = "horizontal",
      individual = TRUE,
      status = "primary",
      checkIcon = list(yes = icon("square-check"),
                       no = icon("square"))
    )
  ),
            
            #plots setup--------
            column(
              7,
              fluidRow(
                ## Input: Filter by date range --------
                sliderInput(
                  inputId = "daterange",
                  label = "",
                  value = c(as.Date("2014-09-17"),
                            as.Date("2022-11-30")),
                  min = as.Date("2014-09-14"),
                  max = as.Date("2022-11-30"),
                  width = '100%'
                ),
                
                ## Input: Select variable to plot ----------
                selectInput(
                  inputId = "select1Var",
                  #precipVar
                  label = "Selection 1",
                  choices = all_of(chem_vals)
                ),
                ## Output: plot variable selected ------------
                plotlyOutput("select1", width = "100%", height = 160),
                
                
                ## Input: select 2nd variable to plot --------
                selectInput(
                  inputId = "select2Var",
                  #tempVar
                  label = "Selection 2",
                  choices = all_of(chem_vals)
                ),
                ## Output: plot 2nd variable selected---------
                plotlyOutput("select2", width = "100%", height = 190),
                
                ##Input: select 3rd variable----------
                selectInput(
                  inputId = "select3Var",
                  #streamVar
                  label = "Selection 3",
                  choices = all_of(chem_vals)
                ),
                ##Output: plot 3rd variable --------
                plotlyOutput("select3", width = "100%", height = 190),
                
                
                ##Input: select 4th variable to plot ---------
                selectInput(
                  inputId = "select4Var",
                  #qual
                  label = "Selection 4",
                  choices = all_of(chem_vals)
                ),
                ##Output: plot 4th variable-----
                plotlyOutput("select4", width = "100%", height = 190),
                
                
                strong("Note: some data may be missing for certain dates/variables"),
                br(),
                br()
            ))
))


# server ----------
server <- function(input, output, session){
  
# question for Caitlin (I think this is the problem) -----
  
  #commented out and trying the simple version
  #not sure how to mimic this for me bc these vals cross over
  # chem_data_filtered <- reactive({
  # 
  #   if(is.null(input$burnStatus))
  #     return(chem_data %>% filter(is.na(data_available)))
  # 
  #   chem_data %>% filter(Campaign %in% input$campaignChoice) %>%
  #   filter(str_detect(data_available, paste(input$burnStatus, collapse = "|")))
  # })
  
  chem_data_filtered <- reactive({
    
    
    chem_data %>% filter(status %in% input$burnStatus)
    
  })
  
  
  pal <- colorFactor(palette = c("#04e9e7","#d40000","#fdf802"), 
                    chem_data$status)
  
  pal_campaign <- colorFactor(palette = c("#6eaab5", "#6e6eb5", "#996eb5", "#b16eb5"), 
                              chem_data$Campaign)
#### Building the Map ####
  #Render the map based on our reactive occurrence dataset
  
  output$map <- leaflet::renderLeaflet({
    leaflet() %>%
      setView(lng = -105.5440, lat = 40.62055, zoom = 09) %>%
      addTiles(group = "Open Street Map") %>%
      addProviderTiles("Esri.WorldImagery", layerId = "C", group = "Satellite") %>%
      addWMSTiles(
        sprintf(
          "https://%s/arcgis/services/%s/MapServer/WmsServer",
          "basemap.nationalmap.gov",
          "USGSTopo"
        ),
        group = "USGS Topo",
        attribution = paste0(
          "<a href='https://www.usgs.gov/'>",
          "U.S. Geological Survey</a> | ",
          "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
          "Policies</a>"
        ),
        layers = "0"
      ) %>%
      addMapPane("burn", zIndex = 410) %>%

      # don't have layer files to make Cameron Peak map
# question for Caitlin: how to zoom to extent? ----------

      addLegend("topright", data = chem_data, values = ~Campaign,
                pal = pal_campaign, title = "Campaign") %>%

      addLegend("topright", data = chem_data, values = ~status,
                pal = pal, title = "Burn Status") %>%

      addScaleBar(position = "bottomright") %>%

      addLayersControl(
        baseGroups = c("USGS Topo", "Open Street Map", "Satellite"),
        # do i need? ->
        # overlayGroups = "Burn Status", "Campaign",
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)
      )
  })
    
    #### Table Tab ####
    output$table <- DT::renderDataTable(sites, rownames = FALSE,
                                        options = list(autoWidth = TRUE, scrollX = TRUE,
                                                       scrollY = "200px", scrollCollapse = TRUE,
                                                       paging = FALSE, float = "left"),
                                        width = "80%", height = "70%")
    
    
    tableProxy <- DT::dataTableProxy("table")
    
    observe({

      input$nav

# formatting map ------
      #### Caitlin - don't think I need this ####
      tab <- leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(
          data = chem_data_filtered(), #instead of chem_data_filtered()
          layerId = ~ Site,
          lng = ~ Long,
          lat = ~ Lat,
          radius = 8,
          color = "black",
          fillColor = ~ pal(status),
          stroke = TRUE,
          weight = 1,
          fillOpacity = 1,
          popup = paste(
            "Burn Status:",
            chem_data_filtered()$status, #testing
            "<br>",
            "Site:",
            chem_data_filtered()$Site #testing
          ),

          options = pathOptions(pane = "burn")
        )

  })
  

  #this is throwing an error, trying without the mutate
  #df <- reactiveVal(chem_data %>% mutate(key = 1:nrow(.)))
  df <- reactiveVal(chem_data)
  combined <- reactiveVal(data.frame())
  
  
  filtered_df <- reactive({
    res <- df() %>% filter(dates >= input$daterange[1] & dates <= input$daterange[2])
    
  })
  
  
  observeEvent(input$map_marker_click, {
    
    combined(
      bind_rows(combined(),
                df() %>%
                  filter(Site %in% input$map_marker_click)) 
      
    )
  })
  
  observeEvent(input$table_rows_selected, {
    
    tableSelected <- sites[input$table_rows_selected,]
    
    combined(bind_rows(combined(),
                       filtered_df() %>% 
                         filter(Site %in% tableSelected$Site)))
  })

  final_df <- reactive({
    combined() %>% rename(
      select3Choice = input$select3Var, #streamflow = streamVar
      select4Choice = input$select4Var, #quality = qual
      select1Choice = input$select1Var, #precip = precipVar
      select2Choice = input$select2Var #temp = tempVar
    ) %>%
      filter(dates >= input$daterange[1] &
               dates <= input$daterange[2]) %>%
      
      arrange(dates)
    })
  
  # Plots -------
  # Question for Caitlin - which structure to use? ---------
  ##### Select 1 #####
  output$select1 <- renderPlotly({
    
    
    if(nrow(combined()) == 0)
      return(NULL)
    
    if(input$select1Var == "Turbidity"){
      
      plotly::plot_ly() %>%
        add_bars(x = final_df()$dates, y = final_df()$select1Choice, name = ~ final_df()$Site,
                 color = ~ final_df()$Site) %>%
        plotly::layout(yaxis = list(title = "Turbidity (NTU)", autorange = "reversed"),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = TRUE),
                       showlegend = TRUE,
                       legend = list(orientation = "h", x = 0.01, y = 1.4))
    }else {
      
      plot_ly(final_df()) %>%
        add_trace(x = final_df()$dates,
                  y = final_df()$select1Choice,
                  name = ~ final_df()$Site,
                  linetype = ~ final_df()$Site,
                  mode = "lines+markers") %>%
        plotly::layout(yaxis = list(title = paste(input$select1Var, "(mg/L)"), range = list(0, max(final_df()$select1Choice))),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = T),
                       showlegend = TRUE,
                       legend = list(orientation = "h", x = 0.01, y = 1.2))
      
      
    }
    }
    )
  
  ##### Select 2 ####
  output$select2 <- renderPlotly({
    
    if(nrow(combined()) == 0)
      return(NULL)
    
    plot_ly(final_df()) %>%
      add_trace(x = final_df()$Date,
                y = final_df()$select2Choice,
                name = ~ final_df()$Site,
                linetype = ~ final_df()$Site,
                mode = "lines+markers") %>%
      plotly::layout(yaxis = list(title = paste(input$select2Var), range = list(0, max(final_df()$select2Choice))),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = T),
                     showlegend = TRUE,
                     legend = list(orientation = "h", x = 0.01, y = 1.2))
  })
  
  ##### Select 3 ####
  output$select3 <- renderPlotly({
    
    
    if(nrow(combined()) == 0)
      return(NULL)
    
    
    plot_ly(final_df()) %>%
      add_trace(x = final_df()$Date,
                y = final_df()$select3Choice,
                name = ~final_df()$Site,
                linetype = ~ final_df()$Site,
                mode = "lines+markers") %>%
      plotly::layout(yaxis = list(title = input$select3Var),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = T),
                     showlegend = TRUE,
                     legend = list(orientation = "h", x = 0.01, y = 1.2))
  })
  
  ##### Select 4 ####
  output$select4 <- renderPlotly({
    
    if(nrow(combined()) == 0)
      return(NULL)
    
    if(!(input$select4Var %in% names(combined())))
      return(NULL)
    
    plotly::plot_ly(final_df()) %>%
      add_trace(x = final_df()$Date,
                y = final_df()$select4Choice,
                name = ~final_df()$Site,
                mode = 'lines+markers',
                linetype = ~ final_df()$Site,
                connectgaps = TRUE) %>%
      plotly::layout(yaxis = list(title = input$select4Var),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = T),
                     showlegend = TRUE,
                     legend = list(orientation = "h", 
                                   x = 0.01, y = 1.2))
  })
  
  observeEvent(input$clear, {
    combined(data.frame())
    
    tableProxy %>% DT::selectRows(NULL)
    
  })
  
  }

# Run the application --------
shinyApp(ui = ui, server = server)
