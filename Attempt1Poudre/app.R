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

load("tidyResChem.RData")

tmap_mode("view")

# ui -------------------------------
ui <- fluidPage(
  # Application title
  titlePanel("Cameron Peak Burn Area - Upper Poudre Lake Chemistry"),
  
  # Add some informational text using and HTML tag (i.e., a level 5 heading)
  h5("In this app you can filter occurrences by burn status and date range,
    You can also click on individual occurrences to view metadata."),
  
  fluidRow(column (5, 
  #### Caitlin Layout ####
  tabsetPanel(
    tabPanel("Map",
      
      #Output: interactive map
      leaflet::leafletOutput("map", width = '100%' , height = 500)
    ),
    
      #Output: interactive table NEED TO DEVELOP
      tabPanel("Table", div(DT::dataTableOutput("table"), style = "font-size:80%"))
  ),
  
  #Formatting
  br(),
  em("Click on a site to view time series plots to the right"),
  hr(),
  actionButton("clear", "Clear Plots"),
  br(),
  br(),
  
  # Input: select burn status shown on map
  checkboxGroupButtons(
    inputId = "burnStatus",     #burnStatus = varChoice
    label = "Filter by Category:",
    choices = c(
      "Partially Burned",
      "Burned",
      "Unburned"),
    
    selected = c(
      "Partially Burned",
      "Burned",
      "Unburned"),
    
    direction = "horizontal",
    individual = TRUE,
    status = "primary",
    checkIcon = list(yes = icon("square-check"),
                     no = icon("square"))
  )
  ),
),

  
  
  
  
  #### My Original Layout ####
  # Sidebar layout
  # sidebarLayout(
  #   sidebarPanel(width = 6,
  #     
  #     #Output: our interactive map
  #     tmapOutput("map"),
  #     
  #     # Input: select burn status shown on map
  #     checkboxGroupInput(
  #       inputId = "burnstatus",
  #       label = "Burn Severity",
  #       # these names should match that in the dataset, if they didn't you would use 'choiceNames' and 'choiceValues' like we do for the next widget
  #       choices = list("Partially Burned", "Burned", "Unburned"),
  #       # selected = sets which are selected by default
  #       selected = c("Partially Burned", "Burned", "Unburned")
  #     ),
  #   ),
    
    
####moving onto plots setup!!
column(
  7,
  fluidRow(
    # Input: Filter by date range
    sliderInput(inputId = "daterange",
                value = c(as.Date("2014-09-17"),
                          as.Date("2022-11-30")),
                min = as.Date("2014-09-14"),
                max = as.Date("2022-11-30"),
                width = "100%"),
    
    
    selectInput(
      inputId = "select1Var", #precipVar
      label = "Select the attribute to plot",
      choices = c("Turbidity",
                  "TSS",
                  "ChlA", 
                  "DOC",
                  "DTN", 
                  "pH",
                  "ANC", 
                  "SC",
                  "Na",
                  "NH4",
                  "K",
                  "Mg",
                  "Ca",
                  "F",
                  "Cl",
                  "NO3",
                  "PO4",
                  "SO4")
    ),
    plotlyOutput("select1", width = "100%", height = 160),
    
    
    selectInput(
      inputId = "select2Var", #tempVar
      label = "Select the attribute to plot",
      choices = c("Turbidity",
                  "TSS",
                  "ChlA", 
                  "DOC",
                  "DTN", 
                  "pH",
                  "ANC", 
                  "SC",
                  "Na",
                  "NH4",
                  "K",
                  "Mg",
                  "Ca",
                  "F",
                  "Cl",
                  "NO3",
                  "PO4",
                  "SO4")
    ),
    plotlyOutput("select2", width = "100%", height = 190),
    
    
    selectInput(
      inputId = "select3Var", #streamVar
      label = "Select the attribute to plot",
      choices = c("Turbidity",
                  "TSS",
                  "ChlA", 
                  "DOC",
                  "DTN", 
                  "pH",
                  "ANC", 
                  "SC",
                  "Na",
                  "NH4",
                  "K",
                  "Mg",
                  "Ca",
                  "F",
                  "Cl",
                  "NO3",
                  "PO4",
                  "SO4")
    ),
    plotlyOutput("select3", width = "100%", height = 190),
    
    
    selectInput(
      inputId = "select4Var", #qual
      label = "Select the attribute to plot",
      choices = c("Turbidity",
                  "TSS",
                  "ChlA", 
                  "DOC",
                  "DTN", 
                  "pH",
                  "ANC", 
                  "SC",
                  "Na",
                  "NH4",
                  "K",
                  "Mg",
                  "Ca",
                  "F",
                  "Cl",
                  "NO3",
                  "PO4",
                  "SO4"),
      selected = "Turbidity"
      
    ),
    plotlyOutput("select4", width = "100%", height = 190),
    
    strong("Note: some data may be missing for certain dates/variables"),
    br(),
    br()
    
  ),
), )


#### MY Main panel for displaying output (our map) ####
  #   mainPanel(# have date range slider & plots
  #     width = 6,
  #     
  #     # Input: Filter by date range
  #     sliderInput(inputId = "daterange",
  #                 label  = "Select a date range",
  #                 value = c(as.Date("2014-09-17"),
  #                           as.Date("2022-11-30")),
  #                 min = as.Date("2014-09-14"),
  #                 max = as.Date("2022-11-30"),
  #                 width = "100%"),
  # 
  #     #Input: Select a variable to plot
  #     selectInput("compounds", "Choose a compound:",
  #                 list(`NO3` = "NO3",
  #                      `SO4` = "SO4",
  #                      `PO4` = "PO4",
  #                      `NH4` = "NH4")),
  #     
  #     #Output: Plots for selected variables
  #     # plotOutput("compounds"),
  #     # plotOutput(),
  #     
  # )

server <- function(input, output, session){
  
  
  pal <- colorFactor(palette = "Spectral", tidyResChem$status)
  
  # Make a reactive object for the chem data by calling inputIDs to extract the values the user chose
  chem_react <- reactive(
    tidyResChem %>%
      filter(status %in% input$burnstatus) %>%
      filter(newdate >= input$daterange[1] &
               newdate <= input$daterange[2])
      #filter(compounds %in% input$compounds) #how will I do this if the compounds are in diff columns
  )
  
  
  # Render the map based on our reactive occurrence dataset
  # output$map <- renderTmap({
  #   tm_shape(chem_react()) +
  #     tm_dots(
  #       col = "status",
  #       size = 0.1,
  #       palette = "BrBG",
  #       # want to find better color pallette
  #       title = "Sites",
  #       popup.vars = c(
  #         "Burn Status" = "status",
  #         "Date" = "newdate"
  #       )
  #     )
  # })
  
#### Caitlin Building the Map ####
  output$map <- leaflet::renderLeaflet({
    #what does leaflet() do?
    leaflet() %>%
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
      addMapPane("fire", zIndex = 410) %>%
      addPolygons(
        data = camPeak_simple,
        color = NA,
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.9,
        fillColor = ~ colorFactor("Reds", Severity)(Severity),
        group = "Cameron Peak Fire",
        options = pathOptions(pane = "fire")
      ) %>%
      
      addLegend("topright", data = tidyResChem, values = ~status, 
                pal = pal, title = "Burn Status") %>% 
      
      addScaleBar(position = "bottomright") %>%
      
      addLayersControl(
        baseGroups = c("USGS Topo", "Open Street Map", "Satellite"),
        overlayGroups = "Cameron Peak Fire",
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(c("Cameron Peak Fire"))
    
    output$table <- DT::renderDataTable(Site, rownames = FALSE,
                                        options = list(autoWidth = TRUE, scrollX = TRUE,
                                                       scrollY = "200px", scrollCollapse = TRUE,
                                                       paging = FALSE, float = "left"),
                                        width = "80%", height = "70%")
    
    
    tableProxy <- DT::dataTableProxy("table")
    
    
  })
  
  #MAKE PLOTS
  df <- reactiveVal(tidyResChem %>% mutate(key = 1:nrow(.)))
  combined <- reactiveVal(data.frame())
  
  
  filtered_df <- reactive({
    res <- df() %>% filter(newdate >= input$daterange[1] & newdate <= input$daterange[2])
    
  })
  
  observeEvent(input$map1_marker_click, {
    
    combined(
      bind_rows(combined(),
                df() %>%
                  filter(Site %in% input$map1_marker_click)) 
      
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
      filter(newdate >= input$daterange[1] &
               newdate <= input$daterange[2]) %>%
      
      arrange(newdate)
    
  })
  
#### Caitlin Make Plots ####
  ##### Select 1 #####
  output$select1 <- renderPlotly({
    
    
    if(nrow(combined()) == 0)
      return(NULL)
    
    if(input$select1Var == "Turbidity"){
      
      plotly::plot_ly() %>%
        add_bars(x = final_df()$newdate, y = final_df()$select1Choice, name = ~ final_df()$Site,
                 color = ~ final_df()$Site) %>%
        plotly::layout(yaxis = list(title = "Turbidity (NTU)", autorange = "reversed"),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = TRUE),
                       showlegend = TRUE,
                       legend = list(orientation = "h", x = 0.01, y = 1.4))
    }else {
      
      plot_ly(final_df()) %>%
        add_trace(x = final_df()$newdate,
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

# Run the application 
shinyApp(ui = ui, server = server)