navbarPage("Vision 2050",
#fluidPage(
  #titlePanel("Vision 2050"),
  tabPanel("Topsheet",
    tabsetPanel(
      tabPanel(
      "Transportation indicators"
      ), 
      tabPanel("Land use indicators"),
      tabPanel("Air quality"),
      tabPanel("Ecosystems")
    )
  ),
  tabPanel("Maps",
    column(width = 3,
      radioButtons("map_scenario", "Alternative", c("No Action", "TOD", "Just Friends")),
      selectInput("map_indicator", "Indicator",
                choices = c("Population density",
                            "Employment density",
                            "VMT", 
                            "VHT",
                            "Delay per person")
      ),
      selectInput("map_year", "Year", c(2017, 2050)),
      selectInput("map_geo", "Geography", c("City", "FAZ", "TAZ"))
    )
  ),
  tabPanel("Time Series",
           # max width of page is 12 units
           column(width = 2,
                  selectInput(inputId = "lu_select_attr",
                              label = "Indicator",
                              choices = c("Population" = "population",
                                          "Households" = "households",
                                          "Employment" = "employment",
                                          "Residential Units" = "residential_units"),
                              selected = "Population"),
                  selectInput("ts_geo", "Geography", c("Region", "County", "RGS", "City"))  
           ),
           column(width = 10,
                  plotlyOutput("regplot") 
           )
  ),
  tabPanel("Histograms"),
  tabPanel("Tables"),
  tabPanel("Origin-Destination",
           selectInput("OD_indicator", "Indicator",
                       choices = c("AM Travel Times",
                                   "PM Travel Times"))
           )
)
