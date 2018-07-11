fluidPage(
  # max width of page is 12 units
  column(width = 2,
         selectInput(inputId = "lu_select_attr",
                     label = "Indicator",
                     choices = c("Population" = "population",
                                 "Households" = "households",
                                 "Employment" = "employment",
                                 "Residential Units" = "residential_units"),
                     selected = "Population")
         ),
  column(width = 10,
         plotlyOutput("regplot") 
         )
         
)