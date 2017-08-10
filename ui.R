library(shinydashboard)
library(leaflet)
library(RColorBrewer)

header <- dashboardHeader(
  title = "PREPROS - PRedicting Epidemic PROpagation Service"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
      box(width = NULL, solidHeader = TRUE,
        leafletOutput("bahiamap", height = 500)
      )
      # ,
      # box(width = NULL, status = "warning",
      #   tableOutput("targetsTable")
      # )
    ),
    column(width = 9,
      box(width = NULL, status = "warning",
        uiOutput("citySelect")
      ),
      box(width = NULL, status = "warning",
        uiOutput("wEdge")
      ),
      box(width = NULL, status = "warning",
          uiOutput("dEdge")
      )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
