shinyUI(dashboardPage(
  skin = "black",
  dashboardHeader(title = "Tanzania Water Pumps"),
  dashboardSidebar(
    sidebarUserPanel("Stephen Shafer",
                     image = "https://media.licdn.com/dms/image/C5603AQEKSve45-fUnw/profile-displayphoto-shrink_200_200/0?e=1530316800&v=beta&t=bzwu85cd5Z5wJc9cB_JL1bav-oZAvVWzd12uNifxyU4"),
    sidebarMenu(
      menuItem("Water Pump Funcionality", tabName = "bar", icon = icon("bar-chart-o")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Data", tabName = "data", icon = icon("database"))
    ),
    selectizeInput("selected",
                   "Select Item to Display",
                   choice)

  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "bar",
              fluidRow(box(plotlyOutput("pump_plot"), width = 12))
              ),
      tabItem(tabName = "map",
              fluidRow(box(leafletOutput("map"), width = 8))
              ),
      tabItem(tabName = "data",
              fluidRow(box(DT::dataTableOutput("table"), width = 12)))
    )
  )
))

