sidebar <- dashboardSidebar(
  collapsed = TRUE,
  # https://fontawesome.com/icons?d=gallery&m=free
  sidebarMenu(
    id = "tabs",
    # menuItem("Main", tabName = "main_tab",
    #          icon = icon("home")),
    menuItem("Simulation", tabName = "sim_tab",
             icon = icon("home"),
             startExpanded = TRUE),
    menuItem("Info", tabName = "info_tab",
             icon = icon("info")),
    menuItem("App Code on Github", href = "https://github.com/marwincarmo/model-selection-app", icon = icon("github"))
  )
)
