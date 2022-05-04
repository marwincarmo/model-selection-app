sidebar <- dashboardSidebar(
  collapsed = FALSE,
  # https://fontawesome.com/icons?d=gallery&m=free
  sidebarMenu(
    id = "tabs",
    menuItem("Simulation", tabName = "sim_tab",
             icon = icon("robot"),
             startExpanded = TRUE),
    menuItem("Info", tabName = "info_tab",
             icon = icon("info")),
    menuItem("Code", href = "https://github.com/marwincarmo/model-selection-app", icon = icon("github"))
  )
)