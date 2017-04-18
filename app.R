source('LE_vs_FR.R')

if (!require("shiny")) {
  install.packages("shiny", repos = "http://cran.us.r-project.org")
}

if (!require("ggplot2")) {
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
}

if (!require("ggvis")) {
  install.packages("ggvis", repos = "http://cran.us.r-project.org")
}



regions <- c("Latin America & Caribbean", "South Asia",  "Sub-Saharan Africa", "Europe & Central Asia", "Middle East & North Africa",
             "East Asia & Pacific", "North America")

size_func <- function(data, max_size, min_size){
  data_max <- max(data)
  rescaled_data <- (data/data_max)*max_size
  return(sapply(rescaled_data, function(x) max(min_size, x)))
}

ui <- fluidPage(
  headerPanel("Fertility Rate vs Life Expectancy"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("regions", label = "Region", choices = regions, selected = regions, inline = FALSE)
    ),
    mainPanel(
      ggvisOutput("plot"),
      sliderInput("year_slider", label = NULL, min = 1960, max = 2014, value = 1960, ticks = FALSE, round = TRUE,
                  sep = "", step = 1, width = '100%',  animate=animationOptions(interval = 200)),
      uiOutput("plot_ui")
      ),
    position = "right"
  )
)

server <- function(input, output) {
  
  country_data_opaque <- reactive({
    country_data %>% 
      mutate(opacity = ifelse(Region %in% input$regions, 0.75, 0.1))
  })
  
  year_data <- reactive({
    country_data_opaque() %>% 
      filter(year == input$year_slider)
  })
  
  vis <- reactive({
      year_data %>%
      ggvis(x = ~le_interp, y = ~fr_interp) %>%
      layer_points(key:= ~`Country Name`, stroke:= "black",
                   opacity:= ~opacity, fill = ~Region,
                   size:= ~point_size, size.hover:= ~hover_point_size) %>%
      add_axis("x", title = "Life Expectancy") %>% 
      add_axis("y", title = "Fertility Rate") %>% 
      scale_numeric("y", domain = c(0.5, 9), nice = FALSE, clamp = TRUE) %>% 
      scale_numeric("x", domain = c(10, 90), nice = FALSE, clamp = TRUE) %>% 
      add_tooltip(function(data){
        paste0("<b>Country:</b> ", data$`Country Name`, "<br>", 
               "<b>Region:</b> ", data$Region, "<br>",
               "<b>Life Expectancy:</b> ", as.character(round(data$le_interp, 2)), "<br>",
               "<b>Fertility Rate:</b> ", as.character(round(data$fr_interp, 2)))
      }, "hover") %>%
      hide_legend("size") %>% 
      # hide_legend("fill") %>% 
      set_options(height = 600, width = 900)
    })
    vis %>% bind_shiny("plot", "plot_ui")
}

shinyApp(ui = ui, server = server)