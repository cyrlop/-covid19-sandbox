library(shiny)
library("ggplot2")

# Load CSSE deaths data
deaths.data <- read.csv(file="../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")


# Filter data by country
get_death_by_country <- function(data, country){
  deaths_country = subset(data, Country.Region == country, select=c(5:ncol(data)))
  deaths_country <- as.data.frame(t(deaths_country))

  colnames(deaths_country) <- c("Deaths")

  # Weird date format with X appended after the transposition
  deaths_country$Date <- sub("X", "", rownames(deaths_country))
  deaths_country$Date <- as.Date(deaths_country$Date, "%m.%d.%y")

  deaths_country$Deaths <- rowSums(deaths_country[1:ncol(deaths_country)-1])
  deaths_country <- deaths_country[,c("Deaths", "Date")]

  return(deaths_country)
}



# Define UI for application that plots random distributions 
ui <- shinyUI(bootstrapPage(
  
  p("The data comes from the CSSE 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE."),

  h3("Number of deaths by country (time series)"),
  
  div(class = "row",

    div(class = "col-sm-3",

      # Country select input
      selectInput("country",
                    "Select a country:",
                    choices = deaths.data$Country.Region,
                    #selected = c("France","France"),
                    multiple = FALSE,
                    selectize = TRUE,
                    width = NULL,
                    size = NULL),
    ),

    div(class = "col-sm-9",
      # Show a plot of the generated distribution
      plotOutput("plot_deaths", height=300)
    )
  )

))



# Define server logic required to draw plots
server <- shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot

  output$plot_deaths <- renderPlot({

    # get death data for selected country
    deaths_country = get_death_by_country(deaths.data, input$country)

    # plot
    ggplot(data=deaths_country, aes(y=Deaths, x=Date)) + geom_line()
  })

})



shinyApp(ui = ui, server = server)