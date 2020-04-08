#for the app:
library(shiny)
library(shinydashboard)
#graphs:
library(plotly)
#map:
library(leaflet)
library(leaflet.extras)

tsunami <- read.csv("Tsunami.csv") #the data
source("map.R") #the map
t <- list( #making the default font for the plots
    family = "Georgia", size = 14, color = '#08427B')

#how the page looks
ui <- fluidPage(
    includeCSS("www/custom.css"),
    titlePanel("Tsunami Data"),
    sidebarPanel(
        p("By: Su-Ah Lee"),
        br(),
        p("April 8th, 2020"),
        br(),
        p("A Shiny app made for GMU's ENGH 485 Spring 2020.", width = "100%"),
        br(),
        p("The data used is from the Homeland Infrastructure Foundation-Level Data.", 
          width = "100%"),
        br(),
        includeText("info.txt")
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Tsunami Map", br(), leafletOutput("map")),
            tabPanel("Magnitude Frequency", br(), plotlyOutput("magFig"), br(),
                     verbatimTextOutput("magfreqT") ),
            tabPanel("Magnitude Frequency", br(), plotlyOutput("magfig20"), br(),
                     verbatimTextOutput("mft20")),
            tabPanel("Year", br(), plotlyOutput("yr"), br(), 
                     verbatimTextOutput("yrSum"))
        )
    )
)

#server-side
server <- function(input, output) {
    
    output$magFig <- renderPlotly({
        data <- read.csv("Tsunami.csv")
        totMagC = table(data$EQ_MAGNITUDE)
        totmc = as.data.frame(totMagC)
        magFig <- plot_ly( x = totmc$Var1, y = totmc$Freq, type = "bar",
                           marker = list(color = "#A2C4EF", 
                                         line = list(color = "#4E148C", width = 1.5)))
        magFig <- magFig %>% 
            layout(title = "Magnitude Frequency of Tsunamis", font = t,
                   xaxis = list(title = "Magnitude"),
                   yaxis = list(title = "Frequency"))
    })
    
    output$magfreqT <- renderPrint({
       summary(data$EQ_MAGNITUDE)
        })
    
    output$magfig20 <- renderPlotly({
        data <- read.csv("Tsunami.csv")
        tsun <- data[which(data$YEAR>= 2000),]
        magnitudeCount = table(tsun$EQ_MAGNITUDE)
        mc = as.data.frame(magnitudeCount)
        magfig20 <- plot_ly( x = mc$Var1, y = mc$Freq, type = "bar", text = c(mc$Freq),
                             marker = list(color = "#A2C4EF", 
                                           line = list(color = "#4E148C", width = 1.5)))
        magfig20 <- magfig20 %>% 
            layout(title = "Magnitude Frequency of Tsunamis in the Past 20 Years", 
                   font = t, xaxis = list(title = "Magnitude"), 
                   yaxis = list(title = "Frequency"))
    })
    
    output$mft20 <-renderPrint({
        tsun <- data[which(data$YEAR>= 2000),]
        summary(tsun$EQ_MAGNITUDE)
    })
    
    output$yr <- renderPlotly({
        tsunami$EQ_MAGNITUDE[is.na(tsunami$EQ_MAGNITUDE)] <- 1
        data <- tsunami[which(tsunami$EQ_MAGNITUDE > 0),]
        yrct = table(sort(data$YEAR))
        yr = as.data.frame(yrct)
        yrmap <- plot_ly( x = yr$Var1, y = yr$Freq, type = "bar", text = c(yr$Freq),
                          marker = list(color = "#A2C4EF", 
                                        line = list(color = "#4E148C", width = 1.5)))
        yrmap <- yrmap %>% 
            layout(title = "Tsunami Frequency by Year", font = t,
                   xaxis = list(title = "Year"), yaxis = list(title = "Frequency")) 
    })
    
    output$yrSum <- renderPrint({
        tsunami$EQ_MAGNITUDE[is.na(tsunami$EQ_MAGNITUDE)] <- 1
        data <- tsunami[which(tsunami$EQ_MAGNITUDE > 0),]
        yrct = table(sort(data$YEAR))
        yr = as.data.frame(yrct)
        summary(yr$Freq)
    })
    
    output$map <- renderLeaflet({ 
        map()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
