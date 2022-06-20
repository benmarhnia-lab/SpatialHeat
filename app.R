library(shiny)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Documents - Kristen’s MacBook Pro/EnviroResearchArmin")
#addResourcePath(prefix = "imgResources", directoryPath = "myimages")

#myPlotPaths <- c(paste0("myimages/", "BayesianAbsolute", ".png"),paste0("myimages/", "BayesRelative", ".png"))
# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Impacts of Heat using Various Extreme Heat Definitions in California"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    # Input: Selector for variable to plot against mpg ----
    selectInput("variable", "Extreme Heat Definition:", 
                c("max99_1",   "max99_2",   "max99_3",   "max975_1",  "max975_2",  "max975_3", 
                  "max95_1",   "max95_2",   "max95_3",   "min99_1",   "min99_2",   "min99_3",   "min975_1",  "min975_2", 
                  "min975_3",  "min95_1",   "min95_2",   "min95_3",   "diff01_1",  "diff01_2",  "diff01_3",  "diff025_1",
                  "diff025_2", "diff025_3", "diff05_1",  "diff05_2",  "diff05_3")),
    
    # Input: Checkbox for whether outliers should be included ----
    #selectInput("scale","Scale:",c("Absolute", "Relative"))
    ),
  # Main panel for displaying outputs ----
mainPanel(
  
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), imageOutput("bayes_abs"), imageOutput("bayes_rel"))
    ),
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_whichmax_abs"), plotOutput("plot_largeclust_abs"))
    ),
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_whichmax_rel"), plotOutput("plot_largeclust_rel"))
    ),
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_maxval_abs"), plotOutput("plot_maxval_rel"))
    ),
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("Plot_relative"), plotOutput("Plot_abs"))
    )
  )
)


#Data Processing 


joined <- read.csv("joined_rel.csv")
library(sf)
library(ggplot2)
library(scales)
joined$X = NULL
maxes <- apply(joined[,c(3:ncol(joined))], 1, function(x) max(x))
joined$whichMax_value <- apply(joined[,c(2:28)], 1, function(x) max(x))
joined$whichMax<- apply(joined[,c(2:28)], 1, function(x) which.max(x))
joined$largeClust = ifelse(joined$whichMax < 10, "max", ifelse(joined$whichMax < 19, "min", "diff"))
joined$whichMax = names(joined)[(joined$whichMax+1)]

path = "~/Downloads/ShapefileZCTA/cb_2016_us_zcta510_500k.shp"
#Path is the path to the shapefile for ZCTAs in california (shp)

joined$whichMax = as.factor(joined$whichMax)
joined$whichMax_test = factor(joined$whichMax, levels = c(unique(sort(joined$whichMax))[c(7:9,4:6,1:3,10:27) ]))
library(sf)
shape <- st_read(dsn = path)
m_rel <- merge(shape, joined, by = ("ZCTA5CE10"))


joined_abs <- read.csv("abs_joined.csv")
joined_abs$X = NULL
maxes <- apply(joined_abs[,c(3:ncol(joined_abs))], 1, function(x) max(x))
joined_abs$whichMax_value <- apply(joined_abs[,c(2:28)], 1, function(x) max(x))
joined_abs$whichMax<- apply(joined_abs[,c(2:28)], 1, function(x) which.max(x))
joined_abs$largeClust = ifelse(joined_abs$whichMax < 10, "max", ifelse(joined_abs$whichMax < 19, "min", "diff"))
joined_abs$whichMax = names(joined_abs)[(joined_abs$whichMax+1)]
joined_abs$whichMax = as.factor(joined_abs$whichMax)
joined_abs$whichMax_test = factor(joined_abs$whichMax, levels = c(unique(sort(joined_abs$whichMax))[c(7:9,4:6,1:3,10:27) ]))

m <- merge(shape, joined_abs, by = ("ZCTA5CE10"))
library(RColorBrewer) # Change the orange to greens 
maxes = colorRampPalette(c("white", "red4"))(12)[4:12]

mins = colorRampPalette(c("white", "purple4"))(12)[4:12]
diff = colorRampPalette(c("palegreen1", "springgreen4"))(9)[9:1]
#joined$whichMax = factor(joined$whichMax, levels = c(unique(sort(joined$whichMax))[c(9:1, 10:18, 19:27)]))
colors = c(diff, maxes, mins)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("Hospitalization attributable to heat for the following heat definition on a relative and absolute scale:", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  output$bayes_abs <- renderImage({
    list(
      src = file.path("www", paste0("BayesianAbsolute.png")),
      contentType = "image/png",
      width = 400,
      height = 400
    )
  }, deleteFile = FALSE)
  output$bayes_rel <- renderImage({
    list(
      src = file.path("www", paste0("BayesRelative.png")),
      contentType = "image/png",
      width = 400,
      height = 400
    )
  }, deleteFile = FALSE)
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
    output$plot_whichmax_abs <- renderPlot({
      ggplot(data = m) + geom_sf(aes(fill = whichMax), colour = NA) + scale_fill_manual(values = colors) + labs(fill = "Maximum definition") + ggtitle("Maximum effect heat event definition on absolute scale") #+ xlim(-117.6,-115.8) + ylim(32.5,33.5)
    })
    output$plot_largeclust_abs <- renderPlot({
      ggplot(data = m) + geom_sf(aes(fill = largeClust), colour = NA) + scale_fill_manual(values = c("springgreen4", "red4", "purple4")) + labs(fill = "Which Metric strongest?") + ggtitle("Metric of the maximum effect heat event definition on absolute scale")
    })
    output$plot_maxval_abs <- renderPlot({
      ggplot(data = m) + geom_sf(aes(fill = whichMax_value), colour = NA) + scale_fill_gradient(low = "white", high = muted("red")) + labs(fill = "Value at max") + ggtitle("Maximum value of the absolute hospitalization attributable to heat")
    })
    
    output$plot_whichmax_rel <- renderPlot({
      ggplot(data = m_rel) + geom_sf(aes(fill = whichMax), colour = NA) + scale_fill_manual(values = colors) + labs(fill = "Maximum definition")+ ggtitle("Maximum effect heat event definition on relative scale") #+ xlim(-117.6,-115.8) + ylim(32.5,33.5)
    })
    output$plot_largeclust_rel <- renderPlot({
      ggplot(data = m_rel) + geom_sf(aes(fill = largeClust), colour = NA) + scale_fill_manual(values = c("springgreen4", "red4", "purple4")) + labs(fill = "Which Metric strongest?") + ggtitle("Metric of the maximum effect heat event definition on relative scale")
    })
    output$plot_maxval_rel <- renderPlot({
      ggplot(data = m_rel) + geom_sf(aes(fill = whichMax_value), colour = NA) + scale_fill_gradient(low = "white", high = muted("red")) + labs(fill = "Value at max") + ggtitle("Maximum value of the relative hospitalization attributable to heat")
    })
    output$Plot_abs <- renderPlot({
      ggplot(data = m) + geom_sf(aes_string(fill = input$variable), colour = NA) + scale_fill_gradient2(low = "white", high = muted("red"), limits = c(-1,7)) + labs(fill = paste("Extreme Heat Def.", input$variable)) + ggtitle("Hospitalization attributable to heat")
    })
  
    output$Plot_relative <- renderPlot({
      ggplot(data = m_rel) + geom_sf(aes_string(fill = input$variable), colour = NA) + scale_fill_gradient2(low = muted("blue"), high = muted("red"), limits = c(-1,7)) + labs(fill = paste("Extreme Heat Def.", input$variable)) + ggtitle("Relative hospitalization increase from Heat")
    })
    #Just Los Angeles
    output$Plot_abs <- renderPlot({
      ggplot(data = m) + geom_sf(aes_string(fill = input$variable), colour = NA) + scale_fill_gradient2(low = "blue", high = "red", limits = c(-2,7)) + labs(fill = paste("Extreme Heat Def.", input$variable)) + ggtitle("Hospitalization attributable to heat in Los Angeles per EHE day") + xlim(-119,-117.6) + ylim(33.5,34.92)
    })
    
    output$Plot_relative <- renderPlot({
      ggplot(data = m_rel) + geom_sf(aes_string(fill = input$variable), colour = NA) + scale_fill_gradient2(low = "blue", high = "red", limits = c(-2,7)) + labs(fill = paste("Extreme Heat Def.", input$variable)) + ggtitle("Relative hospitalization increase from Heat in Los Angeles per EHE day")+ xlim(-119,-117.6) + ylim(33.5,34.92)
    })
    #Select county have map, move mouse on specific zip code see specific estimate
    #Each county overall ranking on absolute scale and relative scale which defintion has highest number of 
    #hospitalizations
    
    #On click zip code 
    
    #How many hospitalization attributable to heat 
    
    
}
  
  

shinyApp(ui, server)
