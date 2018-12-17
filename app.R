#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(htmltools)
library(tidyverse)
library(benford.analysis)
library(rgdal)
library(arm)


#Data Import
fifa1data = read.csv(file = "complete.csv")
fifa1data <- dplyr::select(fifa1data, name, overall, club, age, league, height_cm, weight_kg, nationality, eur_wage)
colnames(fifa1data) <- c("name", "evaluation", "club", "age", "league", "height","weight", "nationality", "wage")
fifa1data = fifa1data[-which(fifa1data$wage == 0),]
fifa1data = fifa1data[-which(fifa1data$evaluation < 70),]

countrycount <- fifa1data %>%
  group_by(nationality) %>%
  count()
countrycount

fifa1data <- fifa1data %>%
  group_by(nationality) %>% 
  mutate(count = n()) %>%
  filter(count>10)

weightcountry <- fifa1data %>%
  group_by(nationality) %>%
  summarise(avg.weight = mean(weight))
weightcountry

heightcountry <- fifa1data %>%
  group_by(nationality) %>%
  summarise(avg.height = mean(height))
heightcountry

bodycountry <- cbind(weightcountry,heightcountry)
colnames(bodycountry) <- c("nationality", "avg.weight", "nationality2", "avg.height")
bodycountry <- dplyr::select(bodycountry, nationality, avg.weight, avg.height)

wagecountry <- fifa1data %>%
  group_by(nationality) %>%
  summarise(avg.wage = mean(wage))
wagecountry
##################################

#Map
## Download Map
url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"

tmp <- tempdir()
file <- basename(url)
download.file(url, file)
unzip(file, exdir = tmp)

## Interset and Merge
countries <- readOGR(dsn=tmp,
                     layer = "ne_50m_admin_0_countries", 
                     encoding = "UTF-8")
country_name<-intersect(countries$SUBUNIT,wagecountry$nationality)
fifa2018 <-sp::merge(countries, wagecountry %>% filter(nationality%in%country_name),
                     by.y="nationality",by.x="SUBUNIT",sort=FALSE,duplicateGeoms =
                       TRUE,all.x=FALSE)

#map1
pal <- colorNumeric("YlOrRd", domain = fifa2018$avg.wage)
labels<- sprintf(
  "<strong>%s</strong><br/>%s EURO",
  fifa2018$SUBUNIT, round(fifa2018$avg.wage,2)
) %>% lapply(htmltools::HTML)
map1 <- leaflet() %>%
  addTiles() %>%
  addPolygons(data=fifa2018,
              fillColor = ~pal(fifa2018$avg.wage),
              weight = 4,opacity = 0.4,color = "darksalmon",
              dashArray = "1",fillOpacity = 0.4,
              
              highlight = highlightOptions(
                weight = 5,color = "#7acde9",
                dashArray = "",
                fillOpacity = 0.7,bringToFront = TRUE),
              
              label = labels)%>%
  addLegend(pal = pal, values = fifa2018$avg.wage, opacity = 0.4, title = "Average Wage", labFormat = labelFormat(prefix = "EURO "), position = "bottomleft")

#map23 formulate
country_name2<-intersect(countries$SUBUNIT,bodycountry$nationality)
fifa2018_2 <-sp::merge(countries, bodycountry %>% filter(nationality%in%country_name2),
                       by.y="nationality",by.x="SUBUNIT",sort=FALSE,duplicateGeoms =
                         TRUE,all.x=FALSE)


#map2
pal2 <- colorNumeric("YlOrRd", domain = fifa2018_2$avg.weight)
labels2<- sprintf(
  "<strong>%s</strong><br/>%s Kilogram",
  fifa2018_2$SUBUNIT, round(fifa2018_2$avg.weight,2)
) %>% lapply(htmltools::HTML)
map2 <- leaflet() %>%
  addTiles() %>%
  addPolygons(data=fifa2018_2,
              fillColor = ~pal2(fifa2018_2$avg.weight), weight = 4, opacity = 0.4, color = "darksalmon",
              dashArray = "1", fillOpacity = 0.4,
              
              highlight = highlightOptions(
                weight = 5,color = "#7acde9",
                dashArray = "",fillOpacity = 0.7,
                bringToFront = TRUE),
              
              label = labels2)%>%
  addLegend(pal2, values = fifa2018_2$avg.weight, opacity = 0.4, title = "Average Weight", labFormat = labelFormat(prefix = "Kilograms "), position = "bottomleft")

#map2
pal3 <- colorNumeric("YlOrRd", domain = fifa2018_2$avg.height)
labels3<- sprintf(
  "<strong>%s</strong><br/>%s centimeter",
  fifa2018_2$SUBUNIT, round(fifa2018_2$avg.height,2)
) %>% lapply(htmltools::HTML)
map3 <- leaflet() %>%
  addTiles() %>%
  addPolygons(data=fifa2018_2,
            fillColor = ~pal3(fifa2018_2$avg.height),weight = 4,opacity = 0.4,color = "darksalmon",
              dashArray = "1",fillOpacity = 0.4,
            
              highlight = highlightOptions(
                weight = 5,color = "#7acde9",
                dashArray = "",fillOpacity = 0.7,
                bringToFront = TRUE),
            
              label = labels3)%>%
  addLegend(pal3, values = fifa2018_2$avg.height, opacity = 0.4, title = "Average Height", labFormat = labelFormat(prefix = "Centimeters "), position = "bottomleft")
#####################################################################################################################################################################

#Model Formulating
relationshipmodel <- lm(data = fifa1data, log(wage) ~ nationality + weight)

#Kmeans
nationalcoef <- relationshipmodel$coefficients[2:62]
set.seed(1)
nationalkmean <- kmeans(nationalcoef, 4, nstart = 25)
nationalname = names(relationshipmodel$coefficients)[2:62]
nationalcluster <- as.data.frame(nationalkmean$cluster)
nationalcluster <- cbind(nationalname, nationalcluster,nationalcoef)
colnames(nationalcluster) <- c("nationality","clusternumber","coefficient")

#Benford Law
bf_wage <- benford(fifa1data$wage)

a <- as.character(fifa1data$height)
heightnumber <- as.numeric(substr(a, 3,3))

b <- as.character(fifa1data$weight)
weightnumber <- as.numeric(substr(b,2,2))


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Analysis for Top Soccer Player's Wage", titleWidth = 400),
  
  dashboardSidebar(
    
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Hello", tabName = "a", icon = icon("plane",lib = "glyphicon")),
      menuItem("Map",
               tabName = "Map", icon = icon("map"),
               menuItem("Average Wage",
                        tabName = "Wage",
                        icon = icon("map")
               ),
               menuItem("Average Weight",
                        tabName = "Weight",
                        icon = icon("map")
               ),
               menuItem("Average Height",
                        tabName = "Height",
                        icon = icon("map")
               )
      ),
      menuItem("EDA",
               tabName = "EDA", icon = icon("image",lib = "font-awesome"),
               menuItem("Basic distributions",
                        tabName = "Basic",
                        icon = icon("image")
               ),
               menuItem("Relationships: wage & other",
                        tabName = "Relationship",
                        icon = icon("image")
               )
      ),
      menuItem("Model Analysis", tabName = "Model Analysis", icon = icon("edit"),
               menuItem("Model Summary",
                        tabName = "Summary",
                        icon = icon("edit")),
               menuItem("KMeans Interpretation",
                        tabName = "KMeans",
                        icon = icon("edit"))
               ),
               
      menuItem("Benford Law", tabName = "Bf", icon = icon("copy")),
      menuItem("Player Raw Data", tabName = "Rd", icon = icon("android"))
    )
  ),
  dashboardBody(
    
    tags$style(HTML("
                    .box.box-solid.box-primary>.box-header {
                    background:#e9967a;
                    }
                    ")),

 tabItems(
   tabItem(
     tabName = "a",
     
     fluidRow(
     column(12,
                      box(width=10,solidHeader = TRUE, status = "primary",
                          title="Introduction",
                          h4("In the current world, many famous soccer players are fully appreciated by soccer clubs and fans. 
                             Since they are in top-level clubs, they are paid an attractive salary that many people dream of it. 
                             If we take their nationalities into account, which countries' players would make more money compared to other? 
                             Although we know that a player's wage highly depends on their abilities and which club does he join, for the top players who their abilities are nearly the same,
                             is there a relationship between their nationalities and their wages? 
                             What about their height and weight? 
                             This shiny app is going to reveal to what extent and how does height, weight and nationality affect top soccer player's wages 
                             and how is the accuracy of their wages in this data? ",style = "font-family: 'Lucida Console'" )
                          ) ,
            
            box(width = 5,solidHeader = TRUE, status = "primary",
                title ="More Info",style = "font-family: 'Lucida Console'",
                h4("This shiny app is developed for practice. 
                   In order to do the analysis for the top players in the world,
                   the data of players whose official evaluations are less than 70 are removed. ",style = "font-family: 'Lucida Console'")
                ),
                      box(width=5, solidHeader = TRUE, status = "primary",
                          title="Navigate",
                          h4("Users can be directed to:
                             (1) Map
                             (2) EDA
                             (3) Model Analysis
                             (4) Benford Law
                             (5) Raw Data
                             ",style = "font-family: 'Lucida Console'")
                          )
       ),column(12,
                mainPanel(imageOutput("fuli"))
       )
     ))
   ,
   
   tabItem(
     tabName = "Wage",
     
     fluidRow(
         leafletOutput("wagemap", height = "700")
     )
   ),
   
   tabItem(
     tabName = "Weight",
     fluidRow(
       leafletOutput("weightmap", height = "700")
       )
     
   ),
   tabItem(
     tabName = "Height",
     fluidRow(
       leafletOutput("heightmap", height = "700")
      
     )
     
   ),
   tabItem(
     tabName = "Basic",
     fluidRow(
       
       box(title = "Weight", status = "primary", collapsible = TRUE,
           solidHeader = TRUE, plotlyOutput("plotweight"), width = 6, height = 500),
       
       box( 
         title = "Height", status = "primary", collapsible = TRUE, 
         solidHeader = TRUE, plotlyOutput("plotheight"), width = 6, height = 500),
       
       box(title = "Wage", status = "primary", collapsible = TRUE, 
           solidHeader = TRUE, plotlyOutput("plotwage"), width = 6, height = 500),
       
       box(width = 6,solidHeader = TRUE, status = "primary",
           title ="Findings",style = "font-family: 'Lucida Console'",
           h4("From the distributions of players' weight and height, which seems like normal distributions,
                we can see most of the players are around 180 cm height and 75 kg weight. 
              But for the distribution of players' wage,  as the number of wage increases,
              the number of players who can get this amount decreases rapidly.",style = "font-family: 'Lucida Console'")
           )
     )
   ),
   
   tabItem(
     tabName = "Relationship",
     fluidRow(
       box(title = "Wage and Nationality", status = "primary", collapsible = TRUE, 
           solidHeader = TRUE, plotlyOutput("plotwr"), width = 6, height = 500),
       
       box(title = "Wage and Weight", status = "primary", collapsible = TRUE,
           solidHeader = TRUE, plotlyOutput("plotww"), width = 6, height = 500),

       box(title = "Wage and Height", status = "primary", collapsible = TRUE,
           solidHeader = TRUE, plotlyOutput("plotwwh"), width = 6, height = 500),
       
       box(width = 6,solidHeader = TRUE, status = "primary",
           title ="Findings",style = "font-family: 'Lucida Console'",
           h4("We can see that players from some countries do make more money compared to others. 
              For the relationship between height and wage, 
              I thought that players tend to gain more money if they are taller, 
              but the fact is that height does not have any influence on their wages. 
              By looking at the relationship between weight and wage, 
              what is interesting is that although the influence of players' weight is really tiny, 
              there is still a small tendency that the more weight results in the more wage they get. ",style = "font-family: 'Lucida Console'")
           
       )
      
     )
   ),
   
   tabItem(
     tabName = "Summary",
     fluidRow(
       box(width = 6,solidHeader = TRUE, status = "primary",
           title ="Findings",style = "font-family: 'Lucida Console'",
           h4("The linear model function is log(wage) ~ nationality + weight. 
              The variable Height is removed because it does not seem to have an effect on players' wage by looking at the EDA. From the summary of this model in R, 
              we can clearly find that for a players' weight increases by 1 unit, 
              his wage would increase by 0.003544, which is expected by comparing it with the former EDA result. 
              Compare to weight, nationality has more impact on soccer players' wage. 
              Since different nationalities would have different impacts on players' wage, 
              I am going to use the method K-means clustering to make these nationalities' coefficients into several clusters, 
              which can be viewed in the Further Interpretation tab.",style = "font-family: 'Lucida Console'")
           ),
       box(title = "Summary", status = "primary", collapsible = TRUE, 
           solidHeader = TRUE, verbatimTextOutput("plotsummary"), width = 6, height = 1600)
       
       
     )),
   
   tabItem(
     tabName = "KMeans",
     fluidRow(
       box(title = "Kmeans Table", status = "primary", collapsible = TRUE, 
           solidHeader = TRUE, DT::dataTableOutput("tablekmeans"), width = 6, height = 500),
       
       box(title = "Kmeans Plot", status = "primary", collapsible = TRUE, 
           solidHeader = TRUE, plotlyOutput("plotkmeans"), width = 6, height = 500),
       
       box(width = 6,solidHeader = TRUE, status = "primary",
           title ="Method Explanation",style = "font-family: 'Lucida Console'",
           h4("Since different nationalities would have different impacts on players' wage, 
              the method K-means clustering can be used to make these nationalities' coefficients into several clusters.
              By using this K-means clustering method, nationalities' coefficients are divided into four clusters, in which the means of coefficients are 
              0.41805 (cluster#1)
              , 0.76852 (cluster#2)
              , 0.00025 (cluster#3)
              , -0.69076 (cluster#4).",style = "font-family: 'Lucida Console'")
           
           ),
       
       box(width = 6,solidHeader = TRUE, status = "primary",
           title ="Findings",style = "font-family: 'Lucida Console'",
           h4("After visualizing the data of cluster number into a data frame and a plot, 
              it is easy for us to find that the top soccer players whose nationalities in cluster#2 
              have more chance to get more wages, such as Germany, Croatia, Egypt, England and so on. 
              Also, the top soccer players whose nationalities in cluster#4 
              might get relatively fewer wages, such as Chile, Colombia and so on. ",style = "font-family: 'Lucida Console'")
           
           )
       
       )),
   
   tabItem(
     tabName = "Bf",
     fluidRow(
       
       tabBox(tabPanel(title = "Benford Analysis for Wage", status = "primary", collapsible = TRUE, 
           solidHeader = TRUE, plotOutput("bfplot"), width = 7, height = 500),
       
       tabPanel(title = "Height", status = "primary", collapsible = TRUE, 
           solidHeader = TRUE, plotOutput("bfploth"), width = 7, height = 500),
       
       tabPanel(title = "Weight", status = "primary", collapsible = TRUE, 
           solidHeader = TRUE, plotOutput("bfplotw"), width = 7, height = 500)),
       
       box(width = 6,solidHeader = TRUE, status = "primary",
           title ="Findings",style = "font-family: 'Lucida Console'",
           h4("Now, one question comes to us: Is this data reliable so that our model is appropriate?
              According to the digit distribution of soccer players' wage, 
              the data somehow have a tendency to follow Benford's law, 
              but discrepancies are also clear at around 30,40,50,60,70 and 80. 
              In additon, for the Benford Analysis Plot of height and weight, 
              we can clearly find that height And weight 
              does not follow the tendency of Benford Law. ",style = "font-family: 'Lucida Console'")
       )
       
       
     )),
   
   tabItem(
     tabName = "Rd",
     fluidRow(
       box(title = "Soccer Player Table", status = "primary", collapsible = TRUE, 
           solidHeader = TRUE, DT::dataTableOutput("tableraw"), width = 12, height = 500)
     )
     
   )
   
   
   )))
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$wagemap <- renderLeaflet(map1)

   output$weightmap <- renderLeaflet(map2)
   
   output$heightmap <- renderLeaflet(map3)
   
   output$plotwage <- renderPlotly({ gwage <- ggplot(data = fifa1data, aes(x = wage))+
       geom_bar(color = "darksalmon") + ggtitle("Distribution of Player's wage")
   
   ggplotly(gwage)
   })
   
   output$plotweight <- renderPlotly({ gweight <- ggplot(data = fifa1data, aes(x = weight))+
     geom_bar(color = "darksalmon") + ggtitle("Distribution of Player's weight")
   
   ggplotly(gweight)
   })
   
   output$plotheight <- renderPlotly({ gheight <- ggplot(data = fifa1data, aes(x = height))+
     geom_bar(color = "darksalmon") + ggtitle("Distribution of Player's height")
   
   ggplotly(gheight)
   })
   
   output$plotwr <- renderPlotly({ gwr <- ggplot(fifa1data, aes(x = nationality, y= log(wage))) +
     geom_smooth() + geom_point(color="darksalmon")  + ggtitle("Relationship between nationality and wage")
   
   ggplotly(gwr)
   })
   
   output$plotww <- renderPlotly({ gww <- ggplot(fifa1data, aes(x = weight, y= log(wage))) +
     geom_smooth() + geom_point(color="darksalmon")  + ggtitle("Relationship between weight and wage")
   
   ggplotly(gww)
   })
   
   output$plotwwh <- renderPlotly({ gwwh <- ggplot(fifa1data, aes(x = height, y= log(wage))) +
     geom_smooth() + geom_point(color="darksalmon")  + ggtitle("Relationship between height and wage")
   
   ggplotly(gwwh)
   })
   
   output$plotsummary <- renderPrint({summary(relationshipmodel)})
   
   output$tablekmeans <- DT::renderDataTable({
     DT::datatable(nationalcluster,options = list(searching = TRUE,pageLength = 8,lengthMenu = c(4, 6, 8, 10), scrollX = T,scrollY = "300px"),rownames= FALSE
     )})
   
   output$tableraw <- DT::renderDataTable({
     DT::datatable(fifa1data, options = list(searching = TRUE,pageLength = 8,lengthMenu = c(10, 20, 30, 50, 100), scrollX = T,scrollY = "300px"),rownames= FALSE
     )
   })
     
   output$plotkmeans <- renderPlotly({ a <- ggplot(data= nationalcluster, aes(x=clusternumber, y=coefficient, color=clusternumber, fill= nationality))+ geom_jitter() + theme(legend.position="none")
   ggplotly(a)
   })
   
   output$bfplot <- renderPlot({bf_wage <- benford(fifa1data$wage)
   plot(bf_wage)})
   
   output$bfploth <- renderPlot({bf_height <- benford(heightnumber)
   plot(bf_height)})
   
   output$bfplotw <- renderPlot({bf_weight <- benford(weightnumber)
   plot(bf_weight)})
   
   output$fuli<- renderImage({
     Leg<-"www/fuli.jpg"
     list(src=Leg)
   },deleteFile = FALSE)  
   
}


# Run the application 
shinyApp(ui = ui, server = server)

