library(shiny)
library(ggplot2)
library(graphics)
library(plotly, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(stats, warn.conflicts = FALSE)
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(stats))
library(shinythemes)
library(leaflet)
library(scales)

sindhcities <-read.csv("Map_Data.csv", header = TRUE, sep = ",")

rownames(sindhcities) <- c("District_Province", "Karachi Central", "Karachi East", "Karachi Korangi", "Karachi Malir", "Karachi South", "Karachi West", "Karachi Division", "Badin", "Dadu", "Hyderabad", "Jamshoro", "Matiari", "Sujawal", "Tando Allahyar", "Tando Muhammad Khan", "Thatta", "Hyderabad Division", "Ghotki", "Khairpur", "Sukkur", "Sukkur Division", "Mirpur Khas", "Umer Kot", "Mirpurkhas Division", "Naushahro Feroze", "Sanghar", "Shaheed Benazir Abad", "SBA Division", "Jacobabad", "Kambar ShahdadKot", "Kashmore", "Larkana", "Shikarpur", "Larkana Division", "Sindh Total")

smax <- rownames(sindhcities)[which.max(sindhcities$Total_Coverage_Percentage)]
smin <- rownames(sindhcities)[which.min(sindhcities$Total_Coverage_Percentage)]

P1data <- read.csv("P1.csv", header = TRUE, sep = ",")
P2data <- read.csv("P2.csv", header = TRUE, sep = ",")
P3data <- read.csv("P3.csv", header = TRUE, sep = ",")
P4data <- read.csv("P4.csv", header = TRUE, sep = ",")

l <- list(
    font = list(size = 12,
        family = "sans-serif",
        color = "#000"),
    bgcolor = "#E2E2E2",
    bordercolor = "#FFFFFF",
    borderwidth = 0.5)

cp1 <- read.csv("Cp1.csv", header = TRUE, sep = ",")
cp2 <- read.csv("Cp2.csv", header = TRUE, sep = ",")
cp3 <- read.csv("Cp3.csv", header = TRUE, sep = ",")
cp4 <- read.csv("Cp4.csv", header = TRUE, sep = ",")
cp5 <- read.csv("Cp5.csv", header = TRUE, sep = ",")
cp6 <- read.csv("Cp6.csv", header = TRUE, sep = ",")
cp7 <- read.csv("Cp7.csv", header = TRUE, sep = ",")

vdata <- read.csv("Experiment2.csv", header = TRUE, sep = ",")

dplot <- read.csv("Distribution.csv", header = TRUE, sep = ",")

lvr <- label_number_si(accuracy=0.1)(dplot$Vaccination_Received)
ltu <- label_number_si(accuracy=0.1)(dplot$Total_Dose_Utilization)
lw <- label_number_si(accuracy=0.1)(dplot$Wastage)
lb <- label_number_si(accuracy=0.1)(dplot$Balance)

pvr <- paste(format(round(dplot$Vaccination_Received / 1e6, 1), trim = TRUE), "M")
pw <- paste(format(round(dplot$Wastage / 1e6, 1), trim = TRUE), "M")
pb <- paste(format(round(dplot$Balance / 1e6, 1), trim = TRUE), "M")

dp1 <- c(dplot$Vaccines, dplot$Vaccination_Received, dplot$Total_Dose_Utilization)
dp2 <- c(dplot$Vaccines, dplot$Vaccination_Received, dplot$Wastage)
dp3 <- c(dplot$Vaccines, dplot$Vaccination_Received, dplot$Balance)

dplot1 <- data.frame(dp1)
dplot2 <- data.frame(dp2)
dplot3 <- data.frame(dp3)

ctable <- read.csv("Coverage.csv", header = TRUE, sep = ",")

dtable <- read.csv("Distribution.csv", header = TRUE, sep = ",")



ui <- fluidPage( theme = shinytheme("flatly"),
                 
                 navbarPage(
                   title = 'COVID Vaccine Utilization in Districts of Sindh',
                   
                   tabPanel(('Map'),
                            bootstrapPage(
                              leafletOutput("sindhcities", height = "625px"))),
                   
                   tabPanel(('Coverage Plots'),
                            dashboardPage(
                              dashboardHeader(disable = TRUE),
                              dashboardSidebar(selectInput("select1", "Select1", label = h4("Select a District or Division"), width = "300", 
                                                           choices = list("Karachi Central" = "Karachi_Central", "Karachi East" = "Karachi_East", "Karachi Korangi" = "Karachi_Korangi", "Karachi Malir" = "Karachi_Malir", "Karachi South" = "Karachi_South", "Karachi West" = "Karachi_West", "Karachi Division" = "Karachi_Division", "Badin" = "Badin", "Dadu" = "Dadu", "Hyderabad" = "Hyderabad", "Jamshoro" = "Jamshoro", "Matiari" = "Matiari", "Sujawal" = "Sujawal", "Tando Allahyar" = "Tando_Allahyar", "Tando Muhammad Khan" = "Tando_Muhammad_Khan", "Thatta" = "Thatta", "Hyderabad Division" = "Hyderabad_Division", "Ghotki" = "Ghotki", "Khairpur" = "Khairpur", "Sukkur" = "Sukkur", "Sukkur Division" = "Sukkur_Division", "Mirpur Khas" = "Mirpur_Khas", "Tharparkar" = "Tharparkar", "Umer Kot" = "Umer_Kot", "Mirpurkhas Division" = "Mirpurkhas_Division", "Naushahro Feroze" = "Naushahro_Feroze", "Sanghar" = "Sanghar", "Shaheed Benazir Abad" = "Shaheed_Benazir_Abad", "SBA Division" = "SBA_Division", "Jacobabad" = "Jacobabad", "Kambar ShahdadKot" = "Kambar_Shahdad_Kot", "Kashmore" = "Kashmore", "Larkana" = "Larkana", "Shikarpur" = "Shikarpur", "Larkana Division" = "Larkana_Division", "Sindh Total" = "Sindh_Total")
                              )),
                              dashboardBody(
                                tags$head(tags$style(HTML('
      .skin-blue .main-sidebar {
        background-color: grey;
      }
      .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
        background-color: #111111;
      }
    '))),
                                fluidRow(
                                  column(width = 6,
                                         box(title = "Daily Coverage",
                                             solidHeader = T,
                                             width = NULL,
                                             collapsible = F, 
                                             plotlyOutput("PlotTest2", height = "240px")),
                                         box(title = "Total Dose 24 Hours", 
                                             solidHeader = T,
                                             width = NULL, 
                                             collapsible = F,
                                             plotlyOutput("PlotTest3", height = "240px"))
                                  ),
                                  
                                  column(width = 6,
                                         box(title = "Total Coverage",
                                             solidHeader = T,
                                             width = NULL, 
                                             collapsible = F,
                                             plotlyOutput("PlotTest1", height = "240px")),
                                         box(title = "Total Dose", 
                                             solidHeader = T,
                                             width = NULL, 
                                             collapsible = F,
                                             plotlyOutput("PlotTest4", height = "240px"))
                                  ))))),                                                                        
                   
                   
                   tabPanel(('Coverage Comparison'),
                            dashboardPage(
                              dashboardHeader(disable = T),
                              dashboardSidebar(
                                sidebarMenu(radioButtons("visuBtn", label = h4("Choose a Comparison"), choices = c('Population versus Total First Dose', 'Per Day Target versus First Dose 24 Hours'), selected = 'Population versus Total First Dose')
                                )
                                
                              ),
                              dashboardBody(
                                tags$head(tags$style(HTML('
      .skin-blue .main-sidebar {
        background-color: grey;
      }
      .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
        background-color: #111111;
      }
    '))),
                                fluidRow(
                                  box(title = "Karachi",
                                      solidHeader = T,
                                      width = 4,
                                      collapsible = F, 
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Population versus Total First Dose'",
                                        plotlyOutput('plot1a', height = "225px")
                                      ),
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Per Day Target versus First Dose 24 Hours'",
                                        plotlyOutput('plot1b', height = "225px")
                                      )
                                  ),
                                  box(title = "Hyderabad",
                                      solidHeader = T,
                                      width = 5,
                                      collapsible = F, 
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Population versus Total First Dose'",
                                        plotlyOutput('plot2a', height = "225px")
                                      ),
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Per Day Target versus First Dose 24 Hours'",
                                        plotlyOutput('plot2b', height = "225px")
                                      )
                                  ),
                                  box(title = "Sukkur",
                                      solidHeader = T,
                                      width = 3,
                                      collapsible = F, 
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Population versus Total First Dose'",
                                        plotlyOutput('plot3a', height = "225px")
                                      ),
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Per Day Target versus First Dose 24 Hours'",
                                        plotlyOutput('plot3b', height = "225px")
                                      )
                                  ),
                                  box(title = "Mirpurkhas Division",
                                      solidHeader = T,
                                      width = 3,
                                      collapsible = F, 
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Population versus Total First Dose'",
                                        plotlyOutput('plot4a', height = "225px")
                                      ),
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Per Day Target versus First Dose 24 Hours'",
                                        plotlyOutput('plot4b', height = "225px")
                                      )
                                  ),
                                  box(title = "Shaheed Benazir Abad Division",
                                      solidHeader = T,
                                      width = 3,
                                      collapsible = F, 
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Population versus Total First Dose'",
                                        plotlyOutput('plot5a', height = "207px")
                                      ),
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Per Day Target versus First Dose 24 Hours'",
                                        plotlyOutput('plot5b', height = "207px")
                                      )
                                  ),
                                  box(title = "Larkana Division",
                                      solidHeader = T,
                                      width = 4,
                                      collapsible = F, 
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Population versus Total First Dose'",
                                        plotlyOutput('plot6a', height = "225px")
                                      ),
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Per Day Target versus First Dose 24 Hours'",
                                        plotlyOutput('plot6b', height = "225px")
                                      )
                                  ),
                                  box(title = "Sindh Total",
                                      solidHeader = T,
                                      width = 2,
                                      collapsible = F, 
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Population versus Total First Dose'",
                                        plotlyOutput('plot7a', height = "225px")
                                      ),
                                      conditionalPanel(
                                        condition = "input.visuBtn == 'Per Day Target versus First Dose 24 Hours'",
                                        plotlyOutput('plot7b', height = "225px")
                                      )
                                  )
                                  
                                  
                                ))
                            )),
                   
                   tabPanel(('Vaccine Utilization'),
                            dashboardPage(
                              dashboardHeader(disable = TRUE),
                              dashboardSidebar(selectInput("elect", "elect", label = h4("Select a Vaccine"), 
                                                           choices = list("Sinopharm" = 'Sinopharm',	"Sinovac" = 'Sinovac',	"Cansino" = 'Cansino',	"Astrazeneca"	= 'Astrazeneca', "Moderna" = 'Moderna',	"Pakvac" = 'Pakvac',	"Pfizer" = 'Pfizer',	"Total Vaccines" = 'Total_Vaccines'
                                                           )),
                                               HTML(paste(p(strong(" *Balance"), ("   Vaccination Received - (Total"),("   Dose Utilization + Wastage)"), style = "white-space: pre-wrap")
                                               ))),
                              dashboardBody(
                                tags$head(tags$style(HTML('
      .skin-blue .main-sidebar {
        background-color: grey;
      }
      .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
        background-color: #111111;
      }
    '))),
                                fluidRow(
                                  box(title = "Vaccines Utilization",
                                      solidHeader = T,
                                      width = 12,
                                      collapsible = F, 
                                      plotlyOutput("Test2", height = "400px"))
                                )))),
                   
                   tabPanel(('Distribution Comparison'),
                            dashboardPage(
                              dashboardHeader(disable = T),
                              dashboardSidebar(
                                sidebarMenu(radioButtons(inputId = "my_choices", label = h4("Choose a Comparison"), choices = c("Vaccines Recieved Versus Total Dose Utilization", "Vaccines Recieved Versus Wastage", "Vaccines Recieved Versus Balance"), selected = "Vaccines Recieved Versus Total Dose Utilization"), width=3,
                                            HTML(paste(p(strong(" *Balance"), ("   Vaccination Received - (Total"),("   Dose Utilization + Wastage)"), style = "white-space: pre-wrap")
                                            )))),
                              
                              dashboardBody(
                                tags$head(tags$style(HTML('
      .skin-blue .main-sidebar {
        background-color: grey;
      }
      .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
        background-color: #111111;
      }
    '))),
                                fluidRow(
                                  box(title = "Vaccines Comparison",
                                      solidHeader = T,
                                      width = 12,
                                      collapsible = F, 
                                      conditionalPanel(
                                        condition = "input.my_choices == 'Vaccines Recieved Versus Total Dose Utilization'",
                                        plotlyOutput("my_test3", height = "400px")
                                      ),
                                      conditionalPanel(
                                        condition = "input.my_choices == 'Vaccines Recieved Versus Wastage'",
                                        plotlyOutput("my_test4", height = "400px")
                                      ),
                                      conditionalPanel(
                                        condition = "input.my_choices == 'Vaccines Recieved Versus Balance'",
                                        plotlyOutput("my_test5", height = "400px")
                                      )
                                      
                                  ))))
                   ),
                   
                   navbarMenu(('More'),
                              tabPanel(('Data'),
                                       tabsetPanel(
                                         tabPanel("Coverage", DT::dataTableOutput("mytable1")),
                                         tabPanel("Distribution", DT::dataTableOutput("mytable2")))),
                              
                              
                              tabPanel("About", fluid = TRUE,
                                       fluidRow(
                                         column(6,
                                                #br(),
                                                h4(p("Background")),
                                                h5(p("Vaccine hesitancy hinders the vaccination programs, as is seen in the case for the COVID-19 vaccination program. This results in low uptake of vaccines and thus government efforts to reach their objectives are hampered. Such is the case with Sindh Province of Pakistan, where despite government's much effort, official statistics aren't satisfactory. Vaccination figures in almost all cities of the Province are lower than targets. However, there is a positive trend observed in distribution of various vaccine brands. Number show that most of received vaccine supply was utilized. Having this information, the current project is aimed at visualizing coverage and distribution trends of the Province through infographics.")),
                                                br(),
                                                h4(p("App Functionality")),
                                                h5(p("The aim of this app is to shows an infographic summary of vaccine utilization in the province of Sindh, Pakistan.  It is intended to report total and daily coverage for the first shot received within the past five months. The first three tabs show coverage and the rest of the two summarize distribution.")),
                                                br(),
                                                h4(p("Coverage")),
                                                h5(p("The first tab provides a broad overview of vaccine utility per city, It allows the user the hover over the cities on the map to see the population covered. The second tab expands on the first tab and allows the user to choose a city from the drop-down menu and reacts by displaying bar charts for the following coverage measures :")),
                                                h5(p("*   Daily Coverage Percentage")),
                                                h5(p("*   Vaccine Administered Total Dose 24 Hours")),
                                                h5(p("*   Total Coverage Percentage")),
                                                h5(p("*   Vaccine Administered Total Dose")),
                                         ),
                                         column(6,
                                                br(),
                                                h5(p("Finally the third tab shows two sets of plots that compare the following coverage measures across all districts and divisions:")),
                                                h5(p("*   Above 18 Population versus Total First Dose")),
                                                h5(p("*   Per Day Target versus First Dose 24 Hours")),
                                                br(),
                                                h4(p("Distribution")),
                                                h5(p("The fourth tab provides shows the actual utilization of each vaccine by brand name in Pakistan, It allows the user to choose a vaccine from the drop-down menu and reacts by displaying the utilization of the chosen vaccine. The fifth and the final tab app shows three sets of plots that compare the following distribution parameters across all available vaccines:")),
                                                h5(p("*   Vaccines Recieved Versus Total Dose Utilization")),
                                                h5(p("*   Vaccines Recieved Versus Wastage")),
                                                h5(p("*   Vaccines Recieved Versus Balance")),
                                                br(),
                                                h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at hassanshah18@gmail.com"),
                                                   p("The source code for this Shiny app is available ", a("on github", href = "https://github.com/HasShah"), ".")),
                                                
                                                #hr(),
                                                
                                                br(),
                                                h4(p("About the Author")),
                                                h5(p("Hasan Shah is a Civil Engineer.")),
                                                br(),
                                         )
                                       ),
                                       br(),
                                       hr(),
                                       h5("Sources:"),
                                       h6(
                                         p("Coverage and Distribution Information from",
                                           a("Health Department of Sindh",
                                             href = "https://www.sindhhealth.gov.pk/"))),
                                       h5("Built with",
                                          img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                          "by",
                                          img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                                          ".")
                              )
                   )
                 )
)

server <- function(input, output, session){
  
  output$sindhcities <- renderLeaflet({
    
    sindhcities <- sindhcities%>%mutate(popup_info=paste(District_Province,	"br/>", lat, "br/>",	long, "br/>",	Total_Coverage_Percentage, "br/>"))
    
    pal <- colorBin(c('red', 'blue'),
                    domain = sindhcities$Total_Coverage_Percentage
    )
    
    leaflet(data = sindhcities) %>%
      addTiles() %>%
      
      setView(lng = 68.5247, lat = 26.55, zoom = 7) %>%
      
      addCircleMarkers(lat = ~lat, lng = ~long, weight = 1, radius = ~sqrt(Total_Coverage_Percentage) * 3, popup = ~paste("<b>District:<b>", District_Province,"<br>" ,"<b>Total Coverage Percentage:<b>", Total_Coverage_Percentage, sep = " "), color = ~pal(Total_Coverage_Percentage)) %>%
      
      addLegend(position = "topleft", pal = pal, values = ~Total_Coverage_Percentage, labFormat = labelFormat(suffix = "%"),
                title = paste(h5("Proportion of Population covered in %"),h6("Min:", smin),h6("Max:", smax), sep = " "), opacity = 0.7) 
    
  })
  
  
  output$PlotTest1 <- renderPlotly({
    
    P1data$Areas1 <- factor(P1data$Areas1, levels = P1data[["Areas1"]])
    
    plot_ly(P1data, x = ~Areas1, y = ~get(input$select1), type = 'bar', color = ~Areas1, colors = "RdPu", showlegend = F) %>% 
      
      add_text(text=~get(input$select1), hoverinfo='none', textposition = 'top', cliponaxis = FALSE, showlegend = FALSE, 
               textfont=list(size="auto", color="black")) %>%
      layout(legend = l, xaxis = list(zeroline = F, showgrid = F, title = "", tickangle = 0),
             yaxis = list(zeroline = F, showgrid = F, title = "", range="auto"))
  }) 
  
  output$PlotTest2 <- renderPlotly({
    
    P2data$Areas2 <- factor(P2data$Areas2, levels = P2data[["Areas2"]])
    
    plot_ly(P2data, x = ~Areas2, y = ~get(input$select1), type = 'bar', color = ~Areas2, colors = "Purples", showlegend = F) %>% 
      
      add_text(text=~get(input$select1), hoverinfo='none', textposition = 'top', cliponaxis = FALSE, showlegend = FALSE, 
               textfont=list(size="auto", color="black")) %>%
      layout(legend = l, xaxis = list(zeroline = F, showgrid = F, title = "", tickangle = 0),
             yaxis = list(zeroline = F, showgrid = F, title = "", range="auto"))
  })
  
  output$PlotTest3 <- renderPlotly({
    plot_ly(P3data, x = ~Areas3, y = ~get(input$select1), type = 'bar', color = ~Areas3, colors = "BuGn", showlegend = F) %>% 
      
      add_text(text=~get(input$select1), hoverinfo='none', textposition = 'top', cliponaxis = FALSE, showlegend = FALSE, 
               textfont=list(size="auto", color="black")) %>%
      layout(legend = l, xaxis = list(zeroline = F, showgrid = F, title = "", tickangle = 0),
             yaxis = list(zeroline = F, showgrid = F, title = "", range="auto"))
  })
  
  output$PlotTest4 <- renderPlotly({
    plot_ly(P4data, x = ~Areas4, y = ~get(input$select1), type = 'bar', color = ~Areas4, colors = "Pastel1", showlegend = F) %>% 
      
      add_text(text=~get(input$select1), hoverinfo='none', textposition = 'top', cliponaxis = FALSE, showlegend = FALSE, 
               textfont=list(size="auto", color="black")) %>%
      layout(legend = l, xaxis = list(zeroline = F, showgrid = F, title = "", tickangle = 0),
             yaxis = list(zeroline = F, showgrid = F, title = "", range="auto"))
  })
  
  
  output$plot1a <- renderPlotly({
    
    cp1$District_Province <- factor(cp1$District_Province, levels = cp1[["District_Province"]])
    
    fig <- plot_ly(cp1, type = 'bar', source = "p1Source", x = ~District_Province, y = ~Above_18_Population, name = 'Population', marker = list(color = 'rgb(26, 118, 255)'))
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Total_Vaccine_Administered_First_Dose, name = 'Total First Dose', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(legend = l, xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(labels = FALSE, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,10000000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
    fig <- fig %>% layout(legend = list(x = -100, y = -0.9))%>%
      event_register('plotly_restyle')
  })
  
  output$plot1b <- renderPlotly({
    
    cp1$District_Province <- factor(cp1$District_Province, levels = cp1[["District_Province"]])
    
    fig <- plot_ly(cp1, type = 'bar', source = "p2Source", x = ~District_Province, y = ~Per_Day_Target_For_5_Months, name = 'Per Day Target', marker = list(color = 'rgb(26, 118, 255)'))
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Vaccine_Administered_First_Dose_24_Hours, name = 'First Dose 24 Hours', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(legend = l, xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(labels = FALSE, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,50000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
    fig <- fig %>% layout(legend = list(x = -100, y = -0.9))%>%
      event_register('plotly_restyle')
  })
  
  
  output$plot2a <- renderPlotly({
    
    cp2$District_Province <- factor(cp2$District_Province, levels = cp2[["District_Province"]])
    
    fig <- plot_ly(cp2, type = "bar", showlegend = FALSE, source = "p1Source", x = ~District_Province, y = ~Above_18_Population, name = 'Population', marker = list(color = 'rgb(26, 118, 255)')) 
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Total_Vaccine_Administered_First_Dose, name = 'Total First Dose', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,10000000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
    
  })
  
  output$plot2b <- renderPlotly({
    
    cp2$District_Province <- factor(cp2$District_Province, levels = cp2[["District_Province"]])
    
    fig <- plot_ly(cp2, type = 'bar', source = "p2Source", showlegend = FALSE, x = ~District_Province, y = ~Per_Day_Target_For_5_Months, name = 'Per Day Target', marker = list(color = 'rgb(26, 118, 255)'))
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Vaccine_Administered_First_Dose_24_Hours, name = 'First Dose 24 Hours', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(labels = FALSE, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,50000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
  })
  
  output$plot3a <- renderPlotly({
    
    cp3$District_Province <- factor(cp3$District_Province, levels = cp3[["District_Province"]])
    
    fig <- plot_ly(cp3, type = "bar", showlegend = FALSE, source = "p1Source", x = ~District_Province, y = ~Above_18_Population, name = 'Population', marker = list(color = 'rgb(26, 118, 255)')) 
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Total_Vaccine_Administered_First_Dose, name = 'Total Vaccine Administered First Dose', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,10000000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
    
  })
  
  output$plot3b <- renderPlotly({
    
    cp3$District_Province <- factor(cp3$District_Province, levels = cp3[["District_Province"]])
    
    fig <- plot_ly(cp3, type = 'bar', source = "p2Source", showlegend = FALSE, x = ~District_Province, y = ~Per_Day_Target_For_5_Months, name = 'Per Day Target', marker = list(color = 'rgb(26, 118, 255)'))
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Vaccine_Administered_First_Dose_24_Hours, name = 'First Dose 24 Hours', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(labels = FALSE, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,50000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
  })
  
  
  output$plot4a <- renderPlotly({
    
    cp4$District_Province <- factor(cp4$District_Province, levels = cp4[["District_Province"]])
    
    fig <- plot_ly(cp4, type = 'bar', source = "p1Source", showlegend = FALSE, x = ~District_Province, y = ~Above_18_Population, name = 'Population', marker = list(color = 'rgb(26, 118, 255)'))
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Total_Vaccine_Administered_First_Dose, name = 'Total First Dose', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(labels = FALSE, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,10000000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group') 
  })
  
  output$plot4b <- renderPlotly({
    
    cp4$District_Province <- factor(cp4$District_Province, levels = cp4[["District_Province"]])
    
    fig <- plot_ly(cp4, type = 'bar', source = "p2Source", showlegend = FALSE, x = ~District_Province, y = ~Per_Day_Target_For_5_Months, name = 'Per Day Target', marker = list(color = 'rgb(26, 118, 255)'))
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Vaccine_Administered_First_Dose_24_Hours, name = 'First Dose 24 Hours', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(labels = FALSE, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,50000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
  })
  
  output$plot5a <- renderPlotly({
    
    cp5$District_Province <- factor(cp5$District_Province, levels = cp5[["District_Province"]])
    
    fig <- plot_ly(cp5, type = "bar", showlegend = FALSE, source = "p1Source", x = ~District_Province, y = ~Above_18_Population, name = 'Population', marker = list(color = 'rgb(26, 118, 255)')) 
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Total_Vaccine_Administered_First_Dose, name = 'Total First Dose', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,10000000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
    
  })
  
  output$plot5b <- renderPlotly({
    
    cp5$District_Province <- factor(cp5$District_Province, levels = cp5[["District_Province"]])
    
    fig <- plot_ly(cp5, type = 'bar', source = "p2Source", showlegend = FALSE, x = ~District_Province, y = ~Per_Day_Target_For_5_Months, name = 'Per Day Target', marker = list(color = 'rgb(26, 118, 255)'))
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Vaccine_Administered_First_Dose_24_Hours, name = 'First Dose 24 Hours', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(labels = FALSE, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,50000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
  })
  
  output$plot6a <- renderPlotly({
    
    cp6$District_Province <- factor(cp6$District_Province, levels = cp6[["District_Province"]])
    
    fig <- plot_ly(cp6, type = "bar", showlegend = FALSE, source = "p1Source", x = ~District_Province, y = ~Above_18_Population, name = 'Population', marker = list(color = 'rgb(26, 118, 255)')) 
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Total_Vaccine_Administered_First_Dose, name = 'Total Vaccine Administered First Dose', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,10000000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
    
  })
  
  output$plot6b <- renderPlotly({
    
    cp6$District_Province <- factor(cp6$District_Province, levels = cp6[["District_Province"]])
    
    fig <- plot_ly(cp6, type = 'bar', source = "p2Source", showlegend = FALSE, x = ~District_Province, y = ~Per_Day_Target_For_5_Months, name = 'Per Day Target', marker = list(color = 'rgb(26, 118, 255)'))
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Vaccine_Administered_First_Dose_24_Hours, name = 'First Dose 24 Hours', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(labels = FALSE, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,50000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
  })
  
  output$plot7a <- renderPlotly({
    
    cp7$District_Province <- factor(cp7$District_Province, levels = cp7[["District_Province"]])
    
    fig <- plot_ly(cp7, type = "bar", showlegend = FALSE, source = "p1Source", x = ~District_Province, y = ~Above_18_Population, name = 'Population', marker = list(color = 'rgb(26, 118, 255)')) 
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Total_Vaccine_Administered_First_Dose, name = 'Total Vaccine Administered First Dose', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,10000000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
  })
  
  output$plot7b <- renderPlotly({
    
    cp7$District_Province <- factor(cp7$District_Province, levels = cp7[["District_Province"]])
    
    fig <- plot_ly(cp7, type = 'bar', source = "p2Source", showlegend = FALSE, x = ~District_Province, y = ~Per_Day_Target_For_5_Months, name = 'Per Day Target', marker = list(color = 'rgb(26, 118, 255)'))
    
    fig <- fig %>% add_trace(x = ~District_Province, y = ~Vaccine_Administered_First_Dose_24_Hours, name = 'First Dose 24 Hours', marker = list(color = 'rgb(55, 83, 109)'))
    fig <- fig %>% layout(xaxis = list(zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = -30),
                          yaxis = list(labels = FALSE, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), range=c(0,220000)),
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
  })
  
  plot2aProxy <- plotlyProxy("plot2a", session)
  plot3aProxy <- plotlyProxy("plot3a", session)
  plot4aProxy <- plotlyProxy("plot4a", session)
  plot5aProxy <- plotlyProxy("plot5a", session)
  plot6aProxy <- plotlyProxy("plot6a", session)
  plot7aProxy <- plotlyProxy("plot7a", session)
  
  
  observe({
    restyle_events <- event_data(source = "p1Source", "plotly_restyle")
    plotlyProxyInvoke(plot2aProxy, "restyle", restyle_events[[1]], restyle_events[[2]])
    plotlyProxyInvoke(plot3aProxy, "restyle", restyle_events[[1]], restyle_events[[2]])
    plotlyProxyInvoke(plot4aProxy, "restyle", restyle_events[[1]], restyle_events[[2]])
    plotlyProxyInvoke(plot5aProxy, "restyle", restyle_events[[1]], restyle_events[[2]])
    plotlyProxyInvoke(plot6aProxy, "restyle", restyle_events[[1]], restyle_events[[2]])
    plotlyProxyInvoke(plot7aProxy, "restyle", restyle_events[[1]], restyle_events[[2]])
    
    
  })
  
  plot2bProxy <- plotlyProxy("plot2b", session)
  plot3bProxy <- plotlyProxy("plot3b", session)
  plot4bProxy <- plotlyProxy("plot4b", session)
  plot5bProxy <- plotlyProxy("plot5b", session)
  plot6bProxy <- plotlyProxy("plot6b", session)
  plot7bProxy <- plotlyProxy("plot7b", session)
  
  
  observe({
    restyle_events <- event_data(source = "p2Source", "plotly_restyle")
    plotlyProxyInvoke(plot2bProxy, "restyle", restyle_events[[1]], restyle_events[[2]])
    plotlyProxyInvoke(plot3bProxy, "restyle", restyle_events[[1]], restyle_events[[2]])
    plotlyProxyInvoke(plot4bProxy, "restyle", restyle_events[[1]], restyle_events[[2]])
    plotlyProxyInvoke(plot5bProxy, "restyle", restyle_events[[1]], restyle_events[[2]])
    plotlyProxyInvoke(plot6bProxy, "restyle", restyle_events[[1]], restyle_events[[2]])
    plotlyProxyInvoke(plot7bProxy, "restyle", restyle_events[[1]], restyle_events[[2]])
    
  })
  
  output$Test2 <- renderPlotly({
    
    vdata$Areas <- factor(vdata$Areas, levels = vdata[["Areas"]])
    
    Test2 <- plot_ly(
      vdata, x = ~Areas, y = ~get(input$elect), type = "bar", color = ~Areas, colors = "BuGn")
    Test2 <- Test2 %>% 
      add_text(text=~get(input$elect), hoverinfo='none', textposition = 'top', showlegend = FALSE, 
               textfont=list(size="auto", color="black")) 
    Test2 <- Test2 %>% layout(legend = l, xaxis = list(zeroline = F, showgrid = F, title = "", tickangle = 0),
                              yaxis = list(zeroline = F, showgrid = F, title = ""))
  })
  
  
  output$my_test3 <- output$my_test3a <- renderPlotly({
    
    dplot$Vaccines <- factor(dplot$Vaccines, levels = dplot[["Vaccines"]])
    
    fig <- dplot3 %>% plot_ly(x = ~dplot$Vaccines, y = ~dplot$Vaccination_Received, type = 'bar',name = "Vaccination Recieved", text = lvr, textposition = 'outside', 
                              textfont=list(size=12, color="black"), cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)'))
    
    fig <- fig %>% add_trace(y = ~dplot$Total_Dose_Utilization, type = 'bar',name = "Total Dose Utilization", text = ltu, textposition = 'outside', 
                             textfont=list(size=12, color="black"), cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) 
    
    fig <- fig %>% layout(legend = l, xaxis = list(zeroline = F, showgrid = F, title = "", tickangle = 0),
                          yaxis = list(zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
    
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  })
  
  output$my_test4 <- output$my_test4a <- renderPlotly({
    
    dplot$Vaccines <- factor(dplot$Vaccines, levels = dplot[["Vaccines"]])
    
    fig <- dplot3 %>% plot_ly(x = ~dplot$Vaccines, y = ~dplot$Vaccination_Received, type = 'bar',name = "Vaccination Recieved", text = lvr, textposition = 'outside', 
                              textfont=list(size=12, color="black"), cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)'))
    
    fig <- fig %>% add_trace(y = ~dplot$Wastage, type = 'bar',name = "Wastage", text = lw, textposition = 'outside', 
                             textfont=list(size=12, color="black"), cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) 
    
    fig <- fig %>% layout(legend = l, xaxis = list(zeroline = F, showgrid = F, title = "", tickangle = 0),
                          yaxis = list(zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
    
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  })
  
  output$my_test5 <- output$my_test5a <- renderPlotly({
    
    dplot$Vaccines <- factor(dplot$Vaccines, levels = dplot[["Vaccines"]])
    
    fig <- dplot3 %>% plot_ly(x = ~dplot$Vaccines, y = ~dplot$Vaccination_Received, type = 'bar',name = "Vaccination Recieved", text = lvr, textposition = 'outside', 
                              textfont=list(size=12, color="black"), cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)'))
    
    fig <- fig %>% add_trace(y = ~dplot$Balance, type = 'bar',name = "Balance", text = lb, textposition = 'outside', 
                             textfont=list(size=12, color="black"), cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) 
    
    fig <- fig %>% layout(legend = l, xaxis = list(tickfont = list(size = "auto"), zeroline = F, showgrid = F, title = "", tickangle = 0),
                          yaxis = list(tickfont = list(size = "auto"), zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                          barmode = 'group')
    
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  })
  
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(ctable, options = list(orderClasses = TRUE))
  })
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(dtable, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  
}


shinyApp(ui = ui, server = server)