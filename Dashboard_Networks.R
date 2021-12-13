## app.R ##

library(shiny)
library(shinydashboard)
library(tidyverse)
library(sna)
library(maps)
library(ggrepel)
library(ggplot2)
library(ggnetwork)

RCP45 <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/RCP_4.5_Analog_Count_Map.csv")
RCP85 <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/RCP_8.5_Analog_Count_Map.csv")
usa_map <- map_data('state') 
options(ggrepel.max.overlaps = 0)
RCP45 |> select(State, Nearest_ID) |> distinct() |> arrange(State, Nearest_ID) |> drop_na()
RCP85 |> select(State, Nearest_ID) |> distinct() |> arrange(State, Nearest_ID) |> drop_na()

Idaho <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/Database/Idaho_value.csv")
Oregon <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/Database/Oregon_value.csv")
Washington <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/Database/Washington_value.csv")

All_data_RCP45 <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/Database/Alldata_RCP45.csv")
All_data_RCP45$model = factor(All_data_RCP45$model, levels=c("gridmet","Analog", "RCP45" ))

All_data_RCP85 <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/Database/Alldata_RCP85.csv")
All_data_RCP85$model = factor(All_data_RCP85$model, levels=c("gridmet","Analog", "RCP85" ))

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Climate Analog PNW Dashboard" ),
                    dashboardSidebar(sidebarMenu(
                      menuItem("Ag_in_PNW", tabName = "Ag_in_PNW", icon = icon("tree")),
                      menuItem("Idaho ", tabName = "Idaho", icon = icon("tree")),
                      menuItem("Oregon ", tabName = "Oregon", icon = icon("tree")),
                      menuItem("Washington ", tabName = "Washington", icon = icon("tree")),
                      menuItem("Analog ", tabName = "Analog", icon = icon("tree"))
                    )
                    ),
                    dashboardBody(
                      tabItems( 
                        tabItem("Idaho",
                                fluidRow(box(plotOutput("Idaho1")), box(dataTableOutput("Idahotable"))),
                                fluidRow(box(plotOutput("Idaho3")), box(plotOutput("Idaho4")))
                        ),
                        tabItem("Oregon",
                                fluidRow(box(plotOutput("Oregon1")), box(dataTableOutput("Oregontable"))),
                                fluidRow(box(plotOutput("Oregon3")), box(plotOutput("Oregon4")))
                        ),
                        tabItem("Washington",
                                fluidRow(box(plotOutput("Washington1")), box(dataTableOutput("Washingtontable"))),
                                fluidRow(box(plotOutput("Washington3")), box(plotOutput("Washington4")))
                        ),
                        tabItem("Analog",
                                selectInput("State", "State", choices = RCP45 %>% 
                                              select(State) %>% 
                                              distinct() %>% 
                                              arrange(State) %>% 
                                              drop_na()),
                                selectInput("Nearest_ID", "Nearest_ID", choices = RCP45 %>% 
                                              select(Nearest_ID) %>% 
                                              distinct() %>% 
                                              arrange(Nearest_ID) %>% 
                                              drop_na()),
                                fluidRow(box(plotOutput("Analog")), box(plotOutput("Analog1")))),
                        tabItem("Ag_in_PNW",
                                fluidRow(imageOutput("header")),
                                fluidRow(box(htmlOutput("text")),
                                         tags$head(tags$style("#text{color: Darkblue;
                                 font-size: 19px;
                                 }"
                                         )
                                         ),imageOutput("crop"))
                                
                        )
                      )
                    )
)

server <- function(input, output) {
  
  output$my_image <- renderImage({
    list(src="E:/R_Tutorial/1/www/image2.png",width=1700, height=200,alt="something went wrong")
  }, deleteFile = FALSE)
  
  output$header <- renderImage({
    list(src="E:/R_Tutorial/1/www/image5.png",width=1700, height=400, alt="something went wrong")
  }, deleteFile = FALSE)
  
  output$crop <- renderImage({
    list(src="E:/R_Tutorial/1/www/image4.png",width=800, height=500, alt="something went wrong")
  }, deleteFile = FALSE)
  
  output$text1 <- renderText({paste0("Pacific Northwest growers feed people across the United States and around the world.
                                      Agriculture is woven into the fabric of Washington's, Oregon's, and Idaho's heritage 
                                      and has been an important part of the Pacific Northwest's culture since the earliest days of territorial settlement.
                                      Farmers and ranchers steward over 40 million acres of the region's lands.From the Pacific Ocean to the Rocky Mountains, 
                                      an unusually diverse climate and landscape make the Pacific Northwest one of the most vital - 
                                      and most threatened - farming regions in the country.Hay, grass seed and specialty crops dominate working lands west of the Cascade Mountains,
                                      whereas much of the nation's wheat, potatoes, apples and pears are produced east of the Cascade Mountains,
                                      west of the Rocky Mountains, and along the Snake River Plain. Some important crops include: wheat, barley, 
                                      oats, hay, dry beans, dry peas, as well as specialty crops such as apples and other tree fruits, peonies,
                                      cranberry, grapes, hazelnuts, hops, spearmint, strawberries, blueberries, and various cane berries. 
                                      Throughout the region dairy and beef cattle, sheep, lambs, hogs, pigs, goats, and chickens are raised. 
                                      The agricultural sector is deeply tied to livelihoods in both rural as well as urban and tribal communities.
                                      Oregon, Washington, and Idaho together are the #1 producer of 22 key agricultural commodities.
                                      The agriculture industry generates hundreds of thousands of jobs and billions of dollars in sales 
                                      every year and in certain rural parts of the region, it is the primary economic engine for growth")})
  
  output$text2 <- renderText({paste("Some important crops include: wheat, barley, 
                                      oats, hay, dry beans, dry peas, as well as specialty crops such as apples and other tree fruits, peonies,
                                      cranberry, grapes, hazelnuts, hops, spearmint, strawberries, blueberries, and various cane berries. 
                                      Throughout the region dairy and beef cattle, sheep, lambs, hogs, pigs, goats, and chickens are raised. 
                                      The agricultural sector is deeply tied to livelihoods in both rural as well as urban and tribal communities.
                                      Oregon, Washington, and Idaho together are the #1 producer of 22 key agricultural commodities.
                                      The agriculture industry generates hundreds of thousands of jobs and billions of dollars in sales 
                                      every year and in certain rural parts of the region, it is the primary economic engine for growth")})
  
  output$text <- renderUI({
    str1 <- paste("")
    str2 <- paste("<ui><li>Produce from the Pacific Northwest growers feed people across United States adn World.", input$var)
    
    str3 <- paste("")
    str4 <- paste("<ui><li>From the Pacific Ocean to the Rocky Mountains,an unusually diverse climate and landscape make the Pacific Northwest one of the most vital - 
                                      and most threatened - farming regions in the country.", input$range[1])
    str5 <- paste("")
    
    str6 <- paste("<ui><li>Some important crops include: wheat, barley,oats, hay, dry beans, dry peas, as well as specialty crops such as 
        apples and other tree fruits, peonies,cranberry, grapes, hazelnuts, hops, spearmint, strawberries, blueberries, and 
                      various cane berries.", input$range[1])
    str7 <- paste("")
    
    str8 <- paste("<ui><li>Throughout the region dairy and beef cattle, sheep, lambs, hogs, pigs, goats, and chickens are raised.",
                  input$range[1])
    
    str9 <- paste("")
    
    str12 <- paste("<ui><li>The agriculture industry generates hundreds of thousands of jobs and billions of dollars in sales 
                                      every year and in certain rural parts of the region, it is the primary economic engine for growth.",
                   input$range[1])
    
    str13 <- paste("")
    
    str14 <- paste("<ui><li>Reference: https://www.climatehubs.usda.gov/hubs/northwest/topic/agriculture-northwest",input$range[1])
    
    HTML(paste(str2,str3, str4,str5,str6,str7,str8,str9,str12,str13,str14, sep = '<br/>'))
  })
  
  
  output$PNW <- renderPlot({ggplot()+geom_point(data=RCP45, aes(x=Nearest_ID, y=Count))+
      labs(y="Latitude", x="Longitude")+theme_bw()
  })
  
  
  output$PNW1 <- renderPlot({ggplot()+geom_point(data=RCP45, aes(x=Nearest_ID, y=Count))+
      labs(y="Latitude", x="Longitude")+theme_bw()
  })
  
  output$Idahotable <- renderDataTable(Idaho, options = list(lengthMenu = c(6, 10, 15), pageLength = 6))
  
  output$Idaho1 <- renderPlot({RCP45 |>  filter(State =="Idaho") |> ggplot() +
      geom_map(data = usa_map, map = usa_map, aes(map_id = region),
               color = 'gray15',
               fill = 'white')+scale_size(range = c(1))+
      xlim(c(-125, -110)) + ylim(c(40, 50)) +
      geom_point(aes(x = Lon.from,
                     y = Lat.from,
                     size = Nearest_ID,
                     colour = State_County),size=3)+labs(y="Latitude", x="Longitude")+
      ggtitle("Top 10 Ag counties in Idaho as per Census 2017")+
      coord_equal() +
      theme_bw()
  })
  
  output$Idaho3 <- renderPlot({ggplot((All_data_RCP45 |>  filter(State =="Idaho")), aes(x=model, y=Frostfree_day,fill=model)) + 
      geom_text((All_data_RCP45 |>  filter(State =="Idaho")),mapping= aes(y = 160, label = Future,color=Future),position = position_dodge(width = 2))+
      geom_violin(trim=FALSE)+coord_flip()+geom_boxplot(width=0.1)+facet_wrap(~State_Coun)+theme_bw()+ facet_wrap(~State_Coun)+
      theme(legend.position = "none",axis.ticks.x=element_blank(),axis.text.x=element_blank())+ggtitle("Future of county under consideration for RCP4.5")
  })
  
  output$Idaho4 <- renderPlot({ggplot((All_data_RCP85 |>  filter(State =="Idaho")), aes(x=model, y=Frostfree_day,fill=model)) + 
      geom_text((All_data_RCP85 |>  filter(State =="Idaho")),mapping= aes(y = 160, label = Future,color=Future),position = position_dodge(width = 2))+
      geom_violin(trim=FALSE)+coord_flip()+geom_boxplot(width=0.1)+facet_wrap(~State_Coun)+theme_bw()+ facet_wrap(~State_Coun)+
      theme(legend.position = "none",axis.ticks.x=element_blank(),axis.text.x=element_blank())+ggtitle("Future of county under consideration for RCP8.5")
  })
  
  output$Oregon1 <- renderPlot({RCP45 |>  filter(State =="Oregon") |> ggplot() +
      geom_map(data = usa_map, map = usa_map, aes(map_id = region),
               color = 'gray15',
               fill = 'white')+scale_size(range = c(1))+
      xlim(c(-125, -110)) + ylim(c(40, 50)) +
      geom_point(aes(x = Lon.from,
                     y = Lat.from,
                     size = Nearest_ID,
                     colour = State_County),size=3)+labs(y="Latitude", x="Longitude")+
      ggtitle("Top 10 Ag counties in Oregon as per Census 2017")+
      coord_equal() +
      theme_bw()
  })
  
  output$Oregontable <- renderDataTable(Oregon, options = list(lengthMenu = c(6, 10, 15), pageLength = 6))
  
  output$Oregon3 <- renderPlot({ggplot((All_data_RCP45 |>  filter(State =="Oregon")), aes(x=model, y=Frostfree_day,fill=model)) + 
      geom_text((All_data_RCP45 |>  filter(State =="Oregon")),mapping= aes(y = 160, label = Future,color=Future),position = position_dodge(width = 2))+
      geom_violin(trim=FALSE)+coord_flip()+geom_boxplot(width=0.1)+facet_wrap(~State_Coun)+theme_bw()+
      theme(legend.position = "none",axis.ticks.x=element_blank(),axis.text.x=element_blank())+ggtitle("Future of county under consideration for RCP4.5")
  })
  
  output$Oregon4 <- renderPlot({ggplot((All_data_RCP85 |>  filter(State =="Oregon")), aes(x=model, y=Frostfree_day,fill=model)) + 
      geom_text((All_data_RCP85 |>  filter(State =="Oregon")),mapping= aes(y = 160, label = Future,color=Future),position = position_dodge(width = 2))+
      geom_violin(trim=FALSE)+coord_flip()+geom_boxplot(width=0.1)+facet_wrap(~State_Coun)+theme_bw()+
      theme(legend.position = "none",axis.ticks.x=element_blank(),axis.text.x=element_blank())+ggtitle("Future of county under consideration for RCP8.5")
  })
  
  output$Washington1 <- renderPlot({RCP45 |>  filter(State =="Washington") |> ggplot() +
      geom_map(data = usa_map, map = usa_map, aes(map_id = region),
               color = 'gray15',
               fill = 'white')+scale_size(range = c(1))+
      xlim(c(-125, -110)) + ylim(c(40, 50)) +
      geom_point(aes(x = Lon.from,
                     y = Lat.from,
                     size = Nearest_ID,
                     colour = State_County),size=3)+labs(y="Latitude", x="Longitude")+
      ggtitle("Top 10 Ag counties in Washington as per Census 2017")+
      coord_equal() +
      theme_bw()
  })
  
  output$Washingtontable <- renderDataTable(Washington, options = list(lengthMenu = c(6, 10, 15), pageLength = 6))
  
  output$Washington3 <- renderPlot({ggplot((All_data_RCP45 |>  filter(State =="Washington")), aes(x=model, y=Frostfree_day,fill=model)) + 
      geom_text((All_data_RCP45 |>  filter(State =="Washington")),mapping= aes(y = 180, label = Future,color=Future),position = position_dodge(width = 2))+
      geom_violin(trim=FALSE)+coord_flip()+geom_boxplot(width=0.1)+facet_wrap(~State_Coun)+theme_bw()+
      theme(legend.position = "none",axis.ticks.x=element_blank(),axis.text.x=element_blank())+ggtitle("Future of county under consideration for RCP4.5")
  })
  
  output$Washington4 <- renderPlot({ggplot((All_data_RCP85 |>  filter(State =="Washington")), aes(x=model, y=Frostfree_day,fill=model)) + 
      geom_text((All_data_RCP85 |>  filter(State =="Washington")),mapping= aes(y = 180, label = Future,color=Future),position = position_dodge(width = 2))+
      geom_violin(trim=FALSE)+coord_flip()+geom_boxplot(width=0.1)+facet_wrap(~State_Coun)+theme_bw()+
      theme(legend.position = "none",axis.ticks.x=element_blank(),axis.text.x=element_blank())+ggtitle("Future of county under consideration for RCP8.5")
  })
  
  output$Analog <- renderPlot({
    (RCP45 |>  filter(State == input$State) |>
       filter(Nearest_ID == input$Nearest_ID)) 
    ggplot() +
      geom_map(data = usa_map, map = usa_map, aes(map_id = region),
               color = 'gray15',
               fill = 'white')+
      geom_segment(data= (RCP45 |>  filter(State == input$State) |>
                            filter(Nearest_ID == input$Nearest_ID)), alpha = 0.9,color = "black",
                   aes(x = Lon.from, y = Lat.from,
                       xend = Lon.to , yend = Lat.to), arrow = arrow(length = unit(0.25, "cm")))+
      scale_size(range = c(1))+
      xlim(c(-125, -68)) + ylim(c(25, 50)) +
      geom_point(data= (RCP45 |>  filter(State == input$State) |>
                          filter(Nearest_ID == input$Nearest_ID)),aes(x = Lon.from,
                                                                      y = Lat.from,
                                                                      size = Nearest_ID,
                                                                      colour = State_County),size=3)+
      geom_point(data= (RCP45 |>  filter(State == input$State) |>
                          filter(Nearest_ID == input$Nearest_ID)),aes(x = Lon.to,
                                                                      y = Lat.to,
                                                                      colour = State_County),size=3)+labs(y="Latitude", x="Longitude")+
      ggtitle("Closest location as per sigma distance for RCP4.5")+
      coord_equal() +
      theme_bw()
  })
  
  output$Analog1 <- renderPlot({
    (RCP85 |>  filter(State == input$State) |>
       filter(Nearest_ID == input$Nearest_ID)) 
    ggplot() +
      geom_map(data = usa_map, map = usa_map, aes(map_id = region),
               color = 'gray15',
               fill = 'white')+
      geom_segment(data= (RCP85 |>  filter(State == input$State) |>
                            filter(Nearest_ID == input$Nearest_ID)), alpha = 0.9,color = "black",
                   aes(x = Lon.from, y = Lat.from,
                       xend = Lon.to , yend = Lat.to), arrow = arrow(length = unit(0.25, "cm")))+
      scale_size(range = c(1))+
      xlim(c(-125, -68)) + ylim(c(25, 50)) +
      geom_point(data= (RCP85 |>  filter(State == input$State) |>
                          filter(Nearest_ID == input$Nearest_ID)),aes(x = Lon.from,
                                                                      y = Lat.from,
                                                                      size = Nearest_ID,
                                                                      colour = State_County),size=3)+
      geom_point(data= (RCP85 |>  filter(State == input$State) |>
                          filter(Nearest_ID == input$Nearest_ID)),aes(x = Lon.to,
                                                                      y = Lat.to,
                                                                      colour = State_County),size=3)+labs(y="Latitude", x="Longitude")+
      ggtitle("Closest location as per sigma distance for RCP8.5")+
      coord_equal() +
      theme_bw()
  })
  
}

shinyApp(ui, server)
