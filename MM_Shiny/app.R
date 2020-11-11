# Packages we use
pacman::p_load("tidyverse","drat","maps","tmap","sf","viridis","sp","hurricaneexposuredata","hurricaneexposure","tmaptools",
               "revealjs","shiny","rsconnect")
library(shiny)
library(rsconnect)

# Extracting general data for mapping
addRepo("geanders")
data("hurr_tracks") 
data("rain")


# Pulling map data
ob <- st_as_sf(map('county',plot=F,fill=T))
colnames(county.fips)[2] = 'ID'
ob <- merge(ob, county.fips, by="ID")


# Filtering target hurr_tracks data
dt_o <- hurr_tracks %>% filter(storm_id == "One-2009")
dt_e <- hurr_tracks %>% filter(storm_id == "Earl-2010")
dt_i <- hurr_tracks %>% filter(storm_id == "Irene-2011")


# Filtering target rain data
ra_o <- rain %>% filter(storm_id == "One-2009") %>% group_by(fips) %>%
  summarise('storm_id'=storm_id[1],'precip'=sum(precip))

ra_e <- rain %>% filter(storm_id == "Earl-2010") %>% group_by(fips) %>%
  summarise('storm_id'=storm_id[1],'precip'=sum(precip))

ra_i <- rain %>% filter(storm_id == "Irene-2011") %>% group_by(fips) %>%
  summarise('storm_id'=storm_id[1],'precip'=sum(precip))


# Changing fips to numerics
ra_o$fips <- as.numeric(ra_o$fips)
ra_e$fips <- as.numeric(ra_e$fips)
ra_i$fips <- as.numeric(ra_i$fips)


# Merging data by fips variable
ra_o <- merge(ob, ra_o, by="fips")
ra_e <- merge(ob, ra_e, by="fips")
ra_i <- merge(ob, ra_i, by="fips")


# Pulling the coordinates for each hurricane path
One <- cbind(dt_o$longitude,dt_o$latitude)%>%
  Line()%>%Lines(ID='One-2009')%>%
  list()%>%SpatialLines()
Earl <- cbind(dt_e$longitude,dt_e$latitude)%>%
  Line()%>%Lines(ID='Earl-2010')%>%
  list()%>%SpatialLines()
Irene <- cbind(dt_i$longitude,dt_i$latitude)%>%
  Line()%>%Lines(ID='Irene-2011')%>%
  list()%>%SpatialLines()


# Organize map data
ra_o <- rain %>% filter(storm_id == "One-2009") %>% group_by(fips) %>%
  summarise(storm_id=storm_id[1],precip=sum(precip))
ra.new.o <- as.data.frame(ra_o)
ra.new.o$fips <- as.numeric(ra.new.o$fips)

ra_e <- rain %>% filter(storm_id == "Earl-2010") %>% group_by(fips) %>%
  summarise(storm_id=storm_id[1],precip=sum(precip))
ra.new.e <- as.data.frame(ra_e)
ra.new.e$fips <- as.numeric(ra.new.e$fips)

ra_i <- rain %>% filter(storm_id == "Irene-2011") %>% group_by(fips) %>%
  summarise(storm_id=storm_id[1],precip=sum(precip))
ra.new.i <- as.data.frame(ra_i)
ra.new.i$fips <- as.numeric(ra.new.i$fips)


# Make a new data frame
data.rain.o<- rain %>% filter(storm_id=="One-2009")
data.rain.o<-data.frame(data.rain.o[1], data.rain.o[2], data.rain.o[4], data.rain.o[5], data.rain.o[6])

data.rain.e<- rain %>% filter(storm_id=="Earl-2010")
data.rain.e<-data.frame(data.rain.e[1], data.rain.e[2], data.rain.e[4], data.rain.e[5], data.rain.e[6])

data.rain.i<- rain %>% filter(storm_id=="Irene-2011")
data.rain.i<-data.frame(data.rain.i[1], data.rain.i[2], data.rain.i[4], data.rain.i[5], data.rain.i[6])


# Merging data by fips variable
ra.new.o <- merge(ob, ra.new.o, by="fips")
ra.new.e <- merge(ob, ra.new.e, by="fips")
ra.new.i <- merge(ob, ra.new.i, by="fips")

png(width=100, height =100)
t_F_o <- tm_shape(ra.new.o)+
  tm_polygons(col='precip',title="Rainfall (mm)")+
  tm_legend(position=c("right","bottom"))+
  tm_shape(One)+
  tm_lines(col='red')+
  tm_layout(main.title='One-2009',main.title.position = "center") 
t_F_o
dev.off()

png(width=100, height =100)
t_F_e = tm_shape(ra.new.e)+
  tm_polygons(col='precip',title="Rainfall (mm)")+
  tm_legend(position=c("right","bottom"))+
  tm_shape(Earl)+
  tm_lines(col='red')+
  tm_layout(main.title='Earl-2010',main.title.position = "center") 
t_F_e
dev.off()

png(width=100, height =100)
t_F_i = tm_shape(ra.new.i)+
  tm_polygons(col='precip',title="Rainfall (mm)")+
  tm_legend(position=c("right","bottom"))+
  tm_shape(Irene)+
  tm_lines(col='red')+
  tm_layout(main.title='Irene-2011',main.title.position = "center") 
t_F_i
dev.off()


# Saving map images
tmap_save(t_F_o, filename= "one.png")
tmap_save(t_F_o, filename ="one.html")

tmap_save(t_F_e, filename= "earl.png")
tmap_save(t_F_e, filename ="earl.html")

tmap_save(t_F_i, filename= "irene.png")
tmap_save(t_F_i, filename ="irene.html")



ui <- fluidPage(
  title = "Examples of Data Tables",
  sidebarLayout(
    tabsetPanel(
      conditionalPanel(
        'input.dataset === "hurr_tracks"'),
      conditionalPanel(
        'input.dataset === "hurr_tracks"'),
      conditionalPanel(
        'input.dataset === "hurr_tracks"')
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Hurricane One-2009",
                 tmapOutput("my_tmap1"),
                 
                 # Creating a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("fips",
                                      "Fips:",
                                      c("All",
                                        unique(as.character(data.rain.i$fips))))
                   ),
                   column(4,
                          selectInput("storm",
                                      "Storm ID:",
                                      c("All",
                                        unique(data.rain.i$storm_id)))
                          
                          
                   ),
                   column(4,
                          selectInput("lag",
                                      "Lag:",
                                      c("All",
                                        unique(as.character(data.rain.i$lag))))
                   ),
                 ),
                 # Creating a new row for the table
                 DT::dataTableOutput("table1")),
        tabPanel("Hurricane Earl-2010",
                 tmapOutput("my_tmap2"),
                 
                 # Creating a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("fips",
                                      "Fips:",
                                      c("All",
                                        unique(as.character(data.rain.i$fips))))
                   ),
                   column(4,
                          selectInput("storm",
                                      "Storm ID:",
                                      c("All",
                                        unique(data.rain.i$storm_id)))
                          
                          
                   ),
                   column(4,
                          selectInput("lag",
                                      "Lag:",
                                      c("All",
                                        unique(as.character(data.rain.i$lag))))
                   ),
                 ),
                 # Creating a new row for the table
                 DT::dataTableOutput("table2")),
        tabPanel("Hurricane Irene-2011",
                 tmapOutput("my_tmap3"),
                 
                 # Creating a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("fips",
                                      "Fips:",
                                      c("All",
                                        unique(as.character(data.rain.i$fips))))
                   ),
                   column(4,
                          selectInput("storm",
                                      "Storm ID:",
                                      c("All",
                                        unique(data.rain.i$storm_id)))
                          
                          
                   ),
                   column(4,
                          selectInput("lag",
                                      "Lag:",
                                      c("All",
                                        unique(as.character(data.rain.i$lag))))
                   ),
                 ),
                 # Creating a new row for the table
                 DT::dataTableOutput("table3"))
        
      )
    )   
  ))

server <- function(input, output) {
  
  # Filtering data based on selections
  output$my_tmap1 = renderTmap({
    tm_shape(ra.new.o)+
      tm_polygons(col='precip',title="Rainfall (mm)")+
      tm_legend(position=c("right","bottom"))+
      tm_shape(One)+
      tm_lines(col='red')+
      tm_layout(main.title='One-2009',main.title.position = "center") 
  })
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- data.rain.o
    if (input$storm != "All") {
      data <- data[data.rain.o$storm_id == input$storm,]
    }
    if (input$fips != "All") {
      data <- data[data.rain.o$storm_id == input$storm,]
    }
    
    data
  }))
  
  # Sorted columns are colored now because CSS are attached to them
  # Filtering data based on selections
  output$my_tmap2 = renderTmap({
    tm_shape(ra.new.e)+
      tm_polygons(col='precip',title="Rainfall (mm)")+
      tm_legend(position=c("right","bottom"))+
      tm_shape(Earl)+
      tm_lines(col='red')+
      tm_layout(main.title='Earl-2010',main.title.position = "center") 
  })
  output$table2 <- DT::renderDataTable(DT::datatable({
    data2 <- data.rain.e
    if (input$storm != "All") {
      data2 <- data2[data.rain.e$storm_id == input$storm,]
    }
    if (input$fips != "All") {
      data2 <- data2[data.rain.e$fips == input$fips,]
    }
    
    data2
  }))
  
  output$my_tmap3 = renderTmap({
    tm_shape(ra.new.i)+
      tm_polygons(col='precip',title="Rainfall (mm)")+
      tm_legend(position=c("right","bottom"))+
      tm_shape(Irene)+
      tm_lines(col='red')+
      tm_layout(main.title='Irene-2011',main.title.position = "center") 
  })
  output$table3 <- DT::renderDataTable(DT::datatable({
    data3 <- data.rain.i
    if (input$storm != "All") {
      data3 <- data3[data.rain.i$storm_id == input$storm,]
    }
    if (input$fips != "All") {
      data3 <- data3[data.rain.i$fips == input$fips,]
    }
    
    data3
  }))
  
  
}

# Running the application 
shinyApp(ui = ui, server = server) 