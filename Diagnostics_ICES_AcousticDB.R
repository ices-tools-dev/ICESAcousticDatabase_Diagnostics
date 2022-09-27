# Shiny app to visualize the input data from the ICES Acoustic database
#Created by Stefanie Haase (Thünen Institute of Baltic Sea Fisheries)


# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


# Load libraries ----------------------------------------------------------

library(rsconnect)
library(shiny)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(ggplot2)
library(mapplots) # draw.pie
data("coast") # coastlines
library(maps)
library(mapdata)
library(gdata)  # drop.levels
library(reshape) # melt and cast
library(gstat)
library(sp)
library(tidyr)
library(dplyr)

# Define User interface ---------------------------------------------------


ui <- fluidPage(
  titlePanel("Diagnostics of the ICES Acoustic database"),
   tabsetPanel(
     tabPanel("Import data", 

  fluidRow(
    column(width = 6,
           h2('Import Biotic file'),
           dataTableOutput('mytable1'),
           fileInput('file1', 'Choose csv to upload',
                     accept = c('.csv',
                       buttonLabel = "Upload...")
           ),
           tags$hr(),
           radioButtons("file", "Show table",
                                            choices = c(Biotic = "df1",
                                                        Acoustic = "df2"),
                                            selected = "df1")
          # selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading
          # column(dataTableOutput("table_display1"),width=4)
    ),
    column(width = 6,
           h2('Import Acoustic File'),
           
           dataTableOutput('mytable2'),
           
           fileInput('file2', 'Choose csv to upload',
                     accept = c(
                       'text/csv',
                       '.csv',
                       buttonLabel = "Upload..."
                     ))),
         mainPanel(
           
           tableOutput("contents")
  ))),
            
    tabPanel("Biotic diagnostics",
              selectInput(inputId = "species",
                          label = "Choose a species:",
                          choices = c("Herring", "Sprat")),
              selectInput(inputId = "Plot_abiotic",
                          label = "Choose a figure:",
                          choices = c("Species distribution", "Length distribution overall", "Length distribution per haul", "Weight-length relationship overall", "Weight-length relationship per haul", "Age distribution overall measured", "Age distribution per haul measured","Age-length-key overall", "Age-length-key per haul", "Haul distribution")),
             plotOutput("plot_biotic")
               #,"length distribution per SD", "Catch overview map"
               
             ),
    tabPanel("Acoustic diagnostics", 
             fluidRow(
               column(6,
                      h4('Brush over points in the map to get details about SA values'),plotOutput("plot", brush="plot_brush")), # brush = brushOpts(id="plot_brush",click="plot_click"
                      verbatimTextOutput("info"),
             column(4,
                    h4("Brushed points"),
                    tableOutput("data")))#verbatimTextOutput("brush_info")
    )
    
  
))
  


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$contents <- renderTable({
    req(input$file1)
    req(input$file2)
    ncol <- max(count.fields(input$file2$datapath, sep = ","))
     df1 <- read.csv(input$file1$datapath,
                    header = F,
                    sep = ",",
                    quote = "")
     df2 <- read.csv(input$file2$datapath,
                     header = F,   
                     col.names = paste0("V", seq_len(ncol)),
                     fill=TRUE,
                     sep = ",",
                     quote = "")
     
      if(input$file == "df1") {
                 return(df1)
               }
               else {
              return(df2)
      }
     

  
   })
  
   output$plot <- renderPlot({
  req(input$file2)
  #ncol <- max(input$file2$datapath, sep = ",")
  df2 <- read.table(input$file2$datapath,
                  header = FALSE,
                  fill=TRUE,
                  col.names = paste0("V", seq_len(29)),
                  sep = ",",
                  quote = "")

   
  
  acoustic_data <- df2[df2$V1=="Data",]
  
  
  col_names <- as.character(acoustic_data[1,])
  colnames(acoustic_data) <- c(col_names)
  acoustic_data <- acoustic_data[-1,]
  
  cruiseTracks <- acoustic_data
  
  cruiseTracks <- as.data.frame(cbind(  as.numeric(as.character(cruiseTracks$LogLatitude)),
                                        as.numeric(as.character(cruiseTracks$LogLongitude)),
                                        as.numeric(as.character(cruiseTracks$LogDistance))))
  
  colnames(cruiseTracks) <- c('LogLatitude','LogLongitude','LogDistance')
  
  agg <- aggregate(cruiseTracks,
                   by = list(cruiseTracks$LogDistance),
                   FUN = max)
  saVec <- as.data.frame(as.numeric(as.character(acoustic_data$DataValue)))
  
  colnames(saVec) <- c('SA')
  
  agg_1 <- aggregate(saVec,
                     by = list(cruiseTracks$LogDistance),
                     FUN = sum)
  
  
  SA.df <- as.data.frame(cbind(agg_1$SA,agg$LogLatitude,agg$LogLongitude))
  colnames(SA.df) <- c('SA','LogLatitude','LogLongitude')
  input$newplot

  map('worldHires', 
      col = "green", 
      fill = TRUE, 
      xlim = c(min(cruiseTracks$LogLongitude)-0.5, 
               max(cruiseTracks$LogLongitude)+0.5), 
      ylim = c(min(cruiseTracks$LogLatitude)-0.5, 
               max(cruiseTracks$LogLatitude)+0.5))
  box()
  axis(side = 2, las = 2)
  points(SA.df$LogLongitude, SA.df$LogLatitude, cex = log(SA.df$SA)/2, col = "darkgrey")
  points(cruiseTracks$LogLongitude, cruiseTracks$LogLatitude, pch = 46, col = "red")

  #draw.shape(coast, col = "darkgreen")

   }, height=1000, width=550)
   
   output$plot_biotic <- renderPlot({
     req(input$file1)
     df1 <- read.csv(input$file1$datapath,
                     header = F,
                     sep = ",",
                     quote = "")
     length_data <- df1[df1$V1=="Catch",]
     col_names <- as.character(length_data[1,])
     colnames(length_data) <- c(col_names)
     length_data <- length_data[-1,]
     age_data <- df1[df1$V1=="Biology",]
     col_names <- as.character(age_data[1,])
     colnames(age_data) <- c(col_names)
     age_data <- age_data[-1,]
     
     haul_data <- df1[df1$V1=="Haul",]
     col_names <- as.character(haul_data[1,])
     colnames(haul_data) <- c(col_names)
     haul_data <- haul_data[-1,]
     
     if(input$Plot_abiotic=="Length distribution overall"){
     if(input$species == "Herring") { 
       length_herring <- length_data[length_data$CatchSpeciesCode=="126417",]
       input$newplot
       ggplot(data=length_herring, aes(x=as.integer(as.character(CatchLengthClass)) , y=as.integer(as.character(CatchNumberAtLength))))+
         geom_col() +
         xlab("Length class (mm)")+ ylab("Number")+
         theme_bw()
     }
     else {
       length_sprat <- length_data[length_data$CatchSpeciesCode=="126425",]
       input$newplot
       ggplot(data=length_sprat, aes(x=as.integer(as.character(CatchLengthClass)), y=as.integer(as.character(CatchNumberAtLength))))+
         geom_col(orientation = "x")+
         xlab("Length class (mm)")+ ylab("Number")+
         theme_bw()
     }}else{
       
       if(input$Plot_abiotic=="Length distribution per haul"){
         if(input$species == "Herring") { 
           length_herring <- length_data[length_data$CatchSpeciesCode=="126417",]
           input$newplot
           ggplot(data=length_herring, aes(x=as.integer(as.character(CatchLengthClass)), y=as.integer(as.character(CatchNumberAtLength))))+
             geom_col() +
             xlab("Length class (mm)")+ ylab("Number")+
             theme_bw()+
             facet_wrap(~HaulNumber)
             
         }
         else {
           length_sprat <- length_data[length_data$CatchSpeciesCode=="126425",]
           input$newplot
           ggplot(data=length_sprat, aes(x=as.integer(as.character(CatchLengthClass)), y=as.integer(as.character(CatchNumberAtLength))))+
             geom_col(binwidth=1, alpha=0.9)+
             xlab("Length class (mm)")+ ylab("Number")+
             theme_bw()+
             facet_wrap(~HaulNumber)
         }}else{
     
     if(input$Plot_abiotic=="Age distribution overall measured"){
       if(input$species == "Herring") { 
         age_herring <- age_data[age_data$CatchSpeciesCode=="126417",]
         input$newplot
         ggplot(data=age_herring, aes(x=as.integer(as.character(BiologyIndividualAge)) ))+
           geom_histogram(binwidth=1, alpha=0.9) +
           xlab("Age (years)")+ ylab("Number")+
           theme_bw()
       }
       else {
         age_sprat <- age_data[age_data$CatchSpeciesCode=="126425",]
         input$newplot
         ggplot(data=age_sprat, aes(x=as.integer(as.character(BiologyIndividualAge))))+
           geom_histogram(binwidth=1, alpha=0.9)+
           xlab("Age (years")+ ylab("Number")+
           theme_bw()
       }} else{

           if(input$Plot_abiotic=="Age distribution per haul measured"){
             if(input$species == "Herring") { 
               age_herring <- age_data[age_data$CatchSpeciesCode=="126417",]
               input$newplot
               ggplot(data=age_herring, aes(x=as.integer(as.character(BiologyIndividualAge)) ))+
                 geom_histogram(binwidth=1, alpha=0.9) +
                 xlab("Age (years)")+ ylab("Number")+
                 theme_bw()+
                 facet_wrap(~HaulNumber)
             }
             else {
               age_sprat <- age_data[age_data$CatchSpeciesCode=="126425",]
               input$newplot
               ggplot(data=age_sprat, aes(x=as.integer(as.character(BiologyIndividualAge))))+
                 geom_histogram(binwidth=1, alpha=0.9)+
                 xlab("Age (years)")+ ylab("Number")+
                 theme_bw()+
                 facet_wrap(~HaulNumber)
             }} else{
               
               
               if(input$Plot_abiotic=="Weight-length relationship per haul"){
                 if(input$species == "Herring") { 
                   age_herring <- age_data[age_data$CatchSpeciesCode=="126417",]
                   input$newplot
                   ggplot(data=age_herring, aes(x=as.integer(as.character(BiologyLengthClass)), y=as.integer(as.character(BiologyIndividualWeight))))+
                     geom_count( alpha=0.9)+
                     xlab("Length (mm)")+ ylab("Weight (g)")+
                     theme_bw()+
                     facet_wrap(~HaulNumber)
                 }
                 else {
                   age_sprat <- age_data[age_data$CatchSpeciesCode=="126425",]
                   input$newplot
                   ggplot(data=age_sprat, aes(x=as.integer(as.character(BiologyLengthClass)), y=as.integer(as.character(BiologyIndividualWeight))))+
                     geom_count( alpha=0.9)+
                     xlab("Length (mm)")+ ylab("Weight (g)")+
                     theme_bw()+
                     facet_wrap(~HaulNumber)
                 }} else{
                   
                   
                   if(input$Plot_abiotic=="Weight-length relationship overall"){
                     if(input$species == "Herring") { 
                       age_herring <- age_data[age_data$CatchSpeciesCode=="126417",]
                       input$newplot
                       ggplot(data=age_herring, aes(x=as.integer(as.character(BiologyLengthClass)), y=as.integer(as.character(BiologyIndividualWeight))))+
                         geom_count( alpha=0.9) +
                         xlab("Length (mm)")+ ylab("Weight (g)")+
                         theme_bw()
                     }
                     else {
                       age_sprat <- age_data[age_data$CatchSpeciesCode=="126425",]
                       input$newplot
                       ggplot(data=age_sprat, aes(x=as.integer(as.character(BiologyLengthClass)), y=as.integer(as.character(BiologyIndividualWeight))))+
                         geom_count( alpha=0.9)+
                         xlab("Length (mm)")+ ylab("Weight (g)")+
                         theme_bw()
                 }} else{
        
         if(input$Plot_abiotic=="Age-length-key per haul"){
           if(input$species == "Herring") { 
             age_herring <- age_data[age_data$CatchSpeciesCode=="126417",]
             input$newplot
             ggplot(data=age_herring, aes(x=as.integer(as.character(BiologyLengthClass)), y=as.integer(as.character(BiologyIndividualAge))))+
               geom_count(alpha=0.9) +
               xlab("Length (mm)")+ ylab("Age (years)")+
               theme_bw()+
               facet_wrap(~HaulNumber)
           }
           else {
             age_sprat <- age_data[age_data$CatchSpeciesCode=="126425",]
             input$newplot
             ggplot(data=age_sprat, aes(x=as.integer(as.character(BiologyLengthClass)), y=as.integer(as.character(BiologyIndividualAge))))+
               geom_count( alpha=0.9)+
               xlab("Length (mm)")+ ylab("Age (years)")+
               theme_bw()+
               facet_wrap(~HaulNumber)
           }} else {
             
             if(input$Plot_abiotic=="Age-length-key overall"){
               if(input$species == "Herring") { 
                 age_herring <- age_data[age_data$CatchSpeciesCode=="126417",]
                 input$newplot
                 ggplot(data=age_herring, aes(x=as.integer(as.character(BiologyLengthClass)), y=as.integer(as.character(BiologyIndividualAge))))+
                   geom_count(alpha=0.9) +
                   xlab("Length (mm)")+ ylab("Age (years)")+
                   theme_bw()
               }
               else {
                 age_sprat <- age_data[age_data$CatchSpeciesCode=="126425",]
                 input$newplot
                 ggplot(data=age_sprat, aes(x=as.integer(as.character(BiologyLengthClass)), y=as.integer(as.character(BiologyIndividualAge))))+
                   geom_count( alpha=0.9)+
                   xlab("Length (mm)")+ ylab("Age (years)")+
                   theme_bw()
               
           }} else {
             
             if(input$Plot_abiotic=="Species distribution"){
               
               species_distr <- distinct(length_data[,1:12])
               species_codes <- data.frame("Species" = c("Clupea harengus","Sprattus sprattus", "Gadus morhua", "Gasterosteus aculeatus", "Merluccius merluccius", "Melanogrammus aeglefinus", "Merlangius merlangus", "Ammodytes spp", "Engraulis Encrasicolus"),
                                           "CatchSpeciesCode" = c("126417", "126425", "126436","126505", "126484", "126437", "126438", "126751", "126426"))
 
               species_distr <- left_join(species_distr, species_codes, by="CatchSpeciesCode")
               input$newplot
             
                    
               ggplot(data=species_distr, aes( x=as.character(HaulNumber), y=as.integer(CatchSpeciesCategoryWeight), fill=Species))+
                 geom_col()+
                 xlab("Haul Number") + ylab("Catch (kg)")+
                 theme_bw()
             } else{
             
                           
               if(input$Plot_abiotic=="Haul distribution"){
                 
                 input$newplot
                 map('worldHires',
                     col = "green",
                     fill = TRUE,
                     xlim = c(min(as.numeric(haul_data$HaulStartLongitude))-3,
                              max(as.numeric(haul_data$HaulStartLongitude))+3),
                     ylim = c(min(as.numeric(haul_data$HaulStartLatitude))-0.5,
                              max(as.numeric(haul_data$HaulStartLatitude))+0.5))
                 box()
                 axis(side = 2, las = 2)
                 points(haul_data$HaulStartLongitude, haul_data$HaulStartLatitude, col = as.factor(haul_data$HaulValidity ), pch=19)
                 legend("topright", c("valid", "invalid"), pch=19, col=1:length(haul_data$HaulValidity))
             
             }}}
             }
           }}}}}
               

       }

   })
   
   output$data <- renderTable({
     req(input$file2)
     df2 <- read.csv(input$file2$datapath,
                     header = F,
                     sep = ",",
                     col.names = paste0("V", seq_len(29)),
                     fill=TRUE,
                     quote = "")
     acoustic_data <- df2[df2$V1=="Data",]
     col_names <- as.character(acoustic_data[1,])
     colnames(acoustic_data) <- c(col_names)
     acoustic_data <- acoustic_data[-1,]
     
     cruiseTracks <- acoustic_data
     
     cruiseTracks <- as.data.frame(cbind(as.numeric(as.character(cruiseTracks$LogLatitude)),
                                         as.numeric(as.character(cruiseTracks$LogLongitude)),
                                         as.numeric(as.character(cruiseTracks$LogDistance))))
     
     colnames(cruiseTracks) <- c('LogLatitude','LogLongitude','LogDistance')
     
     agg <- aggregate(cruiseTracks,
                      by = list(cruiseTracks$LogDistance),
                      FUN = max)
     saVec <- as.data.frame(as.numeric(as.character(acoustic_data$DataValue)))
     
     colnames(saVec) <- c('SA')
     
     agg_1 <- aggregate(saVec,
                        by = list(cruiseTracks$LogDistance),
                        FUN = sum)
     
     
     
     SA.df <- as.data.frame(cbind(agg_1$SA,agg$LogLatitude,agg$LogLongitude))
     colnames(SA.df) <- c('SA','LogLatitude','LogLongitude')
     #nearPoints(SA.df, xvar="LogLongitude",yvar="LogLatitude", input$plot_click)
     brushedPoints(SA.df, xvar="LogLongitude",yvar="LogLatitude", input$plot_brush)
   })
   
}
# Create Shiny app ----
shinyApp(ui, server)