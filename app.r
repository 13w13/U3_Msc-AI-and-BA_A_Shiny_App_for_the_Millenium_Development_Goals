###RMD Part
#library
library(shiny)
library(magrittr)
library(data.table)
library(ggplot2)
library(readr)
library(httr)
library(readxl)
library(stringr)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(scales)

urlfile="https://drive.google.com/uc?export=download&id=1mbWQtW83faGtfnwnjlopJblGavaCm7_T"
goalD<-urlfile%>% url %>% read_csv %>% as.data.table 
cols<-c("Index","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","Country_Name","Series_Code","Series_Name")
colnames(goalD)<-cols
Dates = (colnames(goalD[,!(c(1,38,39,40))]))
goalD[,.N,(goalD$Series_Name)]
goalD[,.N,(goalD$Series_Code)]
goalD[,.N,by = list(Series_Code,Series_Name)] %>% .[order(-N)]
metrics <- as.data.frame(str_extract_all(goalD$Series_Name, "\\([^()]+\\)", simplify = TRUE))
unique(metrics[,2])
metrics$V1 <- apply(metrics, 1, function(x) paste(str_trim(x[!is.na(x)]), collapse=""))
goalD$Metrics<-metrics$V1
urlfile_WDI_break="https://drive.google.com/uc?export=download&id=1eu92j-5ozDeMm-Tu1Wz9sSFKAnrz8JNe"
GET(urlfile_WDI_break, write_disk(tf <- tempfile(fileext = ".xls")))
WDI_break <- read_xls(tf, sheet=2) %>% as.data.table 
colnames(WDI_break) =  colnames(WDI_break) %>% gsub(" ","_",.) 
goalD <- merge(goalD,WDI_break, by.x = 'Series_Code', by.y = 'Series_Code', all = FALSE)
goalD<-goalD[,!42]
colnames(goalD[,40])="Series_Name"
urlfile_Country_break="https://drive.google.com/uc?export=download&id=1mrykesjoTt8SBiHWnm7xDPYS-Siu1nD4"
GET(urlfile_Country_break, write_disk(tf <- tempfile(fileext = ".xls")))
Country_break <- read_xls(tf, range="C5:I224", sheet=1) %>% as.data.table 
Code_break<-Country_break[,c(1,2)] %>% setDT
Country_break<-Country_break[!1,!c(2,3)]
colnames(Country_break) =  colnames(Country_break) %>% gsub(" ","_",.) 
goalD<-merge(goalD,Country_break, by.x = 'Country_Name', by.y = 'Economy', all.x=TRUE)
mean_region_goalD=goalD[, lapply(.SD, mean, na.rm=TRUE), by=list(Region,Series_Name.x),.SDcols=c("1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007") ]
sum(goalD[Region=="South Asia"&Series_Name.x=="GDP (current US$)"][,4],na.rm=TRUE)/6
mean_region_goalD[Region=="South Asia"&Series_Name.x=="GDP (current US$)"][,3]
mean_income_goalD=goalD[, lapply(.SD, mean, na.rm=TRUE), by=list(Income_group,Series_Name.x),.SDcols=c("1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007") ]
sum(goalD[Income_group=="Low income"&Series_Name.x=="GDP (current US$)"][,4],na.rm=TRUE)/20
mean_income_goalD[Income_group=="Low income"&Series_Name.x=="GDP (current US$)"][,3]
mean_lending_goalD=goalD[, lapply(.SD, mean, na.rm=TRUE), by=list(Lending_category,Series_Name.x),.SDcols=c("1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007") ]
mean_other_goalD=goalD[, lapply(.SD, mean, na.rm=TRUE), by=list(Other,Series_Name.x),.SDcols=c("1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007") ]
flags <- read.csv("https://raw.githubusercontent.com/13w13/A_Shiny_App_for_the_Millenium_Development_Goals/main/img/Country_flag.csv")
cols2<-c("Country_Name","Images.File.Name","ImageURL")
colnames(flags)=cols2        
PlotDT_Flags <- merge(goalD, flags,by.x = 'Country_Name', by.y = 'Country_Name', all= FALSE)
urlfile_WDI_metadata="https://drive.google.com/uc?export=download&id=1wHNeRYOh4ajzPOea3sg-hlk93IngChk0"
GET(urlfile_WDI_metadata,  mode="wb",write_disk(tf <- tempfile(fileext = ".xlsx")))
WDI_metadata <- read_xlsx(tf, range="C1:N1438", sheet=3) %>% as.data.table 
head(WDI_metadata)
PlotDT = data.table::melt(goalD, id.vars = c(1,2,3,40,41,42,43,44,45,46,47,48,49), 
                          measure.vars = Dates, 
                          variable.name = "Date",
                          value.name = "Value")
PlotDT$Date = PlotDT$Date %>%  as.Date(format = "%Y")
PlotDT_Region =  data.table::melt(mean_region_goalD, id.vars = c(1,2), 
                                  measure.vars = Dates, 
                                  variable.name = "Date",
                                  value.name = "Value")
PlotDT_Region$Date = PlotDT_Region$Date%>%  as.Date(format = "%Y")
PlotDT_Income_group=  data.table::melt(mean_income_goalD, id.vars = c(1,2), 
                                       measure.vars = Dates, 
                                       variable.name = "Date",
                                       value.name = "Value")
PlotDT_Income_group$Date = PlotDT_Income_group$Date%>%  as.Date(format = "%Y")
PlotDT_Lending_category=  data.table::melt(mean_lending_goalD, id.vars = c(1,2), 
                                           measure.vars = Dates, 
                                           variable.name = "Date",
                                           value.name = "Value")
PlotDT_Lending_category$Date = PlotDT_Lending_category$Date%>%  as.Date(format = "%Y")
PlotDT_Other=  data.table::melt(mean_other_goalD, id.vars = c(1,2), 
                                measure.vars = Dates, 
                                variable.name = "Date",
                                value.name = "Value")
PlotDT_Other$Date = PlotDT_Other$Date%>%  as.Date(format = "%Y")
#####
  
##############
### WORLD DEVELOPMENT GOALS APP
###############

#global_scope - initialisation 
selected_country <- unique(PlotDT_Flags$Country_Name) #Country list
selected_topic <- unique(goalD$Topic) #Topic list
selected_subtopic_1 <- list() #SubTopic1 list
selected_subtopic_2 <- list() #SubTopic2 list
selected_subtopic_3 <- list() #SubTopic3 list
selected_indicator <- list() #Indicator list
selected_aggregation<-unique(str_replace_all(colnames(Country_break[,2:5]), "_", " ")) #Aggregation List (Region, Low Income, Lending Category, Other)
Plot_choices<-PlotDT_Region  #Aggregation List initialization
selected_year<-colnames(goalD[, c(4:39)]) # Year List
selected_flags <- unique(PlotDT_Flags$ImageURL) #Flag List


# Define UI for WDG application 
ui <- fluidPage(theme = "bootstrap.css", #to make the app responsive
                useShinyjs(), #for showing/hiding panel 
  #CSS
  tags$head(
    tags$style(HTML("
    
      .shiny-output-error { 
        visibility: hidden; 
      }
      
      .shiny-output-error:before { 
        visibility: hidden; 
      }
      
      h1 {
        font-family: 'Arial black', cursive;
        font-weight: 650;
        line-height: 1.1;
        color: #0F056B;
      }
      
      h2 {
        font-family: 'Arial', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #1B019B;
      }
      
      h3 {
        font-family: 'Arial narrow', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: grey;
      }
      
      body {
        background-color: #fff;
      }
      
      .selectize-input {
        min-height: 20px;
        border: 0;
        padding: 4px;
        font-family: 'Arial', cursive;
      }
      
       .fa {
        color:#0131B4;
       }
      
       .fa {
        color:#0131B4;
      }
      
      .fas {
        color:#0131B4;
      }
      
      #text {
        font-family: 'Arial', cursive;
        font-size: 150%;
        color : black; 
        text-align: justify;
        text-justify: inter-word;
      }
      
      #author_names{
        font-family: 'Arial', cursive;
        font-style: italic;
        font-size: 150%;
        color : brown; 
        text-align: justify;
        text-justify: inter-word;
      }
      
    "))
  ),
  
  titlePanel(
    # app title/description
    h1("Millenium Development Goals", align="center"),
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      id="Sidebar",
      
      h4("Here you can find some graphical information
                     about the World Development Goals."),
      h4("Please, select topic and subtopics to find your indicator. 
               you can choose."), 
      br(),
      
      tabsetPanel( 
        tabPanel(h2("Topic & Subtopics", style="color:#464E51"),
                 
                 # inputs
                 selectInput("topic", 
                             h2("Choose a topic", align = "center"),
                             selected_topic),
                 
                 br(), 
                 selectInput("subtopic_1", 
                             h2("Choose a subtopic 1", align = "center"),
                             selected_subtopic_1, 
                             choices = NULL),
                 
                 br(), 
                 selectInput("subtopic_2", 
                             h2("Choose a subtopic 2", align = "center"),
                             selected_subtopic_2, 
                             choices = NULL),
                 
                 br(), 
                 selectInput("subtopic_3", 
                             h2("Choose a subtopic 3", align = "center"),
                             selected_subtopic_3, 
                             choices = NULL),
                 
                 br(), 
                 selectInput("indicator", 
                             h2("Choose a indicator", align = "center"),
                             selected_indicator, 
                             choices = NULL),
                 
                 br(), 
                 ),
        
        
        tabPanel(h2("Graph ajustment", style="color:#464E51"),
                 
                 pickerInput("country",  h2("Choose a country", align = "center"), multiple = F,
                             choices = selected_country,
                             
                             choicesOpt = list(content =  
                                                 mapply(selected_country, selected_flags, FUN = function(country, flagUrl) {
                                                   HTML(paste(
                                                     tags$img(src=flagUrl, width=20, height=15),
                                                     country
                                                   ))
                                                 }
                                                 
                                                 ))),
                 
                 
                 br(), 
                 awesomeRadio(inputId = "aggregation", 
                              label = h2("Choose an aggregation view", align = "center"),
                              selected_aggregation, 
                              "Region",
                              status="warning"),
                 
                 br(), 
                 awesomeRadio(
                   inputId = "y_axis_choice",
                   label = h2("Axis :", align = "center"),  
                   c("linear", "logarithmic"),
                   status = "success"
                 ), 
                 ), 
        
        
        tabPanel(h2("Map tools", style="color:#464E51"),
                 selectInput("year", 
                             h2("Choose a year for the map (Page 3)", align = "center"),
                             selected_year, 
                             "1972"),
                 br(),
                 dateRangeInput("date_choice", 
                                h2("Choose a date range :", align="center"),
                                format = "yyyy",
                                start="1972"),
                 
                 br(),
                 p(strong("Full data is available just below."), style="strong"),
                 br(),
                 a(strong("DATA AVAILABLE HERE"), href="https://drive.google.com/uc?export=download&id=1mbWQtW83faGtfnwnjlopJblGavaCm7_T"),
                 br(),
                 img(src="https://i1.wp.com/www.un.org/sustainabledevelopment/wp-content/uploads/2015/12/english_SDG_17goals_poster_all_languages_with_UN_emblem_1.png?fit=728%2C451&ssl=1", height = 72, width = 72, style="margin-left:80px"),
                 br(),
                 
                 )
      ),
  
    ),
    mainPanel(id ="Main",
              
      #for showing/hiding panel
      materialSwitch(
        inputId = "showpanel",
        label = "Show/Hide sidebar panel",
        value=TRUE, 
        status = "primary",
      ),
      
      #choose graph between plot, barplot & map
      radioGroupButtons(
        inputId = "graph",
        label = h3("Choose a graph :"), 
        choices = c(`<i class='fa fa-line-chart fa-2x'></i>` = "line", `<i class='fa fa-bar-chart fa-2x'></i>` = "bar", 
                    `<i class="fas fa-globe-europe fa-2x"></i>` = "globe"),
        justified = TRUE, 
      ),
      
      #choose between percentage and absolute value for barplot
      uiOutput("percent"),
      
      #ouput for plotting 
      plotlyOutput("displot"),
      
      br(),
      
      #indicator description
      htmlOutput("text"),
      
      br(), 
      
      #author names
      HTML("<p id=author_names> By Alexandra, Julieva, Antoine & Simon (MSc Artificial Intelligence & Business Analytics - TBS)</p> ")

    )
  )
)


#  Define a server for the Shiny app
server <- function(input, output, session) {
  
  #giving information about the indicator you choosed. 
  output$text<-renderUI({
    
    Long_definition<-paste("Definition : ",WDI_metadata[`Indicator Name`==input$indicator][,3])
    Source<-paste("Source : ", WDI_metadata[`Indicator Name`==input$indicator][,12])
    Limitation<-paste("Limitation and exceptions : ",WDI_metadata[`Indicator Name`==input$indicator][,9])
    
    HTML(paste(Long_definition,Source,Limitation,sep='<p/>'))
  })
  
  
  #observeEvent for showing/hidding sidebar panel
  observeEvent(input$showpanel, {
    
    if(input$showpanel == TRUE) {
      removeCssClass("Main", "col-sm-12")
      addCssClass("Main", "col-sm-8")
      
      shinyjs::show(id = "Sidebar")
      shinyjs::enable(id = "Sidebar")
      
    }
    else {
      removeCssClass("Main", "col-sm-8")
      addCssClass("Main", "col-sm-12")
      shinyjs::hide(id = "Sidebar")
      
    }
  
  })

  
  #observeEvent the topic value and choose in consequence the good subtopic
  observeEvent(input$topic, {
    #resetting the indicator
      updateSelectInput(
        session,
        inputId = "indicator",
        choices = ''
      )
      #
    
      #look if there is something in SubTopic1 (all the topic have at least one SubTopic1)
      choices <- unique(goalD[goalD$Topic == input$topic, list(SubTopic1)])
      
      #change the value of subtopic_1 in function of the value of topic  
      updateSelectInput(
      session,
      inputId = "subtopic_1",
      choices = choices
      )
  })
  
  #observeEvent the subtopic1 value and choose in consequence the good subtopic2
  observeEvent(input$subtopic_1, {
    #look if there is something in SubTopic2
    choices <- unique(goalD[goalD$SubTopic1 == input$subtopic_1, list(SubTopic2)])
    
    not_value_subtopic_2 = as.vector(is.na(choices[1]))
  
    #change the value of subtopic_2 in function of the value of subtopic1 (some SubTopic1 do not have SubTopic2. Be careful).
    #In this case, we need to find the indicator value directly because there is no subtopic2 and, of course, no subtopic3)
    if(not_value_subtopic_2) {
      updateSelectInput(
        session,
        inputId = "subtopic_2",
        choices = ''
      )
      
      updateSelectInput(
        session,
        inputId = "subtopic_3",
        choices = ''
      )
      
      #find indicator list
      updateSelectInput(
        session,
        inputId = "indicator",
        choices = unique(goalD[goalD$SubTopic1 == input$subtopic_1, list(Series_Name.x)])
      )
      #
      
    }
    else {
      updateSelectInput(
        session,
        inputId = "subtopic_2",
        choices = choices
      )
    }
      
  })
  
  #observeEvent the subtopic2 value and choose in consequence the good subtopic3
  observeEvent(input$subtopic_2, {
    if (input$subtopic_2 != '') {
    #look if there is something in SubTopic3
    choices <- unique(goalD[goalD$SubTopic2 == input$subtopic_2, list(SubTopic3)])
    
    not_value_subtopic_3 = as.vector(is.na(choices[1]))
    
    #change the value of subtopic_3 in function of the value of subtopic2 (some SubTopic2 do not have SubTopic3. Be careful).
    #In this case, we need to find the indicator value directly because there is no subtopic3)
    if(not_value_subtopic_3) {
      updateSelectInput(
        session,
        inputId = "subtopic_3",
        choices = ''
      )
      
      #find indicator list
      updateSelectInput(
        session,
        inputId = "indicator",
        choices = unique(goalD[goalD$SubTopic2 == input$subtopic_2, list(Series_Name.x)])
      )
      #
      
    }
    else {
      updateSelectInput(
        session,
        inputId = "subtopic_3",
        choices = choices
      )
    }
    }
    
  })
  
  #find indicator list
  observeEvent(input$subtopic_3, {
    if (input$subtopic_3 != '') {
      #find indicator list
      updateSelectInput(
        session,
        inputId = "indicator",
        choices = unique(goalD[goalD$SubTopic3 == input$subtopic_3, list(Series_Name.x)])
      )
      #
    }
  })
  
  
  #choose between percentage and absolute value in the barplot
  observeEvent(input$graph, {
    
    if(input$graph == "bar") {
      
      output$percent <- renderUI({
        materialSwitch(
          inputId = "percentV2",
          label = "Make the y-axis in percentage",
          status = "primary",
        )
        
      })
    } else {
      output$percent = NULL
    }
  })
  
  
  #plot function
  output$displot <- renderPlotly({
    
    if(input$graph == "line") {
      p <- switch(input$y_axis_choice,"linear" = NULL,"logarithmic"=scale_y_log10()) #choose the scale axis
      
      
      color <- str_replace(input$aggregation, " ", "_")
      
      Plot_choices <- switch(input$aggregation,"Region"=PlotDT_Region,
                             "Income group"=PlotDT_Income_group,
                             "Lending category"=PlotDT_Lending_category, 
                             "Other"=PlotDT_Other)
      
      q<-ggplot() +
        geom_line(data=PlotDT[`Country_Name`==input$country &`Series_Name.x`==input$indicator], 
                  aes(x=Date, y=Value,colour=input$country))+ 
        geom_line(data=Plot_choices[`Series_Name.x`==input$indicator], 
                  aes_string("Date", "Value", colour = color)) + 
        xlab("Dates")+
        ylab(input$indicator)+
        p
      
      ggplotly(q)
      
    } else if (input$graph == "bar" & !is.null(input$percentV2))  {
      
        if(!input$percentV2) {
          p <- switch(input$y_axis_choice,"linear" = NULL,"logarithmic"=scale_y_log10())
          
          color <- str_replace(input$aggregation, " ", "_")
          
          Plot_choices <- switch(input$aggregation,"Region"=PlotDT_Region,
                                 "Income group"=PlotDT_Income_group,
                                 "Lending category"=PlotDT_Lending_category, 
                                 "Other"=PlotDT_Other)
          
          q <- ggplot() + geom_bar(data=PlotDT[`Country_Name`==input$country 
                                               &`Series_Name.x`==input$indicator], 
                                   aes(x=Date,y=Value, colour=input$country), stat="identity")+
            geom_bar(data=Plot_choices[`Series_Name.x`==input$indicator], 
                     aes_string(x="Date",y="Value", colour=color), stat="identity")+
            xlab("Dates")+
            ylab(input$indicator)
          q
          
        }else {
          p <- switch(input$y_axis_choice,"linear" = NULL,"logarithmic"=scale_y_log10())
          
          color <- str_replace(input$aggregation, " ", "_")
          
          Plot_choices <- switch(input$aggregation,"Region"=PlotDT_Region,
                                 "Income group"=PlotDT_Income_group,
                                 "Lending category"=PlotDT_Lending_category, 
                                 "Other"=PlotDT_Other)
          
          q <- ggplot() + geom_bar(width= .9, Position="Fill", data=PlotDT[`Country_Name`==input$country 
                                                                           &`Series_Name.x`==input$indicator],
                                   aes(x=Date,y=Value, colour=input$country), stat="identity", position = position_fill(.5))+ scale_y_continuous(labels = percent)+ 
            geom_bar(width= .9, Position="Fill", data=Plot_choices[`Series_Name.x`==input$indicator], 
                     aes_string(x="Date", y="Value", colour=color), stat="identity", position = position_fill(.5)) + scale_y_continuous(labels = percent)+
            xlab("Dates")+
            ylab(input$indicator)
          
          q
          
        }
      
    } else if (input$graph == "globe") {
      agr_data <- PlotDT[year(Date) == input$year & 
                           Series_Name.x == input$indicator, 
                         list(Country_Name, Series_Name.x, Value)]
      
      agr.map <- merge(agr_data, Code_break,
                       by.x = 'Country_Name', 
                       by.y = 'Economy', all= TRUE)
      
      agr.map <- as.data.table(agr.map)
      
      idx = agr.map[, .I[which(is.na(Value))]]
      
      agr.map[idx, Value := 0]
      
      
      # light grey boundaries
      l <- list(color = toRGB("grey"), width = 0.5)
      
      # specify map projection/options
      g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator')
      )
      
      fig <- plot_geo(agr.map)
      
      fig <- fig %>% add_trace(
        z = ~Value, color = ~Value, colors = 'Blues',
        text = ~Country_Name, locations = ~Code, marker = list(line = l)
      )
      
      fig <- fig %>% colorbar(title = input$indicator, tickprefix = '')
      
      fig <- fig %>% layout(
        title = input$indicator,
        geo = g
      )
      
      fig
      
    }
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
