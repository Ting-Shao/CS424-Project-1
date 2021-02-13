# sample R + Shiny example for CS 424 Spring 2020 UIC - Andy Johnson
# www.evl.uic.edu/aej/424

# This is a sample dashboard making use of the evl room temperature data and displaying
# it in a variery of ways to show off some of the different capabilities of R and Shiny
# and the Shiny Dashboard.

#libraries to include

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(usmap)

#read csv and preprocess data

ags <- read.csv(file = "annual_generation_state.csv")
generation <- as.numeric(gsub(",", "", ags$GENERATION..Megawatthours.))
ags$generation<-generation
ags$GENERATION..Megawatthours. <- NULL
ags <- subset(ags, STATE != "  ")
ags$STATE = toupper(ags$STATE)

ags$STATE <- as.factor(ags$STATE)
ags$TYPE.OF.PRODUCER <- as.factor(ags$TYPE.OF.PRODUCER)
ags$ENERGY.SOURCE  <- as.factor(ags$ENERGY.SOURCE)

ags <- subset(ags, generation>=0)
ags <- subset(ags, (ENERGY.SOURCE!="Other")&(ENERGY.SOURCE!="Other Gases"))
ags <- subset(ags, (ENERGY.SOURCE!="Other")&(ENERGY.SOURCE!="Other Gases")&(ENERGY.SOURCE!="Other Biomass")&(ENERGY.SOURCE!="Pumped Storage"))

levels(ags$ENERGY.SOURCE) <- c("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Other", "Other Biomass", "Other Gases", "Petroleum", "Pumped Storage", "Solar", "Total", "Wind", "Wood"   )
TEPI <- subset(ags, TYPE.OF.PRODUCER=="Total Electric Power Industry")

TEPI_total <- subset(TEPI, STATE=="US-TOTAL")
TEPI_total_energy <- subset(TEPI_total, ENERGY.SOURCE!="Total")
TEPI_total_total <- subset(TEPI_total, ENERGY.SOURCE=="Total")
TEPI_total_energy$total <- TEPI_total_total$generation[match(TEPI_total_energy$YEAR, TEPI_total_total$YEAR)]

c_es<-c("Coal"= "Coal",
        "Geothermal"= "Geothermal",
        "Hydro"= "Hydro",
        "Natural Gas"= "NaturalGas",
        "Nuclear"= "Nuclear",
        "Petroleum"= "Petroleum",
        "Solar"= "Solar",
        "Wind"= "Wind",
        "Wood"= "Wood")
c_state<-setNames(state.abb, state.name)[state.name]
c_state<-append(c_state, c("Washington DC"="DC"))
c_state<-append(c("US-TOTAL"="US-TOTAL"),c_state)

years<-c(2005:2020)

interesting_comparisons<-c("Default","A","B","C","D","E")

color_fill<-c("Coal"= "darkblue",
              "Geothermal"= "red4",
              "Hydro"= "blue",
              "Natural Gas"= "yellow4",
              "Nuclear"= "maroon",
              "Petroleum"= "purple",
              "Solar"= "pink",
              "Wind"= "springgreen",
              "Wood"= "brown")
#================================================================================
# Create the shiny dashboard
ui <- dashboardPage(
    dashboardHeader(title = "CS424 Spring 2021 Project1"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     sidebarMenu(
                         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("Interesting comparisons",  icon = icon("file-text-o"),
                                  menuSubItem("Comparison1", tabName = "com1", icon = icon("angle-right")),
                                  menuSubItem("Comparison2", tabName = "com2", icon = icon("angle-right")),
                                  menuSubItem("Comparison3", tabName = "com3", icon = icon("angle-right")),
                                  menuSubItem("Comparison4", tabName = "com4", icon = icon("angle-right")),
                                  menuSubItem("Comparison5", tabName = "com5", icon = icon("angle-right"))
                         ),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("About page", tabName = "about", icon = icon("question"))
                     )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    fluidRow(
                        column(6,
                               fluidRow(
                                   column(4, 
                                          selectInput('state1', 'State1', c_state, selected = "IL")
                                          ),
                                   column(4, 
                                          selectInput('es1', 'Energy Source1', c_es)
                                          ),
                                   column(4,
                                          selectInput("year1", "Year1", years, selected = 2019)
                                          )
                               ),
                             
                               
                               fluidRow(
                                   box(
                                       title = "Stacked bar chart", solidHeader = TRUE, status = "primary", width = 12, 
                                       h3("The amount of each energy source"),
                                       plotOutput("sbc_es", height = 200),
                                       h3("The percent of the total production for each energy source"),
                                       plotOutput("sbc_pes", height = 200)
                                   )  
                               ),
                               
                               fluidRow(
                                   box(
                                       title = "Line chart", 
                                       solidHeader = TRUE, 
                                       status = "primary", 
                                       width = 12,
                                       
                                       checkboxInput("All", "All", TRUE),
                                       checkboxGroupInput("variable", "Variables to show:",
                                                          c_es,
                                                          selected = c("Coal",
                                                                       "Geothermal",
                                                                       "Hydro",
                                                                       "NaturalGas",
                                                                       "Nuclear",
                                                                       "Petroleum",
                                                                       "Solar",
                                                                       "Wind",
                                                                       "Wood"),
                                                          inline = TRUE),
                                       
                                       h3("The amount of each energy source"),
                                       plotOutput("ln_es", height = 200),
                                       h3("The percent of the total production for each energy source"),
                                       plotOutput("ln_pes", height = 200)
                                   )
                               ),
                               
                               fluidRow(
                                   box(title = "Table of raw numbers", solidHeader = TRUE, status = "primary", 
                                       width = 12, 
                                       h3("The amount of each energy source"),
                                       dataTableOutput("tr_es", height = 200),
                                       hr(),
                                       hr(),
                                       hr(),
                                       hr(),
                                       hr(),
                                       h3("The percent of the total production for each energy source"),
                                       dataTableOutput("tr_pes", height = 200)
                                   )
                               ),
                               
                               fluidRow(
                                   box(title = "Geographic comparisons", solidHeader = TRUE, status = "primary", width = 12, 
                                       "the percentage of that energy source",
                                       plotOutput("geo2", height = 200),
                                       "the total amount of that energy source",
                                       plotOutput("geo1", height = 200),
                                   )
                               )
                        ),
                        column(6,
                               fluidRow(
                                   column(4, 
                                          selectInput('state2', 'State2', c_state, selected = "US-TOTAL")),
                                   column(4, 
                                          selectInput('es2', 'Energy Source2', c_es)),
                                   column(4,
                                          selectInput("year2", "Year2", years, selected = 2019))
                               ),
                               fluidRow(
                                   box(
                                       title = "Stacked bar chart", solidHeader = TRUE, status = "primary", width = 12, 
                                       h3("The amount of each energy source"),
                                       plotOutput("sbc_es2", height = 200),
                                       h3("The percent of the total production for each energy source"),
                                       plotOutput("sbc_pes2", height = 200)
                                   )  
                               ),
                               
                               fluidRow(
                                   box(
                                       title = "Line chart", 
                                       solidHeader = TRUE, 
                                       status = "primary", 
                                       width = 12,
                                       
                                       checkboxInput("All2", "All", TRUE),
                                       checkboxGroupInput("variable2", "Variables to show:",
                                                          c_es,
                                                          selected = c("Coal",
                                                                       "Geothermal",
                                                                       "Hydro",
                                                                       "NaturalGas",
                                                                       "Nuclear",
                                                                       "Petroleum",
                                                                       "Solar",
                                                                       "Wind",
                                                                       "Wood"),
                                                          inline = TRUE),
                                       
                                       h3("The amount of each energy source"),
                                       plotOutput("ln_es2", height = 200),
                                       h3("The percent of the total production for each energy source"),
                                       plotOutput("ln_pes2", height = 200)
                                   )
                               ),
                               
                               fluidRow(
                                   box(title = "Table of raw numbers", solidHeader = TRUE, status = "primary", 
                                       width = 12, 
                                       h3("The amount of each energy source"),
                                       dataTableOutput("tr_es2", height = 200),
                                       hr(),
                                       hr(),
                                       hr(),
                                       hr(),
                                       hr(),
                                       h3("The percent of the total production for each energy source"),
                                       dataTableOutput("tr_pes2", height = 200)
                                   )
                               ),
                               
                               fluidRow(
                                   box(title = "Geographic comparisons", solidHeader = TRUE, status = "primary", width = 12, 
                                       "the percentage of that energy source",
                                       plotOutput("geo4", height = 200),
                                       "the total amount of that energy source",
                                       plotOutput("geo3", height = 200),
                                   )
                               )
                               
                               
                               
                               )
                    )
            ),
            tabItem(tabName = "com2",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title= "Reactive Log Visualizer",
                         helpText("Graphical representation of the reactive expressions called in the app. It is a minimal example with only the color and horizon setting as adjustable value. To build the graphic please use the mouse and drag the blue bar to the right."),
                         tags$iframe(src = './reactlog_mini.html', 
                                     width = '100%', height = '800px',
                                     frameborder = 0, scrolling = 'auto'
                         )
                    )
                    
            ),
            tabItem(tabName = "com3",
                    fluidPage(
                        tags$iframe(src = './about.html', 
                                    width = '100%', height = '800px',
                                    frameborder = 0, scrolling = 'auto'
                        )
                    )
                    
            ),
            tabItem(tabName = "about",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title= "About page",
                         #helpText("Graphical representation of the reactive expressions called in the app. It is a minimal example with only the color and horizon setting as adjustable value. To build the graphic please use the mouse and drag the blue bar to the right."),
                         mainPanel(
                             h1("Data reference"),
                             h3("https://www.eia.gov/electricity/data/state/ "),
                             h1("App developer"),
                             h3("Ting-Shao, Lee"),
                             h3("This application is part of my CS424 project 1 at the University of Illinois at Chicago, Spring 2021."),
                             
                         )
                    )
                    
            )
            
        )      
        
    ))




#================================================================================
server <- function(input, output, session) {
    
    # increase the default font size
    theme_set(theme_grey(base_size = 18)) 
    
    # calculate the values one time and re-use them in multiple charts to speed things up
    
    TEPI_state1 <- reactive({subset(TEPI, STATE==input$state1)})
    TEPI_state2 <- reactive({subset(TEPI, STATE==input$state2)})
    
    TEPI_state1Reactive <- reactive({subset(TEPI,STATE==input$state1&ENERGY.SOURCE!="Total")})
    TEPI_state2Reactive <- reactive({subset(TEPI,STATE==input$state2&ENERGY.SOURCE!="Total")})
    
    justOneYearReactive <- reactive({subset(allData, year(allData$newDate) == input$Year)})
    newNoonsReactive <- reactive({subset(allData, year(allData$newDate) == input$Year & Hour == 12)})
    oneRoomNoonReactive <- reactive({subset(allData$input$Room, year(allData$newDate) == input$Year & Hour == 12)})
    
    # in 2017 it was y=justOneYear["Hour"] - needed to make a change for 2018
    
    
    

#    Stacked bar chart (dependent to state)
    output$sbc_es <- renderPlot({
        
        ggplot(TEPI_state1Reactive(), aes(fill=ENERGY.SOURCE, y=generation, x=YEAR)) + geom_bar(position="stack", stat="identity")+scale_fill_manual("legend", values = color_fill)+
            theme_minimal()
    })
    
    output$sbc_pes <- renderPlot({
        ggplot(TEPI_state1Reactive(), aes(fill=ENERGY.SOURCE, y=generation, x=YEAR)) + geom_bar(position="fill", stat="identity")+scale_fill_manual("legend", values = color_fill)+
            theme_minimal()
    })
    
    output$sbc_es2 <- renderPlot({
        
        ggplot(TEPI_state2Reactive(), aes(fill=ENERGY.SOURCE, y=generation, x=YEAR)) + geom_bar(position="stack", stat="identity")+scale_fill_manual("legend", values = color_fill)+
            theme_minimal()
    })
    
    output$sbc_pes2 <- renderPlot({
        ggplot(TEPI_state2Reactive(), aes(fill=ENERGY.SOURCE, y=generation, x=YEAR)) + geom_bar(position="fill", stat="identity")+scale_fill_manual("legend", values = color_fill)+
            theme_minimal()
    })
    
#    Table of raw numbers (dependent to state)
    output$tr_es <- DT::renderDataTable(
        DT::datatable({ 
            
            TEPI_state <- TEPI_state1()
            
            TEPI_state_total <- subset(TEPI_state, ENERGY.SOURCE=="Total")
            TEPI_state <- subset(TEPI_state, ENERGY.SOURCE!="Total")
            TEPI_state$total <- TEPI_state_total$generation[match(TEPI_state$YEAR, TEPI_state_total$YEAR)]
            
            TEPI_state <- reshape(TEPI_state, timevar = "ENERGY.SOURCE", idvar = c("YEAR","STATE","TYPE.OF.PRODUCER","total"), direction = "wide")
            colnames(TEPI_state)<-gsub("generation.","",colnames(TEPI_state))
            TEPI_state$STATE <- NULL
            TEPI_state$TYPE.OF.PRODUCER <- NULL
            TEPI_state[is.na(TEPI_state)] <- 0
            for (col in c("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar",  "Wind", "Wood"   ))
            {
                if (!(col %in% colnames(TEPI_state)))
                {TEPI_state[col] <- 0  }
            }
            TEPI_state$total <- NULL
            
            TEPI_state
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc')), scrollX = T
        ), rownames = FALSE 
        )
    )
    
    output$tr_pes <- DT::renderDataTable(
        DT::datatable({ 
            TEPI_state <- TEPI_state1()
            
            TEPI_state_total <- subset(TEPI_state, ENERGY.SOURCE=="Total")
            TEPI_state <- subset(TEPI_state, ENERGY.SOURCE!="Total")
            TEPI_state$total <- TEPI_state_total$generation[match(TEPI_state$YEAR, TEPI_state_total$YEAR)]
            
            w <- reshape(TEPI_state, timevar = "ENERGY.SOURCE", idvar = c("YEAR","STATE","TYPE.OF.PRODUCER","total"), direction = "wide")
            colnames(w)<-gsub("generation.","",colnames(w))
            w$STATE <- NULL
            w$TYPE.OF.PRODUCER <- NULL
            w[is.na(w)] <- 0
            
            for (col in c("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar",  "Wind", "Wood"   ))
            {
                if (!(col %in% colnames(w)))
                {w[col] <- 0  }
            }
            
            p <- w
            w$total <- NULL
            
            for (i in 3:11)
            {
                p[,i]<-format(round(p[,i]/p$total, 3), nsmall = 3)
            } 
            p$total <- NULL
            
            p
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc')), scrollX = T
        ), rownames = FALSE 
        )
    )
    
    
    output$tr_es2 <- DT::renderDataTable(
        DT::datatable({ 
            
            TEPI_state <- TEPI_state2()
            
            TEPI_state_total <- subset(TEPI_state, ENERGY.SOURCE=="Total")
            TEPI_state <- subset(TEPI_state, ENERGY.SOURCE!="Total")
            TEPI_state$total <- TEPI_state_total$generation[match(TEPI_state$YEAR, TEPI_state_total$YEAR)]
            
            TEPI_state <- reshape(TEPI_state, timevar = "ENERGY.SOURCE", idvar = c("YEAR","STATE","TYPE.OF.PRODUCER","total"), direction = "wide")
            colnames(TEPI_state)<-gsub("generation.","",colnames(TEPI_state))
            TEPI_state$STATE <- NULL
            TEPI_state$TYPE.OF.PRODUCER <- NULL
            TEPI_state[is.na(TEPI_state)] <- 0
            for (col in c("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar",  "Wind", "Wood"   ))
            {
                if (!(col %in% colnames(TEPI_state)))
                {TEPI_state[col] <- 0  }
            }
            TEPI_state$total <- NULL
            
            TEPI_state
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc')), scrollX = T
        ), rownames = FALSE 
        )
    )
    output$tr_pes2 <- DT::renderDataTable(
        DT::datatable({ 
            TEPI_state <- TEPI_state2()
            
            TEPI_state_total <- subset(TEPI_state, ENERGY.SOURCE=="Total")
            TEPI_state <- subset(TEPI_state, ENERGY.SOURCE!="Total")
            TEPI_state$total <- TEPI_state_total$generation[match(TEPI_state$YEAR, TEPI_state_total$YEAR)]
            
            w <- reshape(TEPI_state, timevar = "ENERGY.SOURCE", idvar = c("YEAR","STATE","TYPE.OF.PRODUCER","total"), direction = "wide")
            colnames(w)<-gsub("generation.","",colnames(w))
            w$STATE <- NULL
            w$TYPE.OF.PRODUCER <- NULL
            w[is.na(w)] <- 0
            
            for (col in c("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar",  "Wind", "Wood"   ))
            {
                if (!(col %in% colnames(w)))
                {w[col] <- 0  }
            }
            
            p <- w
            w$total <- NULL
            
            for (i in 3:11)
            {
                p[,i]<-format(round(p[,i]/p$total, 3), nsmall = 3)
            } 
            p$total <- NULL
            
            p
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc')), scrollX = T
        ), rownames = FALSE 
        )
    )
    
    
    
    
    
    
    
    
    
    
#    Line chart 
    observe({
        if (input$All){
            updateCheckboxGroupInput(session, "variable", "Variables to show:",
                                     c("Coal",
                                       "Geothermal",
                                       "Hydro",
                                       "NaturalGas",
                                       "Nuclear",
                                       "Petroleum",
                                       "Solar",
                                       "Wind",
                                       "Wood"),
                                     selected = c("Coal",
                                                  "Geothermal",
                                                  "Hydro",
                                                  "NaturalGas",
                                                  "Nuclear",
                                                  "Petroleum",
                                                  "Solar",
                                                  "Wind",
                                                  "Wood"),
                                     inline = TRUE
            )
        }
        if (input$All2){
            updateCheckboxGroupInput(session, "variable2", "Variables to show:",
                                     c("Coal",
                                       "Geothermal",
                                       "Hydro",
                                       "NaturalGas",
                                       "Nuclear",
                                       "Petroleum",
                                       "Solar",
                                       "Wind",
                                       "Wood"),
                                     selected = c("Coal",
                                                  "Geothermal",
                                                  "Hydro",
                                                  "NaturalGas",
                                                  "Nuclear",
                                                  "Petroleum",
                                                  "Solar",
                                                  "Wind",
                                                  "Wood"),
                                     inline = TRUE
            )
        }
    })
    
    observe({
        # TRUE if input$controller is odd, FALSE if even.
        print(length(input$variable))
        if(length(input$variable)<9){
            updateCheckboxInput(session, "All", "All", FALSE)
        }
        else if(length(input$variable)==9){
            updateCheckboxInput(session, "All", "All", TRUE)
        }
        
        if(length(input$variable2)<9){
            updateCheckboxInput(session, "All2", "All", FALSE)
        }
        else if(length(input$variable2)==9){
            updateCheckboxInput(session, "All2", "All", TRUE)
        }
    })
    
    
    
    output$ln_es <- renderPlot({
        TEPI_state <- subset(TEPI, STATE==input$state1)
        TEPI_state_energy <- subset(TEPI_state, ENERGY.SOURCE!="Total")
        
        ss<-subset(TEPI_state_energy, ENERGY.SOURCE %in% input$variable)
        ggplot(ss, aes(x = YEAR, y = generation, group = ENERGY.SOURCE)) + geom_line(aes(color = ENERGY.SOURCE)) + scale_color_manual(values=color_fill)
        
    })
    
    
    output$ln_pes <- renderPlot({
        
        
        TEPI_state <- subset(TEPI, STATE==input$state1)
        TEPI_state_energy <- subset(TEPI_state, ENERGY.SOURCE!="Total")
        TEPI_state_total <- subset(TEPI_state, ENERGY.SOURCE=="Total")
        TEPI_state_energy$total <- TEPI_state_total$generation[match(TEPI_state_energy$YEAR, TEPI_state_total$YEAR)]
        
        #print(input$variable)
        ss<-subset(TEPI_state_energy, ENERGY.SOURCE %in% input$variable)
        ggplot(ss, aes(x = YEAR, y = generation/total, group = ENERGY.SOURCE)) + geom_line(aes(color = ENERGY.SOURCE)) + scale_color_manual(values=color_fill)
    })
    
    output$ln_es2 <- renderPlot({
        TEPI_state <- subset(TEPI, STATE==input$state2)
        TEPI_state_energy <- subset(TEPI_state, ENERGY.SOURCE!="Total")
        
        ss<-subset(TEPI_state_energy, ENERGY.SOURCE %in% input$variable2)
        ggplot(ss, aes(x = YEAR, y = generation, group = ENERGY.SOURCE)) + geom_line(aes(color = ENERGY.SOURCE)) + scale_color_manual(values=color_fill)
        
    })
    
    
    output$ln_pes2 <- renderPlot({
        
        
        TEPI_state <- subset(TEPI, STATE==input$state2)
        TEPI_state_energy <- subset(TEPI_state, ENERGY.SOURCE!="Total")
        TEPI_state_total <- subset(TEPI_state, ENERGY.SOURCE=="Total")
        TEPI_state_energy$total <- TEPI_state_total$generation[match(TEPI_state_energy$YEAR, TEPI_state_total$YEAR)]
        
        #print(input$variable)
        ss<-subset(TEPI_state_energy, ENERGY.SOURCE %in% input$variable2)
        ggplot(ss, aes(x = YEAR, y = generation/total, group = ENERGY.SOURCE)) + geom_line(aes(color = ENERGY.SOURCE)) + scale_color_manual(values=color_fill)
    })
    
    
    
#    Geographic comparisons
    output$geo1 <- renderPlot({
        TEPI_state_energy <- subset(TEPI, ENERGY.SOURCE==input$es1)
        TEPI_state_energy_year <- subset(TEPI_state_energy, YEAR==input$year1)
        
        names(TEPI_state_energy_year )[names(TEPI_state_energy_year ) == 'STATE'] <- 'state'
        
        
        plot_usmap(data = TEPI_state_energy_year, values = "generation", color = "blue") + 
            scale_fill_continuous(low = "white", high = "blue", name = input$es1, label = scales::comma) + 
            labs(title = input$es1, subtitle = input$year1) +
            theme(legend.position = "right")
    })
    
    output$geo2 <- renderPlot({
        data <- subset(TEPI, (ENERGY.SOURCE==input$es1|ENERGY.SOURCE=="Total")&YEAR==input$year1)
        
        
        data <- subset(data,STATE!="US-TOTAL")
        data <- reshape(data, timevar = "ENERGY.SOURCE", idvar = c("YEAR","STATE","TYPE.OF.PRODUCER"), direction = "wide")
        colnames(data)<-gsub("generation.","",colnames(data))
        names(data )[names(data) == 'STATE'] <- 'state'
        
        
        ##!
        names(data )[names(data) == input$es1] <- "generation"
        
        
        data$percentage<-(data$generation/data$Total)*100
        
        
        print(data)
        print(str(data))
        
        plot_usmap(data = data, values = "percentage", color = "blue") + 
            scale_fill_continuous(low = "white", high = "blue", name = input$es1, label = scales::comma) + 
            labs(title = input$es1, subtitle = input$year1) +
            theme(legend.position = "right")
    })
    
    
    output$geo3 <- renderPlot({
        TEPI_state_energy <- subset(TEPI, ENERGY.SOURCE==input$es2)
        TEPI_state_energy_year <- subset(TEPI_state_energy, YEAR==input$year2)
        
        names(TEPI_state_energy_year )[names(TEPI_state_energy_year ) == 'STATE'] <- 'state'
        
        
        plot_usmap(data = TEPI_state_energy_year, values = "generation", color = "blue") + 
            scale_fill_continuous(low = "white", high = "blue", name = input$es2, label = scales::comma) + 
            labs(title = input$es2, subtitle = input$year2) +
            theme(legend.position = "right")
    })
    
    output$geo4 <- renderPlot({
        data <- subset(TEPI, (ENERGY.SOURCE==input$es2|ENERGY.SOURCE=="Total")&YEAR==input$year2)
        
        
        data <- subset(data,STATE!="US-TOTAL")
        data <- reshape(data, timevar = "ENERGY.SOURCE", idvar = c("YEAR","STATE","TYPE.OF.PRODUCER"), direction = "wide")
        colnames(data)<-gsub("generation.","",colnames(data))
        names(data )[names(data) == 'STATE'] <- 'state'
        
        
        ##!
        names(data )[names(data) == input$es2] <- "generation"
        
        
        data$percentage<-(data$generation/data$Total)*100
        
        
        print(data)
        print(str(data))
        
        plot_usmap(data = data, values = "percentage", color = "blue") + 
            scale_fill_continuous(low = "white", high = "blue", name = input$es2, label = scales::comma) + 
            labs(title = input$es2, subtitle = input$year2) +
            theme(legend.position = "right")
    })
    
    
    
    
    
    
    
}

shinyApp(ui = ui, server = server)
