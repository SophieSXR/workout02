library(shiny)
library(ggplot2)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Simulations for Investment Strategies"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4,
        sliderInput("initial",
                    "Initial Amount",
                    min = 0,
                    max = 100000,
                    value = 1000,
                    step = 500)
     ),
     column(4,
        sliderInput("contrib",
                    "Annual Contribution",
                    min = 0,
                    max = 50000,
                    value = 2000,
                    step = 500)
     ),
     column(4,
        sliderInput("rate",
                    "Return Rate (in %)", 
                    min = 0,
                    max = 20,
                    value = 5,
                    step = 1)
     ),
     column(4,
        sliderInput("growth",
                    "Growth Rate (in %)",
                    min = 0,
                    max = 20,
                    value = 2,
                    step = 1)
     ),
     column(4,
        sliderInput("years",
                    "Number of years",
                    min = 0, 
                    max = 50,
                    value = 20,
                    step = 1)
     ),
     column(4,
        selectInput("facet",
                    "Facet?",
                    c("Yes","No"))
        
    ),
      
 mainPanel(
    h4("Timelines"),  
    plotOutput("Plot"),
    h4("Balances"),
    tableOutput("Balances")     
 )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  data<-reactive({
    
    #functions
    future_value<-function(amount ,rate ,years){
      fv <- amount*(1+rate)^years
      return(fv)
    }
    annuity<-function(contrib, rate, years){
      fva<-contrib*((1+rate)^years-1)/rate
      return(fva)
    }
    growing_annuity<-function(contrib, rate, growth, years){
      fvga<-contrib*((1+rate)^years-(1+growth)^years)/(rate-growth)
      return(fvga)
    }
    
    #dataframe
    no_contrib<-vector("numeric", input$years+1)
    fixed_contrib<-vector("numeric", input$years+1)
    growing_contrib<-vector("numeric", input$years+1)
    no_contrib[1]<-input$initial
    fixed_contrib[1]<-input$initial
    growing_contrib[1]<-input$initial
    year<-c(0: input$years)
    for (i in 1:input$years+1){
      no_contrib[i]<-future_value(amount = input$initial, rate = input$rate/100, years = i-1)
      fixed_contrib[i]<-future_value(amount = input$initial, rate = input$rate/100, years = i-1)+annuity(contrib = input$contrib, rate = input$rate/100, years = i-1)
      growing_contrib[i]<-growing_annuity(contrib = input$contrib, rate = input$rate/100, growth = input$growth/100, years = i-1)+future_value(amount = input$initial, rate = input$rate/100, years = i-1)
    }
    
    modalities<-data.frame(year,no_contrib, fixed_contrib, growing_contrib)
    
    colnames(modalities)<-c("year","no_contrib","fixed_contrib","growing_contrib")

    return(modalities)

  })
   #Genereating the timeline graph
 output$Plot <- renderPlot({
     if (input$facet=="No")
     {ggplot(data(), aes(x = year))+
       geom_line(aes(y = no_contrib, color = "no_contrib"))+
       geom_line(aes(y = fixed_contrib, color = "fixed_contrib"))+
       geom_line(aes(y = growing_contrib, color = "growing_contrib"))+
       geom_point(aes(y = no_contrib, col = "no_contrib"),size = 1)+
       geom_point(aes(y = fixed_contrib, col = "fixed_contrib"),size = 1)+
       geom_point(aes(y = growing_contrib, col = "growing_contrib"),size = 1)+
       labs(x="Years", y = "The growth of investment", title = "Timeline Graph for Each Saving Modality")}
   else{
     modalities_tidy <- data() %>% gather (key = type, value = balance, "no_contrib","fixed_contrib","growing_contrib")
     ggplot(data = modalities_tidy, aes(x = year))+
     geom_area(aes(y = balance , color = type, fill = type),alpha=0.4)+
     geom_point(aes(y = balance , color = type))+
     facet_wrap(.~type)+
     theme_bw() 
     
   }
     })
#output$Plot <- renderPlot({ 
      #modalities_tidy <- data() %>% gather (key = type, value = balance, "no_contrib","fixed_contrib","growing_contrib")
      #ggplot(data = modalities_tidy, aes(x = year))+
        # geom_area(aes(y = balance , color = type, fill = type),alpha=0.4)+
         #geom_point(aes(y = balance , color = type))+
         #facet_wrap(.~type)+
         #theme_bw() 
       #})
     
  


   
   #generating the table for balances
   output$Balances <- renderTable({
     data()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

