library(shiny)
library(dplyr)
library(plotly)

ui <- fluidPage(
  titlePanel("R workshop 2018"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Step 1: basic shiny application
      selectInput("varY","Choose Y variable:",
                  choices = list(`Height`="height",
                                 `Mass`="mass",
                                 `Birth Year`="birth_year"))#,

      # different widget - same result
      # radioButtons("varY","Choose Y variable:",
      #             choices = list(`Height`="height",
      #                            `Mass`="mass",
      #                            `Birth Year`="birth_year"))
      # 
      # Step 3: UI dependent UI
      # uiOutput("colList")
      
    ),
    
    mainPanel(
      # Step 1:
      plotOutput("scatter1")
      
      # Step 2: Add interaction with plot 
      # plotOutput("scatter1",
      #            click = "plot_click",
      #            hover = hoverOpts(id="plot_hov")),
      # fluidRow(column(6,textOutput("click_name")),
      #          column(6,textOutput("hover_name")))
      
      # Step 2.2 : same thing but using plotly
      # plotlyOutput("scatter1")
      
      
    )
  )
)

server <- function(input, output) {
  
  # Step 1:
  output$scatter1 <- renderPlot({
    plot(x=starwars$height,y=starwars[[input$varY]], xlab = "Height", ylab = input$varY)
  })
  
  # Step 2:
  # output$click_name <- renderText({
  #   xx <- nearPoints(starwars,input$plot_click,"height",input$varY)
  #   xx$name[1]
  # })
  # output$hover_name <- renderText({
  #   # can remove null statement but then an error appears
  #   if(is.null(input$plot_hov$x)){return("To use hover please place mouse over plot!")}
  #   paste0("X=",round(input$plot_hov$x)," Y=",round(input$plot_hov$y))
  # })
  # 
  # Step 2.2:
  # output$scatter1 <- renderPlotly({
  #   p <- ggplot(starwars, aes(x=height,y=starwars[[input$varY]])) +
  #     geom_point() + labs(x="height",y=input$varY)
  #   ggplotly(p)
  # })
  
  # Step 3:
  # output$colList <- renderUI({
  #   if(input$varY=="mass"){
  #   selectInput("newCol","Choose Color", choices=rainbow(10))
  #   } else {selectInput("newCol","Choose Color", choices=heat.colors(10))}
  # })
  # 
  # output$scatter1 <- renderPlot({
  #   plot(x=starwars$height,y=starwars[[input$varY]], xlab = "Height", ylab = input$varY,
  #        col=input$newCol)
  # })
  # 
  # Step 4: observe
  # values <- reactiveValues(names1="Banana")
  # 
  # observeEvent(input$plot_click,{
  #   xx <- nearPoints(starwars,input$plot_click,"height",input$varY)
  #   values$names1 <- xx$name[1]
  # })
  # 
  # output$click_name <- renderText({
  #   values$names1
  # })
  
  # Step 5: reactive
  # names1 <- eventReactive(input$plot_click,{
  #   xx <- nearPoints(starwars,input$plot_click,"height",input$varY)
  #   return(xx$name[1])
  # })
  # 
  # output$click_name <- renderText({
  #   names1()
  # })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)