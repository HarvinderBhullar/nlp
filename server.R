library(shiny)

shinyServer(function(input, output) {
    
    output$caption <- renderText({
       
        curr <- predictcurr(,input$caption)
        paste( curr)
    })
    
    output$caption2 <- renderText({
     
       w <- predictwords(,input$caption) 
       paste(w )
      
    })
    
    
})