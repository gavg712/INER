shinyServer(function(input, output) {
    output$text1 <- renderText({ paste("Seleccionaste la variable", 
                                        input$var," para las fechas entre",
                                        input$dates[1], " y ",input$dates[2]) 
                                })
    y<-rnorm(22000, 15, 2)
    y2<-rnorm(22000,78, 5)
    y3<-rnorm(22000,500, 900)
    y4<-rnorm(22000,89000, 900)
    x<-seq(as.POSIXct("2013-01-01"), by ="hours", along.with=y)
    df<-data.frame(x,y, y2,y3,y4)
    names(df)<-c("fecha","Temperatura del aire", "Humedad relativa",
                "Radiacion solar", "Presion atmosferica")
    output$plot1 <- renderPlot({
        color <- switch(input$var, 
            "Temperatura del aire" = "darkred",
            "Humedad relativa" = "darkgreen",
            "Radiacion solar" = "darkorange",
            "Presion atmosferica" = "Black")
        plot(df[df[,1]>=as.POSIXct(input$dates[1]) & 
                df[,1]< as.POSIXct(input$dates[2]),1],
                df[df[,1]>=as.POSIXct(input$dates[1]) & 
                df[,1]< as.POSIXct(input$dates[2]),input$var], 
                xlab="Fecha",ylab=input$var, type="l", col=color)
    })
})
