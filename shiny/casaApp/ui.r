shinyUI(fluidPage(
    titlePanel("Visualizacion de datos de la casa prototipo Yachay"),
    sidebarLayout(
        sidebarPanel(
            helpText("Parametros de configuracion del grafico"),
            selectInput("var", 
                label = "Seleccione una variable para mostrar",
                choices = list("Temperatura del aire", "Humedad relativa",
                "Radiacion solar", "Presion atmosferica","Viento"),
                selected = "Temperatura del aire"),
            dateRangeInput("dates", label = "Rango de fechas", 
                format= "yyyy/mm/dd", end=Sys.Date()+1)
        ),
        mainPanel(
            h3("Grafico de series temporales",align = "center"),
            h5(textOutput("text1"), align="center"),
            plotOutput("plot1")
        )
    )
))
