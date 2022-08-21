library(shiny)
library(bslib)              #Para cargar el tema
library(thematic)
library(ggplot2)
library(dplyr)
library(shinycssloaders)
library(shinydashboard)
library(shinythemes)
library(shinyfullscreen)


# Theme setup
miTema <-
  bs_theme(4, 'superhero', base_font = font_google("Fira Sans"))
tema_luz <- bs_theme(4, 'flatly')
thematic_shiny(font = "auto")

localTeams <- "home.score"
awayTeams <- "away.score"

ui <- fluidPage(
  theme = miTema,
  title = "Proyecto Final R/Shiny - BEDU/Santander",
  lang = "es",
  
  fluidRow(
    column(2, align = "center", img(
      src = "logo.png",
      height = 90,
      width = 72
    )),
    column(
      2,
      align = "center",
      imageOutput(
        outputId = "ligaLogo",
        height = "100px",
        width = "100px",
        inline = T
      )
    ),
    column(6, titlePanel("Proyecto R - BEDU: LaLiga Española")),
    column(
      2,
      radioButtons(
        "tema_actual",
        "Modo:",
        choiceNames = list(icon("moon"),
                           icon("sun")),
        choiceValues = list("Oscuro",
                            "Luz"),
        inline = T
      ),
    ),
  ),
  
  fluidRow(column(
    12,
    navlistPanel(
      widths = c(3, 9),
      tabPanel(
        title = "Descripción e Integrantes",
        icon = icon("glyphicon glyphicon-user", lib = "glyphicon"),
        h1("Descripción del Proyecto", align = "center"),
        br(),
        HTML("<p style='text-align: justify'>
        El proyecto consiste en la construcción de un dashboard
        usando conceptos y técnicas aprendidas a lo largo del módulo.<br><br>
        La Primera División de España, conocida comúnmente como LaLiga,es la 
        mayor
        categoría del sistema de ligas de fútbol de España considerada una de 
        las
        cinco grandes ligas europeas, más información
        <a href='https://es.wikipedia.org/wiki/Primera_Divisi%C3%B3n_de_Espa%C3%B1a'>
        aquí</a><br><br>
        A continuación se presenta información de los partidos jugados por 
        temporada,
        desde 2017 hasta 2020, <a href='https://www.football-data.co.uk/spainm.php'>
        fuente</a>.<br><br>
        Con ayuda de este Dashboard puedes analizaruna predicción del resultado 
        de un partido futuro, basada en los resultados de partidos previos de 
        LaLiga.<br><br>
        Puedes encontrar un video en el que se describen las principales
        características de este proyecto en la siguiente liga ->
        <a href='https://www.google.com'>video</a>.<br><br>
        ¡Esperamos que disfrutes tu paso por aquí! </p>
          <h2>Integrantes - Equipo 19</h2>
          <ul>
            <li>Miguel Ángel Romero Hernández</li>
            <li>Adrian Reyes Cruz</li>
            <li>Gustavo Adolfo Bonilla González</li>
            <li>Carlos Rodolfo Lira Rico</li>
            <li>Marcos Rivera Almazo</li>
          </ul>"
        )
      ),
      tabPanel(
        title = "Gráficas de Goles",
        icon = icon("glyphicon glyphicon-stats", lib = "glyphicon"),
        
        titlePanel(h2("Gráficas de resultados de los partidos")),
        br(),
        p(
          "A continuación se muestran los acumulados de goles por equipos visitantes
    y de casa. Las gráficas están separadas por equipo visitante. En cada gráfico,
    los distintos colores de las barras representan los goles acumulados en partidos
    donde el ganador fue el equipo de casa (verde), el visitante
    (azul), o si el resultado fue empate (naranja)."
        ),
        br(),
        p(
          "Seleccione si desea desplegar el acumulado de goles por equipos 
          visitantes o de casa, si desea ver los resultados de todos los 
          equipos o de un equipo en específico."
        ),
        
        
        fluidRow(column(
          6,
          selectInput(
            inputId = "team.type",
            label = "Seleccione el tipo de equipo",
            choice = c("Local", "Visitante")
          )
        ),
        column(6,
               uiOutput("team.select"))),
        
        shinycssloaders::withSpinner(plotOutput("bar.grafics", "auto", 700)),
        
        
        p(
          "Un ejemplo sería:  usando la gráfica de partidos con Barcelona como visitante,
    acumulando por número de goles de casa, se puede observar que la cantidad de
    partidos en las que el equipo de casa anotó un gol o menos y empató es, al
    menos, igual al número de veces que perdió (la barra de partidos con victoria
    de visitante podría estar detrás). Curiosamente, cuando el equipo de casa anota
    dos goles, el Barcelona (visitante) suele ganar: hay una cantidad mayor de resultados
    de victoria de visitante, seguido de empate y por último de casa. A pesar de esto, el
    número de goles con los cuales ha ganado en más ocasiones el equipo de casa 
          es también igual a dos."),
        
      ),
      tabPanel(
        "Probabilidades Marginales",
        titlePanel(h2(
          "Gráficas de Probabilidades Marginales", align = "left"
        )),
        icon = icon("glyphicon glyphicon-picture", lib = "glyphicon"),
        
        HTML(
          "
      <p style='text-align: justify'>
        En esta sección se exhiben gráficos con las probabilidades marginales
        para el número de goles de visitante y de casa, así como un mapa de 
        probabilidades conjuntas de goles de visitante y de casa. "),
        
        tabsetPanel(
          type = "pills",
          tabPanel(
            title = "Marginales Local",
            icon = icon("dot-circle-o"),
            br(),
            h3("Probabilidades marginales de los goles del equipo local", align = "center"),
            imageOutput(outputId = "pw31", inline = T),
            HTML(
              "
      <p style='text-align: justify'>
        Se observa como las probabilidades marginales, para goles de casa, 
              tienden a acumularse alrededor de 1."
            )
          ),
          tabPanel(
            title = "Marginales Visitante",
            icon = icon("dot-circle-o"),
            br(),
            h3(
              "Probabilidades marginales de los goles del equipo visitante",
              align = "center"
            ),
            imageOutput(outputId = "pw32", inline = T),
            HTML(
              "
      <p style='text-align: justify'>
        Se observa como las probabilidades marginales, para goles de visitante, 
        tienden a acumularse alrededor de 0. Es importante resaltar que la
        probabilidad marginal para goles de visitante no considera el número 
        de goles de casa."
            )
          ),
          tabPanel(
            title = "Heatmap Conjuntas",
            icon = icon("dot-circle-o"),
            br(),
            h3(
              "Heatmap de las probabilidades conjuntas estimadas del número de 
              golesanotados",
              align = "center"
            ),
            imageOutput(outputId = "pw33", inline = T),
            HTML(
              "
      <p style='text-align: justify'>
        Al analizar la probabilidad conjunta, se advierte el comportamiento
        anterior, al existir una acumulación alrededor de los resultados 
              cercanos a 1-1."
            )
          )
        ),
      )
      ,
      tabPanel(
        "Ganancias Estimadas",
        titlePanel(h3("Ganancias Estimadas")),
        icon = icon("glyphicon glyphicon-piggy-bank", lib = "glyphicon"),
        HTML("<p style='text-align: justify'>
               Mediante un “ranking” de equipos basado en el modelo pesado por el
               tiempo de Dixon y Coles se generaron las predicciones
               mostradas en los gráficos. Este modelo permite estimar la
               fuerza de ataque (FA) y de defensa (FD) de cada equipo,
               donde la razón entre FA del equipo A y FB del equipo B es el
               número de goles anotados por el equipo A en un partido
               entre A y B. Más información
               <a href='http://lastplanetranking.blogspot.com/2013/11/about.html'>
               aquí.</a> <br>
               ○	Se simularon una serie de apuestas sobre el total
               de goles, basadas en predicciones del modelo. Una apuesta
               a más de 2.5 goles se gana cuando el total de goles es
               mayor o igual a 3; una apuesta a menos de 2.5 goles se
               gana cuando el total es menor o igual a 2. En los datos
               se incluyen los momios máximos y promedio reportados por
               el sitio Betbrain. Se parte de un capital de $50,000, y
               se apuesta a más o menos de 2.5 goles según lo prediga
               el modelo, además, si los momios dan una ganancia total
               mayor a 1.5 o 1.7 en cada caso."
        ),
        
        tabsetPanel(
          type = "pills",
          tabPanel(
            title = "Factor de Ganancia Máximo",
            icon = icon("dot-circle-o"),
            br(),
            h3("Factor de Ganancia Máximo"),
            imageOutput(outputId = "maxMom",
                        inline = T),
            HTML("<p style='text-align: justify'>
                  <br>○	En esta gráfica se muestra el 
                  escenario de momios máximos.
                  Se observa que para la última fecha se
                  reportarían más ganancias, debido a que
                  el capital predicho es superior al inicial.
                  Este es un escenario ideal donde se logra
                  apostar con el corredor de apuestas con
                  mejores momios."
            )
          ),
          tabPanel(
            title = "Factor de Ganancia Promedio",
            icon = icon("dot-circle-o"),
            br(),
            h3("Factor de Ganancia Promedio"),
            imageOutput(outputId = "proMom",
                        inline = T),
            HTML(
              "
      <p style='text-align: justify'>
        <br>○	En el escenario de momios promedio, modelado en esta
        gráfica, se percibe que para la última fecha
        los resultados son negativos, con el capital
        alrededor de 30,000. Sin embargo, existe
        cierta tendencia positiva a partir de 200
        partidos. Una posible explicación podría
        ser que, previo a este punto, el modelo
        no contaba con suficiente información para
        efectuar una predicción más certera.
        Así mismo, no se descarta la posibilidad
        de que a partir de este punto las predicciones
        mejoren y, a largo plazo, sea posible recuperar
        el capital o reportar ganancias."
            )
          ),
        )
        
      ),
      #Pestania de Data frame. Nota: Puede tomar algo de tiempo en cargar
      
      tabPanel(
        title = "Tabla de Resultados de Partidos",
        titlePanel(h3("Tabla de Resultados de Partidos")),
        icon = icon("glyphicon glyphicon-th", lib = "glyphicon"),
        
        HTML(
          "
      <p style='text-align: justify'>
        En la siguiente tabla se muestran los resultados de los partidos
               que fueron obtenidos del set de datos en formato CSV."
        ),
        
        shinycssloaders::withSpinner(dataTableOutput("match.data"))
      )
    )
  ))
)



server <- function(input, output, session) {
  datosLiga <- read.csv("match.data.csv", header = T)
  
  #Gráficas                       <----------
  data <- read.csv("./www/match.data.csv", header = T)
  data <- na.omit(data)
  data <- mutate(data,
                 FTR = ifelse(
                   home.score > away.score,
                   "Local",
                   ifelse(home.score < away.score,
                          "Visitante",
                          "Empate")
                 ))
  output$team.select = renderUI({
    teams <- unique(select(data, home.team))
    teams <- teams[order(teams), ]
    teams <- append(c("Todos los equipos"), teams)
    selectInput(inputId = "team.name",
                label = "Seleccione el equipo",
                choice = teams)
    
  })
  
  output$bar.grafics <- renderPlot({
    team <-
      #ifelse("Local" == input$team.type, "home.score", "away.score")
      ifelse("Local" == input$team.type, localTeams, awayTeams)
    
    show.one.team <-
      !is.null(input$team.name) && input$team.name != "Todos los equipos"
    
    
    if (show.one.team) {
      data <- filter(data, home.team == input$team.name)
    }
    
    x <- data[, team]
    
    data %>% ggplot(aes(x, fill = FTR)) +
      geom_bar() +
      {
        if (!show.one.team)
          facet_wrap("away.team")
      } +
      labs(x = input$team.type, y = "Goles") +
      ylim(0, 50)
  })
  
  output$description <- renderText({
    team.name <- ifelse(!is.null(input$team.name),
                        input$team.name,
                        "Todos los equipos")
    
    team.txt <- ifelse(
      team.name != "Todos los equipos",
      paste(team.name, "participó"),
      paste(tolower(team.name), "participaron")
    )
    
    paste(
      "Total de goles anotados donde",
      team.txt,
      "como equipo",
      tolower(input$team.type),
      "."
    )
  })
  
  output$match.data <- renderDataTable({
    data
  },
  options = list(
    processing = TRUE,
    lengthMenu = c(10, 30, 50, 100),
    pageLength = 10,
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    columns = list(
      list(title = "Fecha"),
      list(title = "Equipo local"),
      list(title = "Goles local"),
      list(title = "Equipo visitante"),
      list(title = "Goles visitante"),
      list(title = "Resultado partido")
    )
  ))
  
  
  
  #Para cargar el data frame.
  output$data_table <- renderDataTable({
    datosLiga
  },
  options = list(
    aLengthMenu = c(5, 25, 50),
    iDisplayLength = 10
  ))
  
  
  
  
  #Para el switch de modo oscuro
  observe({
    # El 4 indica la version de bootstrap
    session$setCurrentTheme(if (input$tema_actual == "Luz") {
      bs_theme_update(miTema, bootswatch = 'flatly')
    } else {
      bs_theme_update(miTema, bootswatch = 'superhero')
    })
  })
  
  #Elegir una imagen distinta segun el tema
  #Imagenes condicionales
  output$maxMom <- renderImage({
    if (input$tema_actual == "Oscuro")
      maxImg <- "www/momMaxO.png"
    else
      maxImg <- "www/momMaxL.png"
    list(src = maxImg)
  }, deleteFile = FALSE)
  
  output$proMom <-
    renderImage({
      if (input$tema_actual == "Oscuro")
        proImg <- "www/momMedO.png"
      else
        proImg <- "www/momMedL.png"
      list(src = proImg)
    }, deleteFile = FALSE)
  
  output$ligaLogo <- renderImage({
    if (input$tema_actual == "Oscuro")
      logoL <- "www/laligaOs.png"
    else
      logoL <- "www/laligas.png"
    list(src = logoL)
  }, deleteFile = FALSE)
  
  output$pw31 <- renderImage({
    if (input$tema_actual == "Oscuro")
      pw1 <- "www/Pw31N.png"
    else
      pw1 <- "www/Pw31.png"
    list(src = pw1)
  }, deleteFile = FALSE)
  
  output$pw32 <- renderImage({
    if (input$tema_actual == "Oscuro")
      pw2 <- "www/Pw32N.png"
    else
      pw2 <- "www/Pw32.png"
    list(src = pw2)
  }, deleteFile = FALSE)
  
  output$pw33 <- renderImage({
    if (input$tema_actual == "Oscuro")
      pw3 <- "www/Pw33N.png"
    else
      pw3 <- "www/Pw33.png"
    list(src = pw3)
  }, deleteFile = FALSE)
  
  
}


shinyApp(ui = ui, server = server)
