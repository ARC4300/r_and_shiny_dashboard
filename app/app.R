library(shiny)
library(bslib)              #Para cargar el tema
library(thematic)
library(ggplot2)
library(dplyr)
library(shinycssloaders)
library(shinydashboard)
library(shinythemes)
library(shinyfullscreen)
library(png)


# Theme setup
miTema <- bs_theme(4,'solar', base_font= font_google("Fira Sans"))
tema_luz <- bs_theme(4,'solar',bg = '#558AC5', fg = '#F9B02D')
thematic_shiny(font = "auto")

ui <- fluidPage(
  theme = miTema, 
    
  fluidRow(
    column(2, align="center", img( src = "logo.png", height = 90, width = 72)),
    column(2, align="center", imageOutput(outputId = "ligaLogo", 
                              height = "100px", width = "100px", inline = T)),
    column(6, titlePanel("Proyecto R - BEDU: LaLiga Española")),
    column(2, radioButtons("tema_actual", "Modo:", 
                        choiceNames = list(icon("moon"), icon("sun")),
                        choiceValues = list("Oscuro", "Luz"),
                        inline = T),
    ),
  ),
  fluidRow(
    column(8,
      navlistPanel(
        tabPanel(
          title = "Descripcion e Integrantes",
          icon = icon("glyphicon glyphicon glyphicon-stats", lib = "glyphicon"),
          column(
            4,
            p("<Drescripcion>"),
            p("<Integrantes>")
          )
        ),
        tabPanel(
        title = "Gráficas de goles",
        icon = icon("glyphicon glyphicon glyphicon-stats", lib = "glyphicon"),
        br(),
        fluidRow(
          column(
            4,
            selectInput(
              inputId = "team.type",
              label = "Seleccione el tipo de equipo",
              choice = c("Local", "Visitante")
            )
          ),
          column(
            4,
            uiOutput("team.select")
          )
        ),
        textOutput("description"),
        
        shinycssloaders::withSpinner(
          plotOutput("bar.grafics", "auto", 700)
        ),
        
        
        p("Interpretacion")
      ),
      tabPanel("Postwork 3", 
               titlePanel(h2("Gráficas del Postwork 3", align = "center")),
               icon = icon("glyphicon glyphicon glyphicon-picture", lib = "glyphicon"),
               
               fluidRow( 
                 h3("Probabilidades marginales de los goles del equipo local", align = "center"),
                 imageOutput(outputId = "pw31", inline = T),
                 
                 h3("Probabilidades marginales de los goles del equipo visitante", align = "center"),
                 imageOutput(outputId = "pw32", inline = T),
                 
                 h3("Heatmap de las probabilidades conjuntas estimadas del número de goles
            anotados", align = "center"), imageOutput(outputId = "pw33", inline = T)
                 
               ) )
      ,
      tabPanel("Ganancias",
               titlePanel(h3("Ganancias estimadas")),
               #selectInput("tipo_momios", "Momios",
               #            c("Máximos" = "maximo",
              #               "Promedio" = "promedio")) ,
               #conditionalPanel(condition = "input.tipo_momios == 'maximo'",
               fluidRow(
                  column(6, h3("Factor de ganancia Máximo"),
                            imageOutput(outputId = "maxMom",
                                            inline = T)
                         ),
               #),
               #conditionalPanel(condition = "input.tipo_momios == 'promedio' ",
                  column(6,h3("Factor de ganancia Promedio"),
                                imageOutput(outputId = "proMom",
                                            inline = T)
                        )
               ),
              p("Interpretacion"),
                #),
        ),
      #Pestania de Data frame. Nota: Puede tomar algo de tiempo en cargar
      
      tabPanel(
        title = "Summary",
        icon = icon("glyphicon glyphicon glyphicon-stats", lib = "glyphicon"),
        
        shinycssloaders::withSpinner(
          dataTableOutput("match.data")
        ),
        p("Interpretacion"))
      
      )
    )
  )
)
    


server <- function(input, output, session) {
  
  datosLiga <- read.csv("match.data.csv", header = T)
  
  #Gráficas                       <----------
  data <- read.csv("./www/match.data.csv", header = T)
  data <- na.omit(data)
  data <- mutate(
    data,
    FTR = ifelse(
      home.score > away.score,
      "Local",
      ifelse(
        home.score < away.score,
        "Visitante",
        "Empate"
      )
    )
  )
  output$team.select = renderUI({
    teams <- unique(select(data, home.team))
    teams <- teams[order(teams),]
    teams <- append(c("Todos los equipos"), teams)
    selectInput(
      inputId = "team.name",
      label = "Seleccione el equipo",
      choice = teams
    )
    
  })
  
  output$bar.grafics <- renderPlot({
    team <- ifelse("Local" == input$team.type, "home.score", "away.score")
    show.one.team <- !is.null(input$team.name) && input$team.name != "Todos los equipos"
    
    
    if (show.one.team) {
      data <- filter(data, home.team == input$team.name)
    }
    
    x <- data[, team]
    
    data %>% ggplot(aes(x, fill = FTR)) + 
      geom_bar() + 
      { if (!show.one.team) facet_wrap("away.team") } +
      labs(x =input$team.type, y = "Goles") + 
      ylim(0,50)
  })
  
  output$description <- renderText({
    team.name <- ifelse(
      !is.null(input$team.name),
      input$team.name,
      "Todos los equipos"
    )
    
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
  
  output$match.data <- renderDataTable(
    { data },
    options = list(
      processing = TRUE,
      lengthMenu = c(10, 30, 50, 100),
      pageLength = 10,
      columns = list(
        list(title = "Fecha"),
        list(title = "Equipo local"),
        list(title = "Goles local"),
        list(title = "Equipo visitante"),
        list(title = "Goles visitante"),
        list(title = "Resultado partido")
      )
    )
  )
  
  
  #Para cargar el data frame.
  output$data_table <- renderDataTable({datosLiga}, 
                                       options = list(aLengthMenu = c(5,25,50),
                                                      iDisplayLength = 10)
  )
    
  
    
  
  #Para el switch de modo oscuro
  observe({
    # El 4 indica la version de bootstrap
    session$setCurrentTheme(
      if(input$tema_actual == "Luz"){
        bs_theme_update(miTema, bootswatch = 'solar', 
                        bg = "#FFFFFF", fg = "#FF6A39")
      } else {
        bs_theme_update(miTema, bootswatch = 'solar')
      }
    )
  })
  
  #Elegir una imagen distinta segun el tema
  #Imagenes condicionales
  output$maxMom <- renderImage({
    if(input$tema_actual=="Oscuro") maxImg <-"www/momMaxO.png"
    else maxImg <-"www/momMaxL.png"
    list(src = maxImg)
  }, deleteFile = FALSE)
  
  output$proMom <- renderImage({                                                                                                                         
    if(input$tema_actual=="Oscuro") proImg <-"www/momMedO.png"
    else proImg<-"www/momMedL.png"
    list(src = proImg)
  }, deleteFile = FALSE)
  
  output$ligaLogo <- renderImage({
    if(input$tema_actual=="Oscuro") logoL <-"www/laligaOs.png"
    else logoL<-"www/laligas.png"
    list(src = logoL)
  }, deleteFile = FALSE)
  
  output$pw31 <- renderImage({
    if(input$tema_actual=="Oscuro") pw1 <-"www/Pw31N.png"
    else pw1 <-"www/Pw31.png"
    list(src = pw1)
  }, deleteFile = FALSE)
  
  output$pw32 <- renderImage({
    if(input$tema_actual=="Oscuro") pw2 <-"www/Pw32N.png"
    else pw2 <-"www/Pw32.png"
    list(src = pw2)
  }, deleteFile = FALSE)   
  
  output$pw33 <- renderImage({
    if(input$tema_actual=="Oscuro") pw3 <-"www/Pw33N.png"
    else pw3 <-"www/Pw33.png"
    list(src = pw3)
  }, deleteFile = FALSE)
  
  
}

# Define server logic required to draw a histogram
#server <- function(input, output) {
  
  
#}

shinyApp(ui = ui, server = server)
