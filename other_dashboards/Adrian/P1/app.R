library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyfullscreen)
library(png)

url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-08/Postwork/match.data.csv"
datos <- read.csv(url, header = TRUE)

# Creamos la interfaz del usuario. Con fluidPage se crea una pantalla que se ajusta
# automáticamente a las dimensiones de la ventana del navegador.

# dashboardPage crea una página en el dashboard.

# Con dashboardHeader añadimos un título al dashboard.

# Con dashboardSidebar se crea la barra lateral para acceder al contenido del dashboard.

# Cada elemento de la barra lateral puede añadirse con menuItem.
# En menuItem, el parámetro tabName corresponde al nombre de una pestaña que este
# elemento del menú activará. badgeColor nos indica el color que daremos a unas 
# pequeñas etiquetas. Mientras que icon será una etiqueta de ícono.

# El listado de íconos puede consultarse en https://fontawesome.com/icons

# Con dashboardBody creamos los contenidos del dashboard.

# Para añadir elementos al cuerpo usamos las funciones tabItems y tabItem

addResourcePath(prefix = 'fotos', directoryPath = 'C:/Users/gabg_/Escritorio/Proyecto de R - Santander-BEDU/r_and_shiny_dashboard/app/www/' )
ui <- fluidPage(
    
    dashboardPage(
        
        dashboardHeader(title = "Proyecto 1, Fase 2.", titleWidth = 300),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Gráficas de Barras", tabName = "gdb", icon = icon("signal", class = "fa-solid"),
                         badgeLabel = "new", badgeColor = "olive"),
                menuItem("Gráficas PostWork 3", tabName = "gpw", icon = icon("images", class = "fa-solid"),
                         badgeLabel = "new", badgeColor = "olive"),
                menuItem("Tabla de Datos", tabName = "datab", icon = icon("table", class = "fa-solid"),
                         badgeLabel = "new", badgeColor = "olive"),
                menuItem("Factores de Ganacia", tabName = "fdg", icon = icon("chart-line", class = "fa-solid"),
                         badgeLabel = "new", badgeColor = "olive")
            )
        ),
        
        dashboardBody(        
            tabItems(
                
                # Nota: h1 crea un título y de h2 a h6 se crean subtítulos con una
                # jerarquía decreciente.
                # Con selectInput se crea una lista de selección para elegir 
                # uno o varios elementos de una lista de valores
                
                tabItem(tabName = "gdb",
                        fluidRow(
                            titlePanel(h1(strong("Goles de local y visitante"), align = "center")),
                            selectInput("x", "Escoje una variable:",
                                        choices = c("home.score", "away.score")),
                            plotOutput("plotfw", width = "100%", height = "650px") 
                )),
                
                # Nota: Para insertar imágenes en el dashboard, debemos crear una carpeta
                # llamada www en el mismo la misma carpeta del proyecto.
                # Consultar https://stackoverflow.com/questions/21996887/embedding-image-in-shiny-app

                tabItem(tabName = "gpw",
                        fluidRow(

                            titlePanel(h1(strong("Gráficas Postwork 3"), align = "center")),
                            h2("Probabilidades marginales de los goles del equipo local", align = "center"),
                            tags$figure(align = "center",
                            tags$img(src = "fotos/Pw31.png")),
                            h2("Probabilidades marginales de los goles del equipo visitante", align = "center"),
                            tags$figure(align = "center",
                                         tags$img(src = "fotos/Pw32.png")),
                            h2("Heatmap probabilidades conjuntas", align = "center"),
                            tags$figure(align = "center",
                                         tags$img(src = "fotos/Pw33.png"))
                        )),
                tabItem(tabName = "datab",
                        fluidRow(
                            titlePanel(h1(strong("Resultados de la Liga Española"), align = "center")),
                            dataTableOutput("data_table")
                        )),
                tabItem(tabName = "fdg",
                        fluidRow(
                            
                            titlePanel(h1(strong("Factores de ganancia"),align = "center")),
                            h2("Factor de ganancia Promedio", align = "center"),
                            tags$figure(align = "center",
                                        tags$img(src = "fotos/grafico2.png")),
                            h2("Factor de ganancia Máximo", align = "center"),
                            tags$figure(align = "center",
                                        tags$img(src = "fotos/grafico1.png")),
                        ))
            )
        )
    )
)


# Definimos la función server encargada del comportamiento de nuestra aplicación
server <- function(input, output) {
    library(ggplot2)
    library(plotly)

# RenderDataTable devuelve un dataframe muy similar a una tabla dinámica
# Con lengthMenu se define el número de datos a ser desplegados, por default es
# 5, 10, 50 y 100.

    output$data_table <- renderDataTable(
        {datos}, options = list(lengthMenu = c(5, 10, 25, 50, 100), align = 'c'))
    
    output$plotfw <- renderPlot({
        
    datos <- mutate(datos, Resultado = ifelse(home.score > away.score,
             "Local", ifelse(home.score < away.score, "Visitante", "Empate")))
        x <- datos[, input$x] 
        
        datos %>% ggplot(aes(x, fill = Resultado)) +
            geom_bar() +
            facet_wrap("away.team")+
            scale_fill_brewer(palette="YlGnBu")+theme_minimal()+
            labs(x = "Goles", y = "Resultados") +
            ylim(0, 100) +
            xlim(0, max(max(datos$home.score), max(datos$away.score)))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)