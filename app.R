#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(lubridate)

datos_total <- read_excel("G:/Mi unidad/CURSOS/R y Shiny/datos_sipa.xlsx", sheet = "totales")
datos_total$fecha <- as.Date(datos_total$fecha, format = "%d/%m/%Y")

thematic::thematic_on()

# Define UI for application that draws a histogram
ui <- page_fluid(
  ### bslib theme
  theme = bs_theme(preset = "quartz",
                   base_font = font_google("Poppins")),
  page_navbar(
    title = "INFORME EMPLEO ASALARIADO FORMAL",
    nav_panel("Empleo total", 
              fluidRow(column(6,
                              value_box(
                                title = "Total asalariados registrados (en miles)",
                                value = "10,123.8",
                                showcase = bs_icon(name = "people-fill"),
                                "Agosto 2024"
                              )), # cantidad de asalariados 
                       column(6,
                              value_box(
                                title = "Var. asalariados registrados",
                                value = "0.02%",
                                showcase = bs_icon(name = "percent"),
                                "Julio-Agosto 2024"
                              )) # variación
                       ),
              fluidRow(
                column(4, plotlyOutput("asal_total_lp"),
                       dateRangeInput(inputId = "selector_asal_lp", 
                                      label = "Años",
                                      start = Sys.Date() - years(1), 
                                      end = Sys.Date()
                       )
                ),
                column(4, plotlyOutput("asal_total_var"),
                       sliderInput(
                         "slider_asal_var", "Slider",
                         min = 2012, max = 2024,
                         value = c(2023, 2024)
                       )
                ),
                column(4, plotlyOutput("asal_total_pie"),
                       dateInput(
                         inputId = "fecha_pie",  # Cambiar inputID a inputId
                         label = h3("Date input"),
                         value = Sys.Date(),
                         format = "dd-mm-yyyy"
                       )
                )
              )
              ),
    nav_panel("Empleo privado", # EMPLEO PRIVADO
              navset_pill(
                nav_panel("Total","Contenido total asal priv"),
                nav_panel("Rama de actividad","Contenido de rama de actividad"),
                nav_panel("Provincia", "contenido por provincias")
              )
              ),
    nav_panel("Remuneraciones", "Contenido"),
    nav_menu("Más",
             nav_panel("Fuentes de información", "Fuentes"),
             nav_panel("Contacto", "Form de contacto")
    )
  )  # Cerrar correctamente el page_navbar
)  # Cerrar correctamente el page_fluid



server <- function(input, output) {

 # bs_themer()
# SERIE LARGO PLAZO
  output$asal_total_lp <- renderPlotly({
    
    filtrados1 <- datos_total %>% filter(
      fecha >= as.Date(input$selector_asal_lp[1]) & fecha <= as.Date(input$selector_asal_lp[2]))
    
      plot_lp <- ggplot(filtrados1, aes(x = fecha, y = asalariados_tot)) +
      geom_line(color = "#468a97", linewidth = 1, alpha = 0.9) + 
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      labs(x = "Periodo", y = "Asalariados registrados (en miles)") +
      theme_minimal()
      
    ggplotly(plot_lp)
  }
  )
  
  # SERIE DE VARIACIONES
  output$asal_total_var <- renderPlotly({

    filtrados2 <- datos_total %>% filter(
      year(fecha) >= input$slider_asal_var[1] & year(fecha) <= input$slider_asal_var[2])
    
    plot_var <- ggplot(filtrados2, aes(x = fecha, y=var_asal_tot*100)) +
      labs(x = "Periodo", y="Variación porcentual (%)") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.01)) +  
      geom_col(fill = "#ee6261") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(plot_var)
    
  })
  
# GRÁFICO DE TORTA
  output$asal_total_pie <- renderPlotly({
    filtrados3 <- datos_total %>% filter(
      fecha == as.Date("2024-08-01")) %>% 
      mutate(prop_asalpriv=asal_privados/asalariados_tot*100,
             prop_asalpub=asal_publicos/asalariados_tot*100,
             prop_asalsd=asal_sd/asalariados_tot*100)
    
   plot_pie <-  ggplot(filtrados3, aes(x = "", y = prop_asalpriv, fill = "Empleo Privado")) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      geom_bar(aes(y = prop_asalpub, fill = "Empleo Público"), stat = "identity", width = 1, color = "white") +
      geom_bar(aes(y = prop_asalsd, fill = "Servicio doméstico"), stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y") +  # Convertir a gráfico circular (dona)
      theme_void() +  # Eliminar el fondo y otras marcas innecesarias
      theme(legend.title = element_blank(),  # Eliminar título de la leyenda
            legend.position = "right") +
      scale_fill_manual(values = c("Empleo Privado" = "skyblue", 
                                   "Empleo Público" = "lightgreen", 
                                   "Servicio doméstico" = "orange"))  # Colores personalizados
    
    
    ggplotly(plot_pie)
  })
  }


# Run the application 
shinyApp(ui = ui, server = server)
