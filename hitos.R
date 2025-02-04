library(readxl)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(dplyr)

# Cargar los datos desde un archivo Excel (reemplaza "datos.xlsx" con tu archivo real)
datos <- read_excel("input_hitos.xlsx")
datos <- datos %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

# Crear una columna de fecha combinando el año y el mes
datos <- datos %>%
  mutate(fecha = as.Date(paste(year, month, "01", sep = "-")))

# Extraer los años únicos para el eje X
anos_unicos <- unique(format(datos$fecha, "%Y"))

# Definir colores personalizados: rojo, azul, verde y amarillo
colores <- c("#2C5530", "#739E82", "#669BBC", "#D38B5D")

# Crear una columna de color para asignar colores a cada tipo
datos <- datos %>%
  mutate(color = case_when(
    type == unique(datos$type)[1] ~ colores[1],
    type == unique(datos$type)[2] ~ colores[2],
    type == unique(datos$type)[3] ~ colores[3],
    type == unique(datos$type)[4] ~ colores[4],
    TRUE ~ "#000000"  # Valor por defecto si no encuentra una coincidencia
  ),
  fecha_formateada = format(fecha, "%b %Y")  # Formato para mostrar mes y año
  )

# Crear el gráfico interactivo
grafico <- plot_ly() %>%
  # Añadir la línea negra (movida hacia abajo)
  add_trace(
    data = datos,
    x = ~fecha,
    y = rep(-0.44, nrow(datos)),  # Movemos la línea hacia abajo, ajusta el valor para acercarla a los números
    type = "scatter",
    mode = "lines",  # Solo líneas, sin puntos
    line = list(color = "black", width = 2),
    showlegend = FALSE  # Esto evita que la línea aparezca en la leyenda
  ) %>%
  # Añadir los puntos (marcadores) con colores personalizados y quitar la leyenda
  add_trace(
    data = datos,
    x = ~fecha,
    y = rep(-0.44, nrow(datos)),  # Movemos los puntos hacia abajo, en la misma posición que la línea
    type = "scatter",
    mode = "markers",
    marker = list(size = 10, color = ~colores),  # Asignamos los colores personalizados aquí
    text = ~paste(fecha_formateada, "<br>", type, "<br>", titulo),  # Mostramos fecha y tipo
    hoverinfo = "text",  # Queremos mostrar solo el texto personalizado
    customdata = ~link,  # Agregar la columna de enlaces a customdata
    showlegend = FALSE  # Esto elimina la leyenda de los puntos
  ) %>%
  layout(
    title = "",
    xaxis = list(
      title = "", 
      showgrid = FALSE,
      tickangle = 0,  # Esto controla el ángulo de los números
      tickpadding = 0,  # Elimina el espacio entre los números y la línea
      tickfont = list(size = 12, family = "Arial", color = "black", y = -10),  # Mueve las etiquetas hacia abajo
      tickmode = "array",  # Modo personalizado para los ticks
      tickvals = as.Date(paste(anos_unicos, "01", "01", sep = "-")),  # Aseguramos que solo haya un número por año
      ticktext = anos_unicos,  # Mostramos solo el año
      showticklabels = TRUE
    ),
    yaxis = list(title = "", showticklabels = FALSE, 
                 showgrid = FALSE, zeroline = FALSE, range = c(-0.5, 0.5))# Ajustamos el rango del eje Y
  ) %>%
  event_register("plotly_click")

# Agregar funcionalidad de clic para abrir enlaces correctamente
grafico <- onRender(grafico, "
  function(el, x) {
    el.on('plotly_click', function(data) {
      var url = data.points[0].customdata;
      if (url) {
        window.open(url, '_blank');
      }
    });
  }")

<<<<<<< HEAD
df <- merge(x=df, y=line_pos, by="date", all = TRUE)
df <- df[with(df, order(date, type)), ]

head(df)


text_offset <- 0.005

df$month_count <- ave(df$date==df$date, df$date, FUN=cumsum)
df$text_position <- 
  (df$month_count * text_offset * df$direction) + 
  df$position
head(df)


month_buffer <- 2

month_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)


year_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)


#### PLOT ####
# Show text for each milestone
library(stringr)

  # c("Creación del grupo de\n Ecoinformática",
  #      "Primera nota ecoinformática", 
  #      "V Aniversario del grupo", 
  #      "Primer Seminario Ecoinformático", 
  #      "Primeras Jornadas\n Ecoinformáticas")

timeline_plot <- 
  df |> 
  ggplot(aes(y = date, col = type)) + # Cambiamos x e y
  labs(col = "type") +
  scale_color_manual(
    values = type_colors, 
    labels = type_levels, 
    drop = FALSE
  ) +
  coord_flip() +
  theme_classic() +
  # Plot a longer vertical black line for timeline
  geom_vline(
    xintercept = 0, 
    color = "black", linewidth = 0.3
  ) +
  # Plot scatter points at zero and date
  geom_point(aes(x = 0), size = 3) +
  # Extend x-axis range to make vertical line longer
  scale_x_continuous(
    limits = c(-0.03, 0.03),  # Ajusta los límites del eje X
    expand = c(0, 0)          # Evita expansión adicional del eje
  ) +
  # Don't show axes, appropriately position legend
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "right" # Ajustar posición de la leyenda
  ) +
  # Adjust position of year text
  geom_text(
    data = year_df, 
    aes(y = year_date_range, 
        x = -0.01,   # Cambia esta coordenada para ajustar la posición de las etiquetas
        label = year_format, 
        fontface = "bold"),
    size = 4.5, color = 'black'
  ) +
  theme(
    legend.position = "none"
    )

timeline_plot

interactive_plot <- ggplotly(timeline_plot)

p_interactive <- interactive_plot %>%
  layout(title = "",
         margin = list(l = 0, r = 0, t = 0, b = 0)) |> 
  onRender("
    function(el, x) {
      el.on('plotly_click', function(data) {
        var pointIndex = data.points[0].pointIndex; // Índice del punto clicado
        var link = x.data[0].customdata[pointIndex]; // Obtener el enlace del punto
        if (link) {
          window.open(link); // Abrir el enlace en una nueva pestaña
        }
      });
    }
  ")

p_interactive$x$data[[1]]$customdata <- df$link

# Guardar como archivo HTML 
saveWidget(p_interactive, "clickable_plot_vertical.html")

# # Save #
# ggsave("plot_hitos.jpg", 
#        timeline_plot1, 
#        dpi = 300, 
#        units = "cm", 
#        width = 33, 
#        height =8.9, 
#        limitsize = F)
=======
# Guardar el gráfico como HTML
saveWidget(grafico, "clickable_plot_horizontal.html", selfcontained = TRUE)
>>>>>>> origin/Hitos
