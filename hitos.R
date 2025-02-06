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
    type == "Creación del grupo de Ecoinformática" ~ colores[1],
    type == "Primeras Jornadas Ecoinformáticas" ~ colores[2],
    type == "Notas ecoinformáticas" ~ colores[3],
    type == "Seminarios" ~ colores[4],
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
    y = rep(0, nrow(datos)),  # Movemos la línea hacia abajo, ajusta el valor para acercarla a los números
    type = "scatter",
    mode = "lines",  # Solo líneas, sin puntos
    line = list(color = "black", width = 2),
    showlegend = FALSE  # Esto evita que la línea aparezca en la leyenda
  ) %>%
  # Añadir los puntos (marcadores) con colores personalizados y quitar la leyenda
  add_trace(
    data = datos,
    x = ~fecha,
    y = rep(0, nrow(datos)),  # Movemos los puntos hacia abajo, en la misma posición que la línea
    type = "scatter",
    mode = "markers",
    marker = list(size = 10, color = ~color),  # Asignamos los colores personalizados aquí
    text = ~paste(fecha_formateada, "<br>", type, "<br>", titulo),  # Mostramos fecha y tipo
    hoverinfo = "text",  # Queremos mostrar solo el texto personalizado
    customdata = ~link,  # Agregar la columna de enlaces a customdata
    showlegend = FALSE  # Esto elimina la leyenda de los puntos
  ) %>%
  layout(
    title = "",
    xaxis = list(
      title = "", 
      showgrid = TRUE,
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


# Guardar como archivo HTML 
saveWidget(grafico, "images/clickable_plot_horizontal.html", selfcontained = TRUE)

