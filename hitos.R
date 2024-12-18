#### codigo linea de hitos####
library(ggplot2)
library(scales)
library(lubridate)
library(readxl)
library(plotly)
library(htmlwidgets)

df <- read_xlsx("input_hitos.xlsx")

df$date <- with(df, ymd(sprintf('%04d%02d%02d', year, month, 1)))
df <- df[with(df, order(date)), ]
head(df)

type_levels <- unique(df$type)
type_colors <- c("#2C5530", "#739E82", "#669BBC", "#D38B5D")
#99621E en caso de que tengamos otra categoria usamos este color :)

df$type <- factor(
  df$type, 
  levels=type_levels, 
  ordered=TRUE)


positions <- c(.02, -0.02, 0.02, -0.02, 0.02, -0.02)
directions <- c(1, -1)

line_pos <- data.frame(
  "date"=unique(df$date),
  "position"=rep(positions, length.out=length(unique(df$date))),
  "direction"=rep(directions, length.out=length(unique(df$date)))
)

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
