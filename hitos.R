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

df$type <- factor(df$type, levels=type_levels, ordered=TRUE)


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
df$text_position <- (df$month_count * text_offset * df$direction) + df$position
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

l <- c("Creación del grupo de\n Ecoinformática",
       "Primera nota ecoinformática", 
       "V Aniversario del grupo", 
       "Primer Seminario Ecoinformático", 
       "Primeras Jornadas\n Ecoinformáticas")

timeline_plot <- 
  df |> 
  ggplot(aes(x=date,y=0, col=type)) +
  labs(col="Milestones") +
  scale_color_manual(
    values=type_colors, 
    labels=type_levels, 
    drop = FALSE) +
  theme_classic() +
  # Plot horizontal black line for timeline
  geom_hline(
    yintercept = 0, 
    color = "black", linewidth=0.3
  ) +
  # Plot vertical segment lines for milestones
  geom_segment(
    data = df[df$month_count == 1,], 
    aes(y = position, yend = 0, xend = date), 
    color = 'black', linewidth = 0.2
  ) +
  # Plot scatter points at zero and date
  geom_point(aes(y = 0), size = 3
  )+
  # Don't show axes, appropriately position legend
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    legend.position = "bottom"
  ) +
  # Show text for each month
  #timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=2.5,vjust=0.5, color='black', angle=90)
  # Show year text
  geom_text(
    data = year_df, 
    aes(x = year_date_range, 
        y = -0.004,
        label = year_format, 
        fontface = "bold"),
    size = 2.5, color='black'
  ) +  
  geom_text(
    aes(y = text_position, label= l),
    size = 3
  ) +
  theme(legend.title = element_blank())


timeline_plot


# Save #
ggsave("plot_hitos.jpg", 
       timeline_plot1, 
       dpi = 300, 
       units = "cm", 
       width = 33, 
       height =8.9, 
       limitsize = F)
