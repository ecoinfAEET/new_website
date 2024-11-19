
data <- data.frame(
  check.names = FALSE,
  `price` = c(6790000, 6950000, 5990000, 5990000, 4150000, 8654100),
  `rooms` = c("4+1", "3+1", "2+kk", "2+1", "1+kk", "4+1"),
  `m2` = c(78, 78, 58, 52, 30, 100),
  `location` = c("Bohunice", "Bystrc", "Plotni", "Lesna", "Styrice", "Zabrdovice"),
  `link` = c(
    "https://www.sreality.cz/detail/prodej/byt/4+1/brno-bohunice-moldavska/2155567196",
    "https://www.sreality.cz/detail/prodej/byt/3+1/brno-bystrc-cerneho/2881087836",
    "https://www.sreality.cz/detail/prodej/byt/2+kk/brno--plotni/2840209756",
    "https://www.sreality.cz/detail/prodej/byt/2+1/brno-lesna-triskalova/2905770332",
    "https://www.sreality.cz/detail/prodej/byt/1+kk/brno-styrice-videnska/3682412892",
    "https://www.sreality.cz/detail/prodej/byt/4+1/brno-zabrdovice-kornerova/2548312156"
  )
)



library(ggplot2)
library(plotly)
library(htmlwidgets)

g <- ggplot(data, aes(
  x = date, y = 0,
  text = paste0(
    "Type: ", rooms, "\n",
    "Price: ", price, "\n",
    "M2: ", m2, "\n",
    "Location: ", location
  ),
  customdata = link
)) +
  geom_point(aes(fill = rooms), color = "black", size = 3, alpha = 0.9) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(0), max(350), by = 50)) +
  scale_y_continuous(breaks = seq(min(0), max(40000000), by = 2000000)) +
  theme(
    legend.title = element_blank()
  )

p <- ggplotly(g, tooltip = c("text"))

onRender(
  p, "
  function(el) {
    el.on('plotly_click', function(d) {
      var url = d.points[0].customdata;
      window.open(url);
    });
  }
"
)