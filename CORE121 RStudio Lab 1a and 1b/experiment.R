library(plotly)
library(sf)
library(maps)
library(mapdata)

us <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

us$random_number <- sample(100, size = nrow(us), replace = TRUE)

us_plot <- ggplotly(ggplot(us, aes(text = paste(ID))) + 
                      geom_sf(aes(fill = random_number)) + 
                      ggtitle("Random numbers")  +
                      scale_fill_continuous("Random")) 


us_plot
  
  style(us_plot, hoveron = “fills”)

  
  p_2 <- ggplot(nc_2) +
    geom_sf(aes(fill = BIR74, text = tooltip_text, color = NAME))
  
  gg_2 <- ggplotly(p_2)
  
  gg_2 %>%
    style(
      hoveron = "fills",
      # override the color mapping
      line.color = toRGB("gray40"),
      # don't apply these style rules to the first trace, which is the background graticule/grid
      traces = seq.int(2, length(gg_2$x$data))
    ) %>%
    hide_legend()