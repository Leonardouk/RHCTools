# Este arquivo tem como objetivo concentrar e organizar todas as funções 
# utilizadas na criação de visualizações (plots) do Dashboard RHC.

plot_stacked_bar <- function(reactive_df, x_var, fill_var, fill_values,
                             fill_labels, fill_name, x_label, y_label,
                             x_breaks = 5, y_breaks  = 5000,
                             scale_fill_type = "manual", pallete = NULL) {
  p <- reactive_df %>%
    count(.data[[x_var]], .data[[fill_var]]) %>%
    ggplot(aes(x = .data[[x_var]], y = n, fill = as.factor(.data[[fill_var]]))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = x_label, y = y_label) +
    scale_x_continuous(breaks = breaks_width(x_breaks)) +
    scale_y_continuous(breaks = breaks_width(y_breaks))
  if (scale_fill_type == "manual") {
    p <-  p + scale_fill_manual(values = fill_values, labels = fill_labels, name = fill_name)
  } else if (scale_fill_type == "discrete") {
    p <- p + scale_fill_discrete(name = fill_name)
  }
  
  ggplotly(p)
}


plot_proportional_stacked_bar <- function(reactive_df, x_var, fill_var, fill_name,
                                        x_label, y_label, x_breaks) {
  reactive_df %>%
    count(.data[[x_var]], .data[[fill_var]]) %>%
    ggplot(aes(x = .data[[x_var]], y = n, fill = as.factor(.data[[fill_var]]))) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_discrete(
      name = fill_name
    ) +
    labs(x = x_label, y = y_label) +
    scale_x_continuous(breaks = breaks_width(x_breaks))
}


plot_percentage_bar <- function(reactive_df, x_var, fill_var, fill_name, x_label,
                                y_label) {
  p <- reactive_df %>%
    count(.data[[x_var]], .data[[fill_var]]) %>%
    mutate(percentage = n / sum(n)) %>%
    ggplot(aes(x = .data[[x_var]], y = percentage, fill = as.factor(.data[[fill_var]]))) +
    geom_bar(stat = "identity", position = "stack") +
    scale_y_continuous(labels= scales::percent) +
    scale_fill_viridis_d(option = "D", name = fill_name)
    labs(x = x_label, y = y_label)
  
  ggplotly(p)
}


plot_treemap <- function(reactive_df, x_var, gradient_colours, fill_name,
                         group_threshold = 0) {
  p <- reactive_df %>%
    count(.data[[x_var]]) %>%
    
    if (group_threshold) {
      reactive_df <- mutate(
        percentage = n / sum(n) * 100,
        x_var_grouped = ifelse(percentage < 1, "Outros (< 1%)", .data[[x_var]])
      ) %>%
        group_by(x_var_grouped) %>% summarise(n = sum(n)) %>% ungroup()
      x_var <- "x_var_grouped"
    }
    
    mutate(
      pct_label = paste0(round(percentage, 1), "%"),
      x_var_wrapped = str_wrap(.data[[x_var]], width = 10)
    ) %>%
    ggplot(aes(area = n, fill = n)) +
    scale_fill_gradientn(colours = gradient_colours,
                         name = fill_name) +
    geom_treemap() +
    geom_treemap_text(aes(label = x_var_wrapped), fontface = "italic", 
                      colour = "white",place = "topleft", grow = TRUE,
                      reflow = TRUE, alpha = 0.65) +
    geom_treemap_text(aes(label = pct_label), fontface = "bold",
                      colour = "white", place = "bottomright", min.size = 8)
    
  ggplotly(p)
}


plot_map <- function(reactive_df, map_data, pallete, fill_var,
                     interactive_click = 0, label, legend_title) {
  leaflet(map_data) %>%
    addPolygons(
      fillColour = ~pallete(fill_var),
      fillOpacity = .7,
      colour = "white",
      weight = 1,
      label = label,
      if (interactive_click) {
        layer_id = ~id_click
      }
    ) %>%
    addLegend(pal = pallete, values = ~fill_var, title = legend_title, 
              position = "bottomright") %>%
    fitBounds(
      lng1 = st_bbox(map_data)[["xmin"]],
      lat1 = st_bbox(map_data)[["ymin"]],
      lng2 = st_bbox(map_data)[["xmax"]],
      lat2 = st_bbox(map_data)[["ymax"]]
    )
}


plot_pizza <- function(reactive_df, y_var, fill_var) {
  p <- reactive_df %>%
    count(.data[[y_var]]) %>%
    ggplot(aes(x = "", y = n,  fill = fill_var)) +
    geom_bar(stat = "identity", width = 1, colour = "white") +
    coord_polar("y", start = 0) +
    theme_void()
}