# Este arquivo tem como objetivo concentrar e organizar todas as funções 
# utilizadas na criação de visualizações (plots) do Dashboard RHC.

#' @export
plot_stacked_bar <- function(reactive_df, x_var, fill_var, fill_values,
                             fill_labels = NULL, fill_name, x_label, y_label,
                             x_breaks = 5, y_breaks  = 5000,
                             scale_fill_type = "manual", pallete = NULL,
                             hover_x, hover_y) {
  count_df <- reactive_df %>% 
    count(.data[[x_var]], .data[[fill_var]])
  
  if (!is.null(fill_labels)) {
    names(fill_values) <- fill_labels
    count_df <- count_df %>% mutate(
      !!fill_var := factor(.data[[fill_var]], levels = names(fill_labels), labels = fill_labels),
    )
  }
  
  p <- count_df %>%
    mutate(
        hover_label = paste0(
        hover_x, ": ", .data[[x_var]], "<br>",
        "Quantidade de Casos: ", n, "<br>",
        hover_y, ": ", .data[[fill_var]]
      )
    ) %>%
    ggplot(aes(x = .data[[x_var]], y = n, fill = as.factor(.data[[fill_var]]),
               text = hover_label)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = x_label, y = y_label) +
    scale_x_continuous(breaks = breaks_width(x_breaks)) +
    scale_y_continuous(breaks = breaks_width(y_breaks))
  if (scale_fill_type == "manual") {
    p <-  p + scale_fill_manual(values = fill_values, name = fill_name)
  } else if (scale_fill_type == "discrete") {
    p <- p + scale_fill_discrete(name = fill_name)
  }
  
  ggplotly(p, tooltip = "text")
}


#' @export
plot_proportional_stacked_bar <- function(reactive_df, x_var, fill_var, fill_name,
                                        x_label, y_label, x_breaks, hover_x,
                                        hover_y) {
  p <- reactive_df %>%
    count(.data[[x_var]], .data[[fill_var]]) %>%
    mutate(
      hover_label = paste0(
        hover_x, ": ", .data[[x_var]], "<br>",
        "Quantidade de Casos: ", n, "<br>",
        hover_y, ": ", .data[[fill_var]]
      )
    ) %>%
    ggplot(aes(x = .data[[x_var]], y = n, fill = as.factor(.data[[fill_var]]),
               text = hover_label)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_discrete(
      name = fill_name
    ) +
    labs(x = x_label, y = y_label) +
    scale_x_continuous(breaks = breaks_width(x_breaks))
  
  ggplotly(p, tooltip = "text")
}


#' @export
plot_percentage_bar <- function(reactive_df, x_var, fill_var, fill_name, x_label,
                                y_label, hover_x, hover_y) {
  p <- reactive_df %>%
    count(.data[[x_var]], .data[[fill_var]]) %>%
    mutate(
      percentage = n / sum(n),
      hover_label = paste0(
        hover_x, ": ", .data[[x_var]], "<br>",
        "Quantidade de Casos: ", n, "<br>",
        hover_y, ": ", .data[[fill_var]]
      )
    ) %>%
    ggplot(aes(x = .data[[x_var]], y = percentage, fill = as.factor(.data[[fill_var]]),
               text = hover_label)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_y_continuous(labels= scales::percent) +
    scale_fill_viridis_d(option = "D", name = fill_name)
    labs(x = x_label, y = y_label)
  
  ggplotly(p, tooltip = "text")
}


#' @export
plot_treemap <- function(reactive_df, x_var, gradient_colors, fill_name,
                         group_threshold = FALSE) {
  count_df <- reactive_df %>%
    count(.data[[x_var]]) %>%
    mutate(percentage = n / sum(n) * 100)

  if (group_threshold) {
    count_df <- count_df %>%
      mutate(
        x_var_grouped = ifelse(percentage < 1, "Outros (< 1%)", .data[[x_var]])
      ) %>%
      group_by(x_var_grouped) %>% summarise(n = sum(n), percentage = sum(percentage)) %>% ungroup()
    
    x_var <- "x_var_grouped"
  }
  
  seq_gradient <- seq(from = 0, to = 1, length.out = length(gradient_colors))
  formatted_gradient_colors <- Map(c, seq_gradient, gradient_colors)

  p <- count_df %>%
    plot_ly(
      type = "treemap",
      labels = ~.data[[x_var]],
      parents = "",
      values = ~n,
      textinfo = "label+percent parent",
      textfont = list(size = 35, color = "white", fontface = "bold"),
      marker = list(colors = ~n, colorscale = formatted_gradient_colors),
      pathbar = "visible",
      hovertemplate = paste0(
        fill_name, ": %{value}",
        "<extra></extra>"
      )
    )
  
  return(p)
}


#' @export
plot_map <- function(reactive_df, pallete, fill_var,
                     id_click = NULL, label, legend_title) {
  current_env <- rlang::current_env()
  fill_color <- rlang::new_formula(
    lhs = NULL,
    rhs = rlang::expr(pallete(!!rlang::sym(fill_var))),
    env = current_env
  )
  fill_var <- as.formula(paste0("~", fill_var))
  if (!is.null(id_click)) {
    id_click <- as.formula(paste0("~", id_click))
  }
  
  leaflet(reactive_df) %>%
    addPolygons(
      fillColor = fill_color,
      fillOpacity = .7,
      color = "white",
      weight = 1,
      label = label,
      layerId = id_click
    ) %>%
    addLegend(pal = pallete, values = fill_var, title = legend_title, 
              position = "bottomright") %>%
    fitBounds(
      lng1 = st_bbox(reactive_df)[["xmin"]],
      lat1 = st_bbox(reactive_df)[["ymin"]],
      lng2 = st_bbox(reactive_df)[["xmax"]],
      lat2 = st_bbox(reactive_df)[["ymax"]]
    )
}


#' @export
plot_pizza <- function(reactive_df, x_var, colors, title, hover_x) {
  count_df <- reactive_df %>%
    count(.data[[x_var]])
  
  p <- count_df  %>%
    plot_ly(labels = ~.data[[x_var]], values = ~n, type = "pie",
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            text = ~paste(n, ' casos'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            showlegend = FALSE,
            hovertemplate = paste0(
              hover_x, ": %{value}",
              "<extra></extra>"
            )) %>%
    layout(title = title, 
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  return(p)
}