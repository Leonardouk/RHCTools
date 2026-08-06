# Este arquivo tem como objetivo concentrar e organizar todas as funções 
# utilizadas na criação de visualizações (plots) do Dashboard RHC.

# ==============================================================================


#' @title
#' Plot de um gráfico de barras empilhadas para valores totais
#' 
#' @description
#' 'plot_stacked_bar' retorna um plot de gráifco de barras empilhadas interativo
#' com base nos argumentos passados.
#' 
#' @details
#' Esta função é pensada para funcionar em sua maioria apenas com as definições
#' de seus argumentos, mas é possível passar argumentos que foram previamente
#' manipulados para que estes se encaixem no contexto de uso de sua necessidade.
#' 
#' @param reactive_df Um Dataframe que foi previamente manipulado para interagir
#' com filtragem de dados dinâmica.
#' @param x_var Uma String que consiste do nome de uma coluna de reactive_df, 
#' seja ela natural dele ou criada por manipulação posterior.Este argumento 
#' define a variável de eixo X do plot.
#' @param fill_var Uma string que compartilha das características de x_var. Este
#' argumento define a variável de eixo y do plot.
#' @param fill_values Um mapa que relaciona um valor categórico, que compõem
#' fill_var, com uma cor que representa o valor no plot.
#' @param fill_labels Um mapa que relaciona um valor categórico com uma String,
#' a qual nomeia e se apresenta no plot.
#' @param fill_name Uma String que define o nome da legenda das categorias do plot.
#' @param x_label,y_label Strings que definem os nomes de eixo x e y do plot.
#' @param x_breaks,y_breaks Valores númericos que definem o intervalo númerico
#' da descrição dos valores dos eixos x e y.
#' @param scale_fill_type Uma String que descreve o tipo de preenchimento de cor
#' realizado pela função. Pode ser "manual", usando de fill_values e fill_name 
#' para definir as cores do plot, ou "discrete", o qual define as cores a partir
#' de uma paleta de cores pré-definida ou criada previamente.
#' @param pallete Um vetor de de Strings, que nomeiam cores, ou uma String que
#' define uma paleta já pré-definida.
#' @param hover_x,hover_y Duas Strings que definem como os valores de x_var e
#' fill_var são descritos nas interações de sobreposição.
#' 
#' @export
#' 
#' @examples
#' plot_stacked_bar(
#'   reactive_df = df,
#'   x_var = "ANODIAG",
#'   fill_var = "CATEATEND",
#'   fill_values = c("1" = "yellow", "2" = "lightblue", "3" = "lightgreen", 
#'                   "9" = "orange"),
#'   fill_labels = c("1" = "Convênio", "2" = "SUS", "3" = "Particular", 
#'                   "9" = "Sem Informação"),
#'   fill_name = "Categorias de Atendimento",
#'   x_label = "Ano de diagnóstico", y_label = "Casos registrados por ano",
#'   x_breaks = 5, y_breaks = 5000,
#'   hover_x = "Ano de Diagnóstico",
#'   hover_y = "Categoria de Atendimento"
#' )
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


# ==============================================================================


#' @title
#' Plot de um gráfico de barras empilhadas para valores Proporcionais
#' 
#' @description
#' 'plot_proportional_stacked_bar' retorna um plot de gráifco de barras empilhadas 
#' interativo com base nos argumentos passados. Diferencia-se por representar as
#' quantidades de dados proporcionalmente ao valor total.
#' 
#' @details
#' Esta função é pensada para funcionar em sua maioria apenas com as definições
#' de seus argumentos, mas é possível passar argumentos que foram previamente
#' manipulados para que estes se encaixem no contexto de uso de sua necessidade.
#' 
#' @param reactive_df Um Dataframe que foi previamente manipulado para interagir
#' com filtragem de dados dinâmica.
#' @param x_var Uma String que consiste do nome de uma coluna de reactive_df, 
#' seja ela natural dele ou criada por manipulação posterior.Este argumento 
#' define a variável de eixo X do plot.
#' @param fill_var Uma string que compartilha das características de x_var. Este
#' argumento define a variável de eixo y do plot.
#' @param fill_name Uma String que define o nome da legenda das categorias do plot.
#' @param x_label,y_label Strings que definem os nomes de eixo x e y do plot.
#' @param x_breaks Valor númerico que define o intervalo númerico da descrição 
#' do valor do eixo x.
#' @param hover_x,hover_y Duas Strings que definem como os valores de x_var e
#' fill_var são descritos nas interações de sobreposição.
#' 
#' @export
#' 
#' @examples
#' plot_proportional_stacked_bar(
#'   reactive_df = df,
#'   x_var = "ANODIAG",
#'   fill_var = "CATEATEND",
#'   fill_name = "Categorias de Atendimento",
#'   x_label = "Ano de diagnóstico",
#'   y_label = "Proporção total de casos registrados por ano",
#'   x_breaks = 5,
#'   hover_x = "Ano de Diagnóstico",
#'   hover_y = "Categoria de Atendimento"
#' )
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


# ==============================================================================


#' @title
#' Plot de um gráfico de barras empilhadas para valores de porcentagem
#' 
#' @description
#' 'plot_percentage_bar' retorna um plot de gráifco de barras interativo com base 
#' nos argumentos passados. Diferencia-se por representar as quantidades de dados 
#' proporcionalmente ao valor total em porcentagem.
#' 
#' @details
#' Esta função é pensada para funcionar em sua maioria apenas com as definições
#' de seus argumentos, mas é possível passar argumentos que foram previamente
#' manipulados para que estes se encaixem no contexto de uso de sua necessidade.
#' 
#' @param reactive_df Um Dataframe que foi previamente manipulado para interagir
#' com filtragem de dados dinâmica.
#' @param x_var Uma String que consiste do nome de uma coluna de reactive_df, 
#' seja ela natural dele ou criada por manipulação posterior.Este argumento 
#' define a variável de eixo X do plot.
#' @param fill_var Uma string que compartilha das características de x_var. Este
#' argumento define a variável de eixo y do plot.
#' @param fill_name Uma String que define o nome da legenda das categorias do plot.
#' @param x_label,y_label Strings que definem os nomes de eixo x e y do plot.
#' @param hover_x,hover_y Duas Strings que definem como os valores de x_var e
#' fill_var são descritos nas interações de sobreposição.
#' 
#' @export
#' 
#' @examples
#' plot_percentage_bar(
#'   reactive_df = df,
#'   x_var = "ECGRUP",
#'   fill_var = "EC",
#'   fill_name = "Estadio Clínico",
#'   x_label = "Grupos de Estadiamento Clínico",
#'   y_label = "Proporção de Casos Registrados",
#'   hover_x = "Grupo de Estadiamento Clínico", 
#'   hover_y = "Estadiamento Clínico"
#' )
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


# ==============================================================================


#' @title
#' Plot de um gráfico de mapa de árvore para valores categóricos
#' 
#' @description
#' 'plot_treemap' retorna um plot de gráifco de mapa de árvore com base nos 
#' argumentos passados.
#' 
#' @details
#' Esta função é pensada para funcionar em sua maioria apenas com as definições
#' de seus argumentos, mas é possível passar argumentos que foram previamente
#' manipulados para que estes se encaixem no contexto de uso de sua necessidade.
#' 
#' @param reactive_df Um Dataframe que foi previamente manipulado para interagir
#' com filtragem de dados dinâmica.
#' @param x_var Uma String que consiste do nome de uma coluna de reactive_df, 
#' seja ela natural dele ou criada por manipulação posterior.
#' @param gradient_colors Um vetor de Strings que definem cores para formar um
#' gradiente para o plot.
#' @param fill_name Uma String que define o nome da legenda da interação de 
#' sobreposição do plot.
#' @param group_threshhold Um booleano que define se categorias que possuem 
#' valores que representam menos de 1% do valor total de x_var vão ser agrupadas
#' em uma categoria ou vão ser categorias independentes.
#' 
#' @export
#' 
#' @examples
#' plot_treemap(
#'   reactive_df = plot_df(),
#'   x_var = "DSCINST",
#'   gradient_colors = c("#91e0ff", "#57c785","#005991"),
#'   fill_name = "Qtd de Casos",
#'   group_threshold = TRUE
#' )
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
  
  count_df <- count_df %>%
    mutate(custom_text = sprintf("%.1f%%", percentage))
  
  seq_gradient <- seq(from = 0, to = 1, length.out = length(gradient_colors))
  formatted_gradient_colors <- Map(c, seq_gradient, gradient_colors)

  p <- count_df %>%
    plot_ly(
      type = "treemap",
      labels = ~.data[[x_var]],
      parents = "",
      values = ~n,
      text = ~custom_text,
      textinfo = "label+text",
      textfont = list(size = 35, color = "white", fontface = "bold"),
      marker = list(colors = ~n, colorscale = formatted_gradient_colors),
      pathbar = "visible",
      hovertemplate = paste0(
        fill_name, ": %{value}",
        "<extra></extra>"
      ),
      branchvalues = "total"
    )
  
  return(p)
}


# ==============================================================================


#' @title
#' Plot de um gráfico de mapa para valores categóricos
#' 
#' @description
#' 'plot_map' retorna um plot de gráifco de mapa com base nos argumentos passados.
#' 
#' @details
#' Esta função é pensada para funcionar em sua maioria apenas com as definições
#' de seus argumentos, mas é possível passar argumentos que foram previamente
#' manipulados para que estes se encaixem no contexto de uso de sua necessidade.
#' 
#' @param reactive_df Um Dataframe que foi previamente manipulado para interagir
#' com filtragem de dados dinâmica.
#' @param pallete Um vetor de Strings que nomeiam cores ou uma String que
#' nomeia uma paleta de cores pré-definida 
#' @param fill_var Uma String que consiste do nome de uma coluna de reactive_df, 
#' seja ela natural dele ou criada por manipulação posterior.
#' @param id_click Uma String que consiste do nome de uma coluna de reactive_df.
#' Ela é utilizada para observar se alguma informação relevante foi interagida
#' pelo clique do mouse do usuário, guardando essa informação a partir de seu valor
#' na coluna.
#' @param label Uma String que define o texto que aparece para o usuário
#' quando ele passa o mouse sobre uma uma informação relevante do mapa.
#' @param legend_title Uma String que define o título da legenda das informações
#' do mapa..
#' 
#' @export
#' 
#' @examples
#' plot_map(
#'   reactive_df = map_df,
#'   pallete = pal,
#'   fill_var = "RRAS",
#'   id_click = "CD_MUN",
#'   label = ~paste0("RRAS: ", RRAS, " | Casos: ", num_cases),
#'   legend_title = "RRAS"
#' )
plot_map <- function(reactive_df, pallete, fill_var, group = NULL,
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
      group = group,
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


# ==============================================================================


#' @title
#' Plot de um gráfico de pizza para valores categóricos
#' 
#' @description
#' 'plot_map' retorna um plot de gráifco de pizza com base nos argumentos passados.
#' 
#' @details
#' Esta função é pensada para funcionar em sua maioria apenas com as definições
#' de seus argumentos, mas é possível passar argumentos que foram previamente
#' manipulados para que estes se encaixem no contexto de uso de sua necessidade.
#' 
#' @param reactive_df Um Dataframe que foi previamente manipulado para interagir
#' com filtragem de dados dinâmica. 
#' @param x_var Uma String que consiste do nome de uma coluna de reactive_df, 
#' seja ela natural dele ou criada por manipulação posterior.
#' @param colors Um vetor de Strings que nomeiam cores ou uma String que
#' nomeia uma paleta de cores pré-definida
#' @param title Uma String que define o título da visualização.
#' @param hover_x Uma String que define como os valores de x_var são 
#' descritos nas interações de sobreposição.
#' 
#' @export
#' 
#' @examples
#' colors <- c("lightblue", "lightgreen")
#' plot_pizza(
#'   reactive_df = diagprev_df,
#'   x_var = "DIAGPREV",
#'   colors = colors,
#'   title = "Quantidade de Casos Registrados por Diagnóstico Prévio",
#'   hover_x = "Qtd de Casos"
#' )
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