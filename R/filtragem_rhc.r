make_card_title <- function(title_text, filters_vector) {
  if (length(filters_vector) > 0) {
    filters_string <- paste(filters_vector, collapse = ", ")
    paste0(title_text, " (", filters_string, ")")
  }
  else {
    title_text
  }
}

#' @export
sidebar_filter <- function(df, input, output, input_vector, card_title_vector) {
  # Conferência de erros
  if (!is.character(input_vector) || !is.character(card_title_vector)) {
    stop("Erro: input_vector/card_title_vector devem ser vetores de strings!")
  }
  if (length(card_title_vector) < 1) {
    stop("Erro: card_title_vector deve apresentar pelo menos um item!")
  }
  # Lógica da função
  filter_vector <- character()  
  for (item in input_vector) {
    item_val <- input[[item]]
    if (item == "ANODIAG") {
      if (!identical(item_val, c(2000, max(df[[item]])))) {
        filter_vector <- c(filter_vector, paste(item_val[1], "-", 
                                                item_val[2]))
      }
    }
    else {
      if (length(item_val) > 1) {
        filter_vector <- c(filter_vector, item_val)
      }
      else {
        if (!is.null(item_val) && item_val != "") {
          filter_vector <- c(filter_vector, item_val)
        }
      }
    }
  }
  
  count <- 1
  for (item in card_title_vector) {
    card_out_str <- paste0("card_title", count)
    output[[card_out_str]] <- renderText(make_card_title(item, filter_vector))
    count <- count + 1
  }
  
  return(output)
}