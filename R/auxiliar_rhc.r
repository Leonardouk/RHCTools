#' Lê um arquivo Markdown e o transforma em uma string HTML.
#'
#' Esta função lê um arquivo Markdown como texto e o passa para a função
#' markdown(), permitindo inseri-lo posteriormente em páginas.
#'
#' @param path Caminho para o arquivo Markdown.
#'
#' @return O conteúdo do arquivo processado pela função markdown().
#'
#' @export
read_markdown <- function(path) {
  shiny::markdown(paste(readLines(path, encoding = "UTF-8")))
}