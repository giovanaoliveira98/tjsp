baixarpg <- function(
    pesquisa="",
    dadosConsulta="",
    foro="",
    diretorio="."){
  
  n_paginas = as.integer(n_paginas)
  #Construindo nossa URL parseada
  parseada <- structure(
    list(
      scheme="https",
      hostname="esaj.tjsp.jus.br",
      port=NULL,
      path="cpopg/search.do",
      query=list(
        conversationId="",
        cbPesquisa=pesquisa,
        dadosConsulta.valorConsulta=dadosConsulta,
        cdForo=foro),
      params=NULL,
      fragment=NULL,
      username=NULL,
      password=NULL), class="url")
  
  url <- httr::build_url(parseada)
  
  #Encontrando o número de páginas
  n_paginas <- url |>
    httr::GET() |>
    httr::content() |>
    xml2::xml_find_first("//*[@id='quantidadeProcessosNaPagina']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_extract("(?<=até\\s)(\\d+)") |>
    as.integer()
  
  #Escrevendo o arquivo
  purrr::walk(1:n_paginas, purrr::possibly(~{
    
    hora <- Sys.time() %>% 
      stringr::str_replace_all("\\D","_")
    
    arquivo <- diretorio %>% 
      file.path(glue::glue("{hora}_pagina_{.x}.html"))
    
    pstr <- toString(.x)
    
    parseada <- structure(
      list(
        scheme="https",
        hostname="esaj.tjsp.jus.br",
        port=NULL,
        path="cpopg/trocarPagina.do",
        query=list(
          paginaConsulta=pstr,
          conversationId="",
          cbPesquisa=pesquisa,
          dadosConsulta.valorConsulta=dadosConsulta,
          cdForo=foro),
        params=NULL,
        fragment=NULL,
        username=NULL,
        password=NULL), class="url")
    
    url <- httr::build_url(parseada)
    
    httr::GET(url, httr::write_disk(arquivo, overwrite = FALSE))
  }, otherwise=NULL))
}