#Baixar HTMLs da coluna link

#links <- unlist(df$link)

diretorio = "data-raw3"

purrr::walk(links, purrr::possibly(~{
  
  hora <-  Sys.time() |>
    stringr::str_replace_all("\\D","_")
  
  codigoProcesso <- stringr::str_extract(t,"(?<=processo.codigo=)([^&]*)")
  
  arquivo <- diretorio |> file.path(glue::glue("{codigoProcesso}_{hora}.html"))
  
  httr::GET(url, httr::write_disk(arquivo, overwrite = TRUE))
  
}, otherwise = NULL))
