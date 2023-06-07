library(dplyr)

#Criar um link

atualizar_link <- function(df){
  links <- unlist(df$link)
  
  criar <- function(link){
    link <- paste0("https://esaj.tjsp.jus.br", link)
  }
  
  links <- purrr::map(links, criar) %>% unlist()
  
  df$link <- links
}

df$link <- writexl::xl_hyperlink(unlist(df$link))

df4$links <- atualizar_link(df4)
