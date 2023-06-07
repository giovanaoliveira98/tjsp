#Criando função ler primeiro grau

lerpg <- function(diretorio="."){
  
  arquivos <- list.files(diretorio, full.names = TRUE)
  
  pb <- progress::progress_bar$new(total=length(arquivos))
  
  func <- function(doct){
    
    hora_coleta <- doct %>% 
      stringr::str_extract("\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}") %>% 
      lubridate::ymd_hms(tz="America/Sao_Paulo")
    
    doct <- doct %>%
      xml2::read_html(encoding = "UTF-8")
    
    foro <- doct %>% 
      xml2::xml_find_all("//*[@id='listagemDeProcessos']/h2") %>% 
      xml2::xml_text(trim=TRUE)
    
    processo <- doct %>% 
      xml2::xml_find_all("//a[@class='linkProcesso']") %>% 
      xml2::xml_text(trim=TRUE)
    
    link <- doct %>% 
      xml2::xml_find_all("//a[@class='linkProcesso']/@href") %>% 
      xml2::xml_text(trim=TRUE)
    
    datavara <- doct %>% 
      xml2::xml_find_all("//div[@class='dataLocalDistribuicaoProcesso']") %>% 
      xml2::xml_text(trim=TRUE)
    
    assunto <- doct %>%
      xml2::xml_find_all("//div[@class='assuntoPrincipalProcesso']") %>% 
      xml2::xml_text(trim=TRUE)
    
    tibble::tibble(processo=processo, assunto=assunto, datavara=datavara,
                   link=link, hora_coleta=hora_coleta)
  }
  
  df <- purrr::map_dfr(arquivos, func)
}
