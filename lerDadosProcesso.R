lerDadosProcesso <- function(diretorio="."){
  
  library(dplyr)
  
  arquivos <- list.files(diretorio, full.names = TRUE)
  
  pb <- progress::progress_bar$new(total=length(arquivos))
  
  func <- function(doct){
    
    hora_coleta <- doct %>% 
      stringr::str_extract("\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}") %>% 
      lubridate::ymd_hms(tz="America/Sao_Paulo")
    
    doct <- doct %>%
      xml2::read_html(encoding = "UTF-8")
    
    processo <- doct %>% 
      xml2::xml_find_all("//*[@id='numeroProcesso']") %>%
      xml2::xml_text(trim=TRUE)%>% 
      {if (length(.) == 0) "NA" else .}
    
    classe <- doct %>%
      xml2::xml_find_all("//*[@id='classeProcesso']") %>% 
      xml2::xml_text(trim=TRUE)%>% 
      {if (length(.) == 0) "NA" else .}
    
    foro <- doct %>%
      xml2::xml_find_all("//*[@id='foroProcesso']") %>% 
      xml2::xml_text(trim=TRUE)%>% 
      {if (length(.) == 0) "NA" else .}
    
    juiz <- doct %>% 
      xml2::xml_find_all("//*[@id='juizProcesso']") %>% 
      xml2::xml_text(trim=TRUE) %>% 
      {if (length(.) == 0) "NA" else .}
    
    areaProcesso <- doct %>% 
      xml2::xml_find_all("//*[@id='areaProcesso']") %>% 
      xml2::xml_text(trim=TRUE)%>% 
      {if (length(.) == 0) "NA" else .}
    
    valor <- doct %>% 
      xml2::xml_find_all("//*[@id='valorAcaoProcesso']") %>% 
      xml2::xml_text(trim=TRUE) %>% 
      stringr::str_extract("(?=\\d).*$")%>% 
      {if (length(.) == 0) "NA" else .}
    
    partes <- doct %>% 
      xml2::xml_find_all("*//td[@class='nomeParteEAdvogado']") %>% 
      xml2::xml_text(trim=TRUE) %>% 
      purrr::map(~ stringr::str_squish(.x)) %>% 
      unlist() %>% 
      unique() %>% 
      paste(collapse="/")%>% 
      {if (length(.) == 0) "NA" else .}
    
    advogados <- partes %>%
      stringr::str_extract("Advogad([^/]*)") %>% 
      unique() %>% 
      stringr::str_extract("(?<=: ).*") %>% 
      paste(collapse="/")%>% 
      {if (length(.) == 0) "NA" else .}
    
    dataUltimaMovimentacao <- doct %>% 
      xml2::xml_find_all("//tbody[@id='tabelaUltimasMovimentacoes']/tr[1]/td[1]") %>% 
      xml2::xml_text(trim=TRUE)%>% 
      {if (length(.) == 0) "NA" else .}
    
    ultimaMovimentacao <- doct %>% 
      xml2::xml_find_all("//*[@id='tabelaUltimasMovimentacoes']/tr[1]/td[3]") %>% 
      xml2::xml_text(trim=TRUE) %>% 
      stringr::str_squish()%>% 
      {if (length(.) == 0) "NA" else .}
    
    peticoes <- doct %>% 
      xml2::xml_find_all("//h2[text()='Petições diversas']/../following-sibling::table[1]/tbody[1]//*/text()")%>% 
      xml2::xml_text(trim=TRUE) %>% 
      paste(collapse = " ")%>% 
      {if (length(.) == 0) "NA" else .}
    
    
    tibble::tibble(processo=processo, foro=foro, classe=classe, juiz=juiz, areaProcesso=areaProcesso,
                   valor=valor, partes=partes, advogados=advogados,dataUltimaMovimentacao=dataUltimaMovimentacao,
                   ultimaMovimentacao=ultimaMovimentacao, peticoes=peticoes) %>% 
      tidyr::separate(advogados, into = c("advogado1", "advogado2", "advogado3","advogado4", "advogado5"),
                      sep="\\sAdvogad.:\\s", extra="merge")
  }
  
  df <- purrr::map_dfr(arquivos, func)
}


df3000<-lerConsultaProcessual(diretorio="data-raw2")