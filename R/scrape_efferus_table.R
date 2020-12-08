library(tidyverse)

plants <- readxl::read_xlsx(here::here("data/skogshage_planter.xlsx"))
plants <- dplyr::filter(plants, !is.na(name))

get_table <- function(url){
  if(is.na(url)){ return(NULL) }

  file <- xml2::read_html(url)
  tables <- rvest::html_nodes(file, "table")
  
  ind <- which(sapply('width="328" height="453"', grepl, tables))
  table1 <- rvest::html_table(tables[ind], fill = TRUE)
  
  tab <- as.data.frame(table1)
  if(nrow(tab) == 0){ return(NULL) }
  
  tab <- tab[1:15, 1:2]
  names(tab) <- c("key", "value")
  tab <- dplyr::as_tibble(tab)

  bilder <- rvest::html_nodes(tables, "img")
  bilder <- rvest::html_attr(bilder, "src")
  bilder <- bilder[!grepl("info|hg.jpg|fb.jpg|paypal", bilder)]
  bilder <- bilder[!grepl("efferus|Home|soppsort.jpg", bilder)]
  bilder <- bilder[!grepl("Oppskrifter|subtop|subbottom", bilder)]
  bilder <- bilder[!grepl("skogshop.jpg|Frørød.jpg", bilder)]

  ind <- grep("plantebilder", bilder)
  if(length(ind)){
    arts_bilde <- bilder[ind]
    bilder <- bilder[ind*-1]
  }
  
  .get_img <- function(type, bilder){

    type <- match.arg(type, 
              c("herdighet", "spiselig", 
                "medisinsk", "sol", "jordsmonn"))
    
    string <- dplyr::case_when(
      type == "herdighet" ~"plantedatabase/herdighet/H.*.jpg",
      type == "spiselig" ~ "plantedatabase/herdighet/Spiselig.*.jpg",
      type == "medisinsk" ~ "plantedatabase/herdighet/medisinsk.*.jpg",
      type == "sol" ~ "bilder/.*.gif",
      type == "jordsmonn" ~ ""
    )

    position <- switch(
      type,
      "herdighet" = grep("Herdighet", unlist(tab[,1])),
      "spiselig" = grep("Spiselig", unlist(tab[,1])),
      "medisinsk" = grep("Medisinsk", unlist(tab[,1])),
      "sol" = grep("Skygge", unlist(tab[,1])),
      "jordsmonn" = grep("Jordsmonn", unlist(tab[,1]))
    )

    x <- grep(string, bilder)

    if(length(x) > 0){
      content <- switch(
        type,
        "sol" = gsub("bilder/|.gif", "", bilder[x]),
        "jordsmonn" = paste0(gsub("bilder/|.jpg", "", bilder), collapse = ", "),
        "herdighet" = as.character(readr::parse_number(bilder[x])),
        "medisinsk" = as.character(readr::parse_number(bilder[x])),
        "spiselig" = as.character(readr::parse_number(bilder[x]))
      )
      bilder <<- bilder[x*-1]  
    } else {
      content <- NA_character_
    }
    
    if(length(position) > 1){
      tab[position,2] <<- content
    }else{
      tab[position,2] <<- paste0(content, collapse="-")
    }
  }

  k <- .get_img("herdighet", bilder)
  k <- .get_img("spiselig", bilder)
  k <- .get_img("medisinsk", bilder)
  k <- .get_img("sol", bilder)
  k <- .get_img("jordsmonn", bilder)

  dt <- as.data.frame(do.call(cbind, as.list(tab$value)))
  names(dt) <- tab$key
  dt <- janitor::clean_names(dt)
  if(exists("arts_bilde")) dt$img <- arts_bilde
  dt$url <- url
  dplyr::as_tibble(dt)
}

data <- lapply(plants$url, get_table) %>% 
  bind_rows() %>% 
  left_join(plants, by="url") %>% 
  select(-samisk_navn, -andre_navn, -kinagresslok, 
         -antall, -formering, -name)


write.table(data, here::here("data/plant.tsv"), 
            sep = "\t", quote = FALSE, row.names = FALSE)
