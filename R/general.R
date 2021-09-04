#' Title read raw otu table from biom-convert in qiime1
#'
#' @param Path character
#' @param rename logic
#'
#' @return tibble
#' @export
#'
#' @examples
#' PATH <- system.file("extdata","otu_example.txt", package = "microbNIE", mustWork = TRUE)
#' example_read <- read_qiime1(PATH, rename = TRUE)
read_qiime1 <- function(Path, rename = FALSE){
  `#OTU ID` <- NULL; OTU_ID <- NULL; taxonomy <- NULL
  otu <- Path %>%
    readr::read_tsv(skip = 1) %>%
    dplyr::rename(OTU_ID = `#OTU ID`) %>%
    dplyr::mutate(OTU_ID = dplyr::case_when(stringr::str_detect(OTU_ID, "^OTU") ~ as.character(OTU_ID), TRUE ~ paste0("OTU", OTU_ID)))
  new_tax <- otu %>%
    dplyr::select(taxonomy) %>%
    tidyr::separate(taxonomy,sep = ";",into = c("kindom","phylum","class","order","family","genus","species"))%>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, "") )) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_trim(.x, side = "left"))) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_sub(.x, 4,-1))) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::case_when(.x == "" ~ "other", TRUE ~ .x) )) %>%
    tidyr::unite("taxonomy", dplyr::everything(), sep = ";", remove = TRUE)
  if(rename == TRUE){
    OTU <- otu %>%
      dplyr::rename_with(~ paste0("S", .x), where(is.numeric)) %>%
      dplyr::select(-taxonomy) %>%
      dplyr::bind_cols(new_tax)
    return(OTU)
  }
  if(rename == FALSE){
    OTU <- otu %>%
      dplyr::select(-taxonomy) %>%
      dplyr::bind_cols(new_tax)
    return(OTU)
  }
}




