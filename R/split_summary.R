#' Title
#'
#' @param otu_table tibble
#' @param level level
#'
#' @return tibble
#' @export
#'
#' @examples
#' data(metadata)
#' data(otu)
#' results_split_tax <- split_taxonomy(otu, class)
split_taxonomy <- function(otu_table, level){
  taxonomy <- NULL
  Otu_table <- otu_table %>%
    tidyr::separate(taxonomy, sep = ";",
             into = c("kindom","phylum","class","order","family","genus","species")) %>%
    dplyr::select(class, where(is.numeric))%>%
    dplyr::group_by(class) %>%
    dplyr::summarise(dplyr::across(where(is.numeric),~ sum(.x, na.rm = TRUE)))%>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ .x/sum(.x, na.rm = TRUE)))
  return(Otu_table)
}








#' Title
#'
#' @param taxonomy_table tibble
#' @param metadata tibble
#' @param group group
#'
#' @return tibble
#' @export
#'
#' @examples
#' data(metadata)
#' Class_otu <- microbNIE:::Class_otu
#' results_comByGroup <- combineTaxonomy_ByGroup(Class_otu, metadata, site)
#'
combineTaxonomy_ByGroup <- function(taxonomy_table, metadata, group){
  group <- rlang::ensym(group)
  Level <- colnames(taxonomy_table)[1]
  value <- NULL
  T_tax <- taxonomy_table %>%
    tidyr::pivot_longer(-!!Level,names_to = "sample")%>%
    tidyr::pivot_wider(names_from = !!Level, values_from = value) %>%
    dplyr::left_join(metadata, by="sample") %>%
    dplyr::group_by(!!group) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), mean)) %>%
    tidyr::pivot_longer(-!!group, names_to = Level) %>%
    tidyr::pivot_wider(names_from = !!group, values_from = value)
  return(T_tax)
}






