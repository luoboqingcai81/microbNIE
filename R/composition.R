#' Title
#'
#' @param combined_tax tibble
#' @param Num integer
#'
#' @return tibble
#' @export
#' @importFrom rlang :=
#'
#' @examples
#' combined_Table <- microbNIE:::combined_Table
#' results_ggtable <- ggtable_composition(combined_Table, 10)
ggtable_composition <- function(combined_tax, Num){
  tax_name <- rlang::sym(colnames(combined_Table)[1])
  Sum <- NULL
  ggcomposition <- combined_Table %>%
    dplyr::mutate(Sum = purrr::pmap(.[,-1], ~ sum(c(...)))) %>%
    dplyr::mutate(Sum = as.numeric(Sum)) %>%
    dplyr::filter(!(!!tax_name == "other")) %>%
    dplyr::arrange(dplyr::desc(Sum)) %>%
    dplyr::slice(1:Num) %>%
    dplyr::select(-Sum)
  Col_other <- dplyr::tibble(!!tax_name := "others") %>%
    dplyr::bind_cols(dplyr::summarise(ggcomposition,dplyr::across(where(is.numeric),~ 1-sum(.x))))
  ggcomposition2 <- ggcomposition %>%
    dplyr::rows_insert(Col_other)
  return(ggcomposition2)
}




#' Title
#'
#' @param ggtable_comp tibble
#' @return ggobject
#' @export
#' @importFrom rlang :=
#'
#' @examples
#' ggtable_Comp <- microbNIE:::ggtable_Comp
#' ggresults <- ggplot_composition(ggtable_Comp)
ggplot_composition <- function(ggtable_comp){
  row_Num <- nrow(ggtable_comp)
  Colors_raw <- c( "#3C5488B2","#00A087B2",
                   "#F39B7FB2","#91D1C2B2",
                   "#8491B4B2", "#DC0000B2",
                   "#e0f3f8", "#abd9e9", "#74add1",
                   "#4575b4", "#313695","#56B4E9",
                   "#F0E442", "#009E73","#de77ae","#6a51a3")
  Colors <- Colors_raw[1:row_Num]
  Tax_name <- colnames(ggtable_comp)[1]
  tax_name <- rlang::sym(colnames(ggtable_comp)[1])
  c_ggtable_comp <- ggtable_comp %>%
    dplyr::mutate(!!tax_name := forcats::as_factor(!!tax_name)) %>%
    tidyr::pivot_longer(-!!tax_name, names_to = "Group")
  Group <- NULL; value <- NULL
  gg <- ggplot2::ggplot(c_ggtable_comp,ggplot2::aes(Group, value, fill = !!tax_name))+
    ggplot2::geom_col(position = "stack", width = 0.8)+
    ggplot2::guides(fill=ggplot2::guide_legend(reverse=F))+
    ggplot2::scale_y_continuous(expand=c(0,0))+
    ggplot2::scale_fill_manual(name= Tax_name,values = Colors )+
    ggplot2::labs(x = "sample", y = 'Relative Abundance(%)')+
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_text(size = 15,vjust = 0.5,hjust = 0.5),
          axis.title.y = ggplot2::element_text(size = 12,vjust = 0.5,hjust = 0.5),
          axis.text.x = ggplot2::element_text(size=10,vjust = 0.5,hjust=0.5),
          axis.text.y = ggplot2::element_text(size=9,vjust = 0.5,hjust=0.5),
          panel.background = ggplot2::element_rect( fill = 'transparent'),
          strip.background = ggplot2::element_rect( fill='transparent',size=1.5, linetype="solid"))
  return(gg)
}













