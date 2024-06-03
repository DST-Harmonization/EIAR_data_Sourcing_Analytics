
########################################################
#required packages
########################################################
packages_required <- c("tidyverse", "tidyr")
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])
}
invisible(lapply(packages_required, library, character.only = TRUE))

########################################################
# get the agronomic efficiency for N and P
########################################################
#' a function to convert the wide format to long and compute the NUE and PUE
#' @param lodData the data frame for one location with the x, y and the yield estimate for N and P combinations
#' @param col1 the name of the first column with yield.N.P format for min N and min P
#' @param col2 the name of the last column with yield.N.P format for max N and max P
#' @return a data frame within x, y, prob indicating the dominant probability of forecast scenario, N, P, NUE and PUE
#' @example calculate_NPUE(lodData= data.frame("x"=34, "y"=9, "yield.0.0" = 2031, "yield.10.0"=2115, "yield.20.0"=2190,
#' "yield.0.5"=2026, "yield.10.5"=2100, "yield.20.5"=2300, 'prob'=1, 'location'="34_9"), col1="yield.0.0", col2="yield.20.5")

calculate_NPUE <- function(lodData, col1, col2){
  lodData_gather <- lodData |>
    tidyr::gather(rate, yield, col1:col2) 
  lodData_gather$n <- as.numeric(sub("^.*?[.](\\w+).*","\\1", lodData_gather$rate))
  lodData_gather$p <- as.numeric(sub(".*\\.(\\d+).*", "\\1", lodData_gather$rate))
  lodData_gather <- lodData_gather %>% 
    dplyr::select(-rate) 
  colnames(lodData_gather) <- c('x', 'y', 'prob', 'location' ,'yield','n' , 'p')
  
  ## Nitrogen use efficiency 
  unq_p <- unique(lodData_gather$p)
  nuseeff_df <- data.frame()
  for(ps in unique(unq_p)){
    nfert_row <- lodData_gather |> dplyr::filter(p == ps) |> unique()
    nueloc <- nfert_row %>% 
      dplyr::arrange(n) |> 
      dplyr::mutate(yld_diff = (yield - yield[1]),
                    nue = yld_diff/ (n - n[1])) |>
      dplyr::select(c(x, y, prob, yield, n,  p, nue))
    nuseeff_df <- rbind(nuseeff_df, nueloc)
  }
  
  #p use efficiency
  unq_n <- unique(lodData_gather$n)
  puseeff_df <- data.frame()
  for(ns in unique(unq_n)){
    pfert_row <- lodData_gather |> dplyr::filter(n == ns) |> unique()
    pueloc <- pfert_row |> 
      dplyr::arrange(p) |> 
      dplyr::mutate(yld_diff = (yield - yield[1]),
                    pue = yld_diff/ (p - p[1])) |>
      dplyr::select(c(x, y, prob, yield, n,  p, pue))
    puseeff_df <- rbind(puseeff_df, pueloc)
  }
  NUE_PUE_loc <- merge(nuseeff_df, puseeff_df, by=c( "x", "y", "prob", "yield", "n","p"))
  NUE_PUE_loc <- NUE_PUE_loc[NUE_PUE_loc$n>0 & NUE_PUE_loc$p>0, ]
  # NUE_PUE <- rbind(NUE_PUE, NUE_PUE_loc)
  # }
  return(NUE_PUE_loc)
}

