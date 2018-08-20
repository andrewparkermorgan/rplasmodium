# plots.R

#' Sensible continuous horizontal axis for genomic coordinates
#' 
#' @export
scale_x_genome <- function(..., scale = 1e3) {
	
	scalers <- c("KB" = 1e3, "KBP" = 1e3, "MB" = 1e6, "MBP" = 1e6)
	m <- match(toupper(scale), names(scalers))
	if (is.numeric(scale))
		s <- scale[1]
	else if (!is.na(m))
		s <- scalers[m]
	else
		s <- 1e3
	
	ggplot2::scale_x_continuous(..., labels = function(x) x/s)
	
}