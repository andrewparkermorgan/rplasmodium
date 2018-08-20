# chromosomes.R

#' Sanitize chromosome names
#' 
#' Convert a vector of chromosome names in some sensible format to "nice" names in karyotype order.
#' 
#' @export
factor_chrom <- function(x, genome = "Pf3D7", ...) {
	
	if (genome != "Pf3D7") {
		warning("Genomes other than Pf3D7 not yet supported.")
	}
	x <- as.character(x)
	x <- gsub("^Pf(?:3D7)*_(.+)_.+", "\\1", x, perl = TRUE)
	x <- gsub("^Pf_(.+)", "\\1", x, perl = TRUE)
	x <- gsub("^0", "", x)
	x <- gsub("^A.+", "A", x)
	x <- gsub("^M.+", "M", x)
	x <- factor(x, levels = c(1:14, "A", "M"))
	class(x) <- c(class(x), "chrom")
	attr(x, "genome") <- genome
	return(x)
	
}

#' Check if chromosome names are in nuclear genome
#' 
#' @export
is_nuclear <- function(x, species = c("pf","pv"), ...) {
	
	species <- match.arg(species)
	
	if (!inherits(x, "chrom"))
		x <- factor_chrom(x)
	
	if (species %in% c("pf","pv"))
		x %in% 1:14
	else
		stop("Speices other than P. falciparum and P. vivax not yet supported.")
	
}

#' Get named vector of chromosome sizes in specified genome assembly
#'
#' @export
chromsizes <- function(genome = "Pf3D7", clean_names = FALSE, ...) {
	
	genome <- match.arg(toupper(genome), c("3D7","PF3D7"))
	if (genome %in% c("PF3D7","3D7")) {
		chroms <- c("Pf3D7_01_v3"     = 640851,
					"Pf3D7_02_v3"     = 947102,
					"Pf3D7_03_v3"     = 1067971,
					"Pf3D7_04_v3"     = 1200490,
					"Pf3D7_05_v3"     = 1343557,
					"Pf3D7_06_v3"     = 1418242,
					"Pf3D7_07_v3"     = 1445207,
					"Pf3D7_08_v3"     = 1472805,
					"Pf3D7_09_v3"     = 1541735,
					"Pf3D7_10_v3"     = 1687656,
					"Pf3D7_11_v3"     = 2038340,
					"Pf3D7_12_v3"     = 2271494,
					"Pf3D7_13_v3"     = 2925236,
					"Pf3D7_14_v3"     = 3291936,
					"Pf3D7_API_v3"    = 34250,
					"Pf_M76611"       = 5967)
	}
	else {
		stop("Specified genome not supported.")
	}
	
	if (clean_names) {
		names(chroms) <- as.character(factor_chrom(names(chroms)))
	}
	
	return(chroms)
	
}

#' Shortcut to chromosome sizes for Pf3D7 assembly
#'
#'@export
chromsizes_3d7 <- function(...) {
	chromsizes("Pf3D7")
}