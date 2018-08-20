# chromosomes.R

#' Guess a specific genome assembly from given character string.
.match_genome <- function(g) {
	possible <- c(
		"PF" = "pf3d7",
		"3D7" = "pf3d7",
		"PF3D7" = "pf3d7",
		"FALCIPARUM" = "pf3d7",
		"PFAL" = "pf3d7"
	)
	gg <- toupper(g[1])
	m <- pmatch(gg, names(possible), nomatch = FALSE)
	if (any(m))
		return(unname(possible[m]))
	else
		stop("Can't find a sensible match for genome '",g,"'.")
}

#' Sanitize chromosome names
#' 
#' Convert a vector of chromosome names in some sensible format to "nice" names in karyotype order.
#' 
#' @export
factor_chrom <- function(x, genome = "Pf3D7", ...) {
	
	.genome <- .match_genome(genome)
	
	if (.genome != "pf3d7") {
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
	attr(x, "genome") <- .genome
	return(x)
	
}

#' Check if chromosome names are in nuclear genome
#' 
#' @export
is_nuclear <- function(x, genome = "pf3d7", ...) {
	
	.genome <- match.arg(genome)
	
	if (!inherits(x, "chrom"))
		x <- factor_chrom(x)
	
	if (.genome %in% c("pf3d7"))
		x %in% 1:14
	else
		stop("Genome '",.genome,"' not yet supported.")
	
}

#' Get named vector of chromosome sizes in specified genome assembly
#'
#' @export
chromsizes <- function(genome = "pf3d7", clean_names = FALSE, ...) {
	
	.genome <- .match_genome(genome)
		
	if (genome == "pf3d7") {
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
	chromsizes("pf3d7")
}