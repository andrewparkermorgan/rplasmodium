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
#' @param x a vector containing chromosome identifiers
#' @param clean_names logical; if \code{TRUE}, scrub names like 'Pf3D7_01_v3','Pf3D7_02_v3' to '1','2'
#' @param genome identifier for a genome whose chromosome complement determines the levels of this factor
#' @param ... ignored
#' 
#' @return a factor whose levels are the chromosomes in the desired genome, in karyotype order
#' 
#' @export
factor_chrom <- function(x, clean_names = TRUE, genome = "pf3d7", ...) {
	
	.genome <- .match_genome(genome)
	
	if (.genome != "pf3d7") {
		warning("Genomes other than 'pf3d7' not yet supported.")
	}
	x <- as.character(x)
	
	if (clean_names) {
		x <- gsub("^Pf(?:3D7)*_(.+)_.+", "\\1", x, perl = TRUE)
		x <- gsub("^Pf_(.+)", "\\1", x, perl = TRUE)
		x <- gsub("^0", "", x)
		x <- gsub("^A.+", "A", x)
		x <- gsub("^M.+", "M", x)
		x <- factor(x, levels = c(1:14, "A", "M"))
	}
	else {
		x <- factor(x, levels = chromnames(FALSE))
	}
	class(x) <- c(class(x), "chrom")
	attr(x, "genome") <- .genome
	return(x)
	
}

#' Check if chromosome names are in nuclear genome
#' 
#' @param x a vector of chromosome identifiers
#' @param genome with what genome are we working?
#' @param ... ignored
#' 
#' @return a logical vector of same length as \code{x} that is \code{TRUE} if the corresponding entry
#'   looks like a nuclear chromosome and \code{FALSE} if not. \code{NA}s are preserved.
#' 
#' @export
is_nuclear <- function(x, genome = "pf3d7", ...) {
	
	.genome <- match.arg(genome)
	
	if (!inherits(x, "chrom"))
		x <- factor_chrom(x, clean_names = TRUE)
	
	if (.genome %in% c("pf3d7"))
		x %in% 1:14
	else
		stop("Genome '", .genome,"' not yet supported.")
	
}

#' Get chromosome names
#' 
#' @param clean_names logical; if \code{TRUE}, scrub names like 'Pf3D7_01_v3','Pf3D7_02_v3' to '1','2'
#' @param genome with what genome are we working?
#' @param ... ignored
#' 
#' @return the chromosome names of the desired genome, in kartoype order
#' 
#' @export
chromnames <- function(clean_names = FALSE, genome = "pf3d7", ...) {
	
	.genome <- match.arg(genome)
	
	if (.genome == "pf3d7") {
		# have to do this manually because apparently sprintf() leading zeros for integers
		#  are not guaranteed across platforms ...
		nums <- c("01","02","03","04","05","06","07",
				  "08","09","10","11","12","13","14")
		chroms <- paste0("Pf3D7_", nums, "_v3")
		chroms <- c(chroms, "Pf3D7_API_v3", "Pf_M76611")
		if (!clean_names)
			return(chroms)
		else
			return(c(1:14,"A","M"))
	}
	else {
		stop("Genome '", .genome,"' not yet supported.")
	}
	
}

#' Get named vector of chromosome sizes in specified genome assembly
#'
#' @param genome identifier for a genome assembly
#' @param clean_names logical; if \code{TRUE}, scrub chromosome names like 'Pf3D7_01_v3','Pf3D7_02_v3' to '1','2'
#' @param as_seqinfo logical; if \code{TRUE}, return a \code{GenomeInfoDb::Seqinfo} object instead of a named vector
#' @param ... ignored
#'
#' @return a named vector of chromosome lengths in base pairs
#'
#' @export
chromsizes <- function(genome = "pf3d7", clean_names = FALSE, as_seqinfo = FALSE, ...) {
	
	.genome <- .match_genome(genome)
		
	if (genome == "pf3d7") {
		chroms <- c("Pf3D7_01_v3"     = 640851L,
					"Pf3D7_02_v3"     = 947102L,
					"Pf3D7_03_v3"     = 1067971L,
					"Pf3D7_04_v3"     = 1200490L,
					"Pf3D7_05_v3"     = 1343557L,
					"Pf3D7_06_v3"     = 1418242L,
					"Pf3D7_07_v3"     = 1445207L,
					"Pf3D7_08_v3"     = 1472805L,
					"Pf3D7_09_v3"     = 1541735L,
					"Pf3D7_10_v3"     = 1687656L,
					"Pf3D7_11_v3"     = 2038340L,
					"Pf3D7_12_v3"     = 2271494L,
					"Pf3D7_13_v3"     = 2925236L,
					"Pf3D7_14_v3"     = 3291936L,
					"Pf3D7_API_v3"    = 34250L,
					"Pf_M76611"       = 5967L)
		
		if (clean_names) {
			names(chroms) <- as.character(factor_chrom(names(chroms), genome = .genome))
		}
		
		if (as_seqinfo) {
			new("Seqinfo",
				seqnames = names(chroms),
				seqlengths = chroms,
				is_circular = c(rep(TRUE, 14), FALSE,FALSE),
				genome = rep("pf3d7", length(chroms))
			)
		}
		else
			return(chroms)
		
	}
	else {
		stop("Specified genome not supported.")
	}
	
}

#' Shortcut to chromosome sizes for Pf3D7 assembly
#' 
#' @param ... ignored
#' 
#' @return Shortcut to return chromosome sizes in Pf3D7 assembly
#' 
#' @export
chromsizes_3d7 <- function(...) {
	chromsizes("pf3d7")
}