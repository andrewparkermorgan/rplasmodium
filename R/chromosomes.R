# chromosomes.R

#' Guess a specific genome assembly from given character string.
.match_genome <- function(g) {
	possible <- c(
		"PF" = "pf3d7",
		"3D7" = "pf3d7",
		"PF3D7" = "pf3d7",
		"FALCIPARUM" = "pf3d7",
		"PFAL" = "pf3d7",
		"PVSAL1" = "pvsal1",
		"PV" = "pvsal1",
		"SAL1" = "pvsal1",
		"VIVAX" = "pvsal1",
		"PVP01" = "pvp01",
		"P01" = "pvp01"
	)
	gg <- toupper(g[1])
	gg <- gsub("_", "", gg)
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
#' @details A vector blessed as a chromosome by \code{factor_chrom()} can be confirmed with \code{is_chrom()}.
#' 
#' @export
factor_chrom <- function(x, clean_names = TRUE, genome = "pf3d7", ...) {
	
	.genome <- .match_genome(genome)
	
	if (!(.genome %in% c("pf3d7","pvsal1","pvp01"))) {
		warning("Genomes other than 'pf3d7','pvsal1','pvp01' not yet supported.")
	}
	x <- as.character(x)
	
	if (clean_names) {
		if (.genome == "pf3d7") {
			x <- gsub("^Pf(?:3D7)*_(.+)_.+", "\\1", x, perl = TRUE)
			x <- gsub("^Pf_(.+)", "\\1", x, perl = TRUE)
		}
		else if (.genome == "pvsal1") {
			x <- gsub("^Pv_Sal1_chr", "", x, perl = TRUE)
			x <- gsub("^PVAD80_", "", x, perl = TRUE)
		}
		else if (.genome == "pvp01") {
			x <- gsub("^Pv(?:P01)*_(.+)_.+", "\\1", x, perl = TRUE)
			x <- gsub("^Pv_(.+)", "\\1", x, perl = TRUE)
		}
		x <- gsub("^0", "", x)
		x <- gsub("^A.+", "A", x)
		x <- gsub("^M.+", "M", x)
		x <- factor(x, levels = c(1:14, "M", "A"))
	}
	else {
		x <- factor(x, levels = chromnames(clean_names = FALSE, genome = .genome))
	}
	class(x) <- c(class(x), "chrom")
	attr(x, "genome") <- .genome
	return(x)
	
}

#' @rdname factor_chrom
#' @export
is_chrom <- function(x, ...) {
	
	GENOMES <- c("pf3d7","pvsal1","pvp01")
	inherits(x, "chrom") && !is.null(attr(x, "genome")) && attr(x, "genome") %in% GENOMES
	
}

#' Check if chromosome vectors are from same genome
#' 
#' @param ... any number of vectors encoding chromosomes; need not have same length
#' 
#' @return logical scalar indicating whether the input vectors have all been encoded as chromosomes in same genome assembly
#' 
#' @export
same_genome <- function(...) {
	
	args <- list(...)
	ischrom <- sapply(args, is_chrom)
	genomes <- sapply(args, attr, "genome")
	
	if (any(sapply(genomes, is.null)))
		return(FALSE)
	else
		return( all(ischrom) && length(unique(genomes)) == 1 )
	
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
	
	.genome <- .match_genome(genome)
	
	if (!inherits(x, "chrom"))
		x <- factor_chrom(x, clean_names = TRUE)
	
	if (.genome %in% c("pf3d7","pvsal1"))
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
	
	.genome <- .match_genome(genome)
	
	# have to do this manually because apparently sprintf() leading zeros for integers
	#  are not guaranteed across platforms ...
	nums <- c("01","02","03","04","05","06","07",
			  "08","09","10","11","12","13","14")
	
	if (.genome == "pf3d7") {
		chroms <- paste0("Pf3D7_", nums, "_v3")
		chroms <- c(chroms, "Pf_M76611", "Pf3D7_API_v3")
		if (!clean_names)
			return(chroms)
		else
			return(c(1:14,"M","A"))
	}
	else if (.genome == "pvsal1") {
		chroms <- paste0("Pv_Sal1_chr", nums)
		chroms <- c(chroms, "PVAD80_MIT")
		if (!clean_names)
			return(chroms)
		else
			return(c(1:14,"M"))
	}
	else if (.genome == "pvp01") {
		chroms <- paste0("PvP01_", nums, "_v1")
		chroms <- c(chroms, "PvP01_MIT_v1", "PvP01_API_v1")
		if (!clean_names)
			return(chroms)
		else
			return(c(1:14,"M","A"))
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
#' @return a named vector of chromosome lengths in base pairs or (in case of \code{chromsizes_cM()}) in centimorgans
#'
#' @export
chromsizes <- function(genome = "pf3d7", clean_names = FALSE, as_seqinfo = FALSE, ...) {
	
	.genome <- .match_genome(genome)
		
	if (genome == "pf3d7") {
		chroms <- c(
			"Pf3D7_01_v3"     = 640851L,
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
			"Pf_M76611"       = 5967L,
			"Pf3D7_API_v3"    = 34250L
		)
		is_circ <- c( rep(FALSE, 14), TRUE, TRUE )
	}
	else if (.genome == "pvsal1") {
		chroms <- c(
			"Pv_Sal1_chr01"	= 830022L,
			"Pv_Sal1_chr02"	= 755035L,
			"Pv_Sal1_chr03"	= 1011127L,
			"Pv_Sal1_chr04"	= 876652L,
			"Pv_Sal1_chr05"	= 1370936L,
			"Pv_Sal1_chr06"	= 1033388L,
			"Pv_Sal1_chr07"	= 1497819L,
			"Pv_Sal1_chr08"	= 1678596L,
			"Pv_Sal1_chr09"	= 1923364L,
			"Pv_Sal1_chr10"	= 1419739L,
			"Pv_Sal1_chr11"	= 2067354L,
			"Pv_Sal1_chr12"	= 3004884L,
			"Pv_Sal1_chr13"	= 2031768L,
			"Pv_Sal1_chr14"	= 3120417L,
			"PVAD80_MIT"	= 5990L
		)
		is_circ <- c( rep(FALSE, 14), TRUE )
	}
	else if (.genome == "pvp01") {
		chroms <- c(
			"PvP01_01_v1"	= 1021664L,
			"PvP01_02_v1"	= 956327L,
			"PvP01_03_v1"	= 896704L,
			"PvP01_04_v1"	= 1012024L,
			"PvP01_05_v1"	= 1524814L,
			"PvP01_06_v1"	= 1042791L,
			"PvP01_07_v1"	= 1652210L,
			"PvP01_08_v1"	= 1761288L,
			"PvP01_09_v1"	= 2237066L,
			"PvP01_10_v1"	= 1548844L,
			"PvP01_11_v1"	= 2131221L,
			"PvP01_12_v1"	= 3182763L,
			"PvP01_13_v1"	= 2093556L,
			"PvP01_14_v1"	= 3153402L,
			"PvP01_MIT_v1"	= 5989L,
			"PvP01_API_v1"	= 29582L
		)
		is_circ <- c( rep(FALSE, 14), TRUE, TRUE )
	}
	else {
		stop("Specified genome not supported.")
	}
	
	if (clean_names) {
		names(chroms) <- as.character(factor_chrom(names(chroms), genome = .genome))
	}
	
	if (as_seqinfo) {
		new("Seqinfo",
			seqnames = names(chroms),
			seqlengths = chroms,
			is_circular = is_circ,
			genome = rep(.genome, length(chroms))
		)
	}
	else
		return(chroms)
	
}

#' @rdname chromsizes
#' @export
chromsizes_3d7 <- function(...) {
	chromsizes("pf3d7")
}

#' @rdname chromsizes
#' @export
chromsizes_sal1 <- function(...) {
	chromsizes("pvsal1")
}

#' @rdname chromsizes
#' @export
chromsizes_p01 <- function(...) {
	chromsizes("pvp01")
}

#' @rdname chromsizes
#' @export
chromsizes_cM <- function(..., clean_names = FALSE, genome = "pf3d7") {
	
	.genome <- .match_genome(genome)
	
	if (.genome == "pf3d7") {
		chroms <- c(
			"Pf3D7_01_v3" =  34.185,
			"Pf3D7_02_v3" =  68.535,
			"Pf3D7_03_v3" =  90.541,
			"Pf3D7_04_v3" =  71.489,
			"Pf3D7_05_v3" =  89.755,
			"Pf3D7_06_v3" = 101.070,
			"Pf3D7_07_v3" =  95.367, 
			"Pf3D7_08_v3" =  85.365,
			"Pf3D7_09_v3" = 113.874,
			"Pf3D7_10_v3" = 119.600,
			"Pf3D7_11_v3" = 140.911,
			"Pf3D7_12_v3" = 137.708,
			"Pf3D7_13_v3" = 181.040,
			"Pf3D7_14_v3" = 209.136
			)
	}
	else {
		stop("Reputable genetic map only exists for the pf3d7 assembly.")
	}
	
	if (clean_names) {
		names(chroms) <- as.character(factor_chrom(names(chroms), genome = .genome))
	}
	
	return(chroms)
	
}

