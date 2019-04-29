# util.R
# miscellaneous utility functions, pending reorganization

#' Convert dataframe-like object to GRanges, assuming sensible column names
#' @export
genomify <- function(df, genome = "3d7", keep_extra = TRUE, zero_based = TRUE, trim_ranges = TRUE, ...) {
	
	if (!inherits(df, "data.frame")) {
		stop("Must an object that inherits from a dataframe.")
	}
	
	.genome <- .match_genome(genome)
	if (!(.genome %in% c("pf3d7","pvsal1","pvp01"))) {
		warning("Genomes other than 'pf3d7','pvsal1','pvp01' not yet supported.")
	}
	
	colnames(df) <- tolower(colnames(df))
	if (!any(c("start","end") %in% colnames(df))) {
		if ("pos" %in% colnames(df)) {
			df$start <- df$pos
			df$end <- df$pos + as.integer(zero_based)
		}
		else {
			stop("Please supply columns named 'start' and 'end' (or just 'pos').")
		}
	}
	if (!("strand" %in% colnames(df))) {
		df$strand <- "*"
	}
	
	rez <- GenomicRanges::makeGRangesFromDataFrame(
		df, keep.extra.columns = keep_extra,
		starts.in.df.are.0based = zero_based,
		seqinfo = chromsizes(.genome, clean_names = FALSE, as_seqinfo = TRUE))
	if (trim_ranges) {
		rez <- GenomicRanges::trim(rez)
	}
	
	return(rez)
	
}

#' Convert GRanges back to tibble
#' @export
as_tibble.GRanges <- function(g, zero_based = FALSE, ...) {
	
	offset <- if (zero_based) 1 else 0
	s <- GenomicRanges::start(g) - offset
	e <- GenomicRanges::end(g)
	chrom <- as.character(GenomicRanges::seqnames(g))
	m <- tibble::as_tibble( GenomicRanges::mcols(g) )
	st <- as.character(GenomicRanges::strand(g))
	
	rez <- tibble::tibble(chr = chrom, start = s, end = e, strand = st)
	dplyr::bind_cols(rez, m)
	
}