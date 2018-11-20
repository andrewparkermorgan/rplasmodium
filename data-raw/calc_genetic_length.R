# calc_genetic_lengths.R
# extrapolate the genetic length of each chromosome in a simple-minded way

library(dplyr)
devtools::load_all()

map <- recomb_map_pf3d7

extrapolate_last_ivl <- function(df, ...) {
	
	chrom <- as.character(df$chr[1])
	rate <- diff(range(df$cM))/diff(range(df$pos))
	chrlen <- chromsizes_3d7()[chrom]
	extra <- rate*(chrlen - max(df$pos))
	last <- max(df$cM)
	
	bind_rows( arrange(df, cM, pos),
			   tibble(chr = factor_chrom(chrom, clean_names = FALSE), marker = paste0(chrom, "_", chrlen), cM = last+extra, pos = chrlen) )
	
}

group_by(map, chr) %>%
	do(extrapolate_last_ivl(.)) %>%
	slice(n()) %>%
	with(., setNames(cM, as.character(chr))) %>%
	round(., 3) %>%
	dput()