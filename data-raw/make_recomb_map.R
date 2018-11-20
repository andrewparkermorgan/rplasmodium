# make_recomb_map.R
# tidy up the genetic map (== recombination map) for P. falciparum, made from data in Miles et al (2016) Genome Research

devtools::load_all()

x <- read.table("data-raw/pf_crosses_v1.map", stringsAsFactors = FALSE)
colnames(x) <- c("chr","marker","cM","pos")
x <- tibble::as_tibble(x)
x$chr <- factor_chrom(x$chr, clean_names = FALSE, genome = "pf3d7")

recomb_map_pf3d7 <- x
devtools::use_data(recomb_map_pf3d7, overwrite = TRUE)