# make_genome_regions.R
# Pre-computed segmentation of Plasmodium genomes into "core" and other regions, taken from published papers.
devtools::load_all()

x <- read.table("data-raw/pf_3d7.regions.bed", stringsAsFactors = FALSE)
colnames(x) <- c("chr","start","end","label")
x <- tibble::as_tibble(x)
x$chr <- factor_chrom(x$chr, clean_names = FALSE, genome = "pf3d7")

regions_pf3d7 <- x
devtools::use_data(regions_pf3d7, overwrite = TRUE)

x <- read.table("data-raw/pv_sal1.regions.bed", stringsAsFactors = FALSE)
colnames(x) <- c("chr","start","end","label")
x <- tibble::as_tibble(x)
x$chr <- factor_chrom(x$chr, clean_names = FALSE, genome = "pvsal1")

regions_pvsal1 <- x
devtools::use_data(regions_pvsal1, overwrite = TRUE)