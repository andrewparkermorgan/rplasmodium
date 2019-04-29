# make_putative_res_sites.R
# Previously identified loci for putative drug resistance
devtools::load_all()

#pf3d7
pf_3d7_PutDrugRxSites <- readr::read_tsv("data-raw/pf_3d7.drug_genes.bed", col_names = c("chr","start","end","gene_symbol","score","strand","gene_id"))
usethis::use_data(pf_3d7_PutDrugRxSites, overwrite = TRUE)

#pvsal1
pv_sal1_PutDrugRxSites <- readr::read_tsv("data-raw/pv_sal1.drug_genes.bed", col_names = c("chr","start","end","gene_symbol","score","strand","gene_id"))
usethis::use_data(pv_sal1_PutDrugRxSites, overwrite = TRUE)

#pvp01
pv_p01_PutDrugRxSites <- readr::read_tsv("data-raw/pv_p01.drug_genes.bed", col_names = c("chr","start","end","gene_symbol","score","strand","gene_id"))
usethis::use_data(pv_p01_PutDrugRxSites, overwrite = TRUE)
