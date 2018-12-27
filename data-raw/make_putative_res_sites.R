# make_putative_res_sites.R
# Previously identified loci for putative drug resistance
devtools::load_all()

#pf3d7
pf_3d7_PutDrugRxSites <- read.table("data-raw/pf_3d7.PutDrugRxSites.bed", stringsAsFactors = FALSE)
usethis::use_data(pf_3d7_PutDrugRxSites, overwrite = TRUE)

#pvsal1
pv_sal1_PutDrugRxSites <- read.table("data-raw/pv_sal1.PutDrugResSites.bed", stringsAsFactors = FALSE)
usethis::use_data(pv_sal1_PutDrugRxSites, overwrite = TRUE)
