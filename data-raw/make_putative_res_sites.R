# make_putative_res_sites.R
# Previously identified loci for putative drug resistance
devtools::load_all()

#pf3d7
pf_3d7_PutDrugRxSites <- readr::read_tsv("data-raw/pf_3d7.PutDrugRxSites.bed")
usethis::use_data(pf_3d7_PutDrugRxSites, overwrite = TRUE)

#pvsal1
pv_sal1_PutDrugRxSites <- readr::read_tsv("data-raw/pv_sal1.PutDrugResSites.bed")
usethis::use_data(pv_sal1_PutDrugRxSites, overwrite = TRUE)

#pvp01
pv_p01_PutDrugRxSites <- readr::read_tsv("data-raw/pv_p01.PutDrugResSites.bed")
usethis::use_data(pv_p01_PutDrugRxSites, overwrite = TRUE)
