# data.R
# documentation of datasets provided with this package

#' Segmentation of P. vivax genome into "core" and hypervariable regions
#'
#' @format A \code{tibble} (inherits from data frame) with 4 columns:
#' \describe{
#'   \item{chr}{chromosome names, as ordered factor (\code{pvsal1} assembly)}
#'   \item{start}{start position of interval, 0-based, left-closed}
#'   \item{end}{end position of interval, 0-based, right-open}
#'   \item{label}{classification of the interval as one of \code{Core},
#'     \code{InternalHypervariable}, or \code{SubtelomericHypervariable}
#' }
#' @source Pearson RD et al. (2016) \emph{Nature Genetics} 48: 959-964. \url{https://doi.org/10.1038/ng.3599}
"regions_pvsal1"

#' Segmentation of P. falciparum genome into "core" and hypervariable regions
#'
#' @format A \code{tibble} (inherits from data frame) with 4 columns:
#' \describe{
#'   \item{chr}{chromosome names, as ordered factor (\code{pf3d7} assembly)}
#'   \item{start}{start position of interval, 0-based, left-closed}
#'   \item{end}{end position of interval, 0-based, right-open}
#'   \item{label}{classification of the interval as one of \code{Core},
#'     \code{InternalHypervariable}, or \code{SubtelomericHypervariable}
#' }
#' @source Miles A et al. (2016) \emph{Genome Research} 26: 1288-1299. \url{https://doi-org.libproxy.lib.unc.edu/10.1101/gr.203711.115}
"regions_pf3d7"

#' Genetic map for P. falciparum genome
#'
#' A genetic map (== recombination map) derived from crossovers reported in Miles et al. (2016), using identity
#'   map function (ie. no unobserved double-crossovers). Format follows the \code{*.map} format of PLINK and other
#'   programs for pedigree and linkage analysis.
#'
#' @format A \code{tibble} (inherits from data frame) with 4 columns:
#' \describe{
#'   \item{chr}{chromosome names, as ordered factor (\code{pf3d7} assembly)}
#'   \item{marker}{dummy marker name, created by concatenating chromosme and position of mid-point of crossover uncertainty interval}
#'   \item{cM}{genetic position in centimorgans}
#'   \item{pos}{physical position in base pairs on \code{pf3d7} assembly, 1-based}
#' }
#' @source Miles A et al. (2016) \emph{Genome Research} 26: 1288-1299. \url{https://doi-org.libproxy.lib.unc.edu/10.1101/gr.203711.115}
"recomb_map_pf3d7"
