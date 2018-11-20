# countries.R
# some functions for encoding/decoding country identifiers, and assigning to geographic regions
# leans heavily on the `countrycode` package

#' Convert country names to standard codes
#' 
#' @param x vector of country identifiers to convert
#' @param code scheme for encoding countries in the output; default is ISO 2-letter code, but
#'   see \code{?countrycode::codelist} for other choices
#' @param ... ignored
#' 
#' @return character vector of country identifiers in the requested encoding; missing or unmatched values will be \code{NA}.
#'   In the case of \code{encode_continent()}, a character vector of continents as assigned by the World Bank.
#' 
#' @details All the heavy lifting is done by the \code{countrycode} package. This function will attempt to find
#'   matches for the input identifiers against a heirarchy of country encodings, starting with ISO 2-letter codes --
#'   so it should be idempotent with respect to already-encoded input.
#' 
#' @seealso \link[countrycode]{codelist}, \link[countrycode]{countrycode}
#' @export
encode_country <- function(x, code = c("iso2c","iso3c","iso.name","un.name","country.name","fips.name","genc.name","continent"), ...) {
	
	x <- as.character(x)
	rez <- rep(NA_character_, length(x))
	.code <- match.arg(code)
	
	ways <- c("iso2c","country.name","iso3c","wb","fips","ecb","cowc","ioc","gwc")
	nways <- length(ways)
	ii <- 1
	while (sum(is.na(rez)) > 0) {
		
		if (ii > nways)
			break
		
		needs <- which(is.na(rez))
		suppressWarnings( attempt <- countrycode::countrycode(x[needs], ways[ii], .code) )
		success <- !is.na(attempt)
		rez[ needs[success] ] <- attempt[success]
		ii <- ii + 1
		
	}
	
	return(rez)
	
}

#' @rdname encode_country
#' @export
encode_continent <- function(x, ...) {
	
	encode_country(x, code = "continent")
	
}