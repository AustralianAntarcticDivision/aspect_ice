
## data here:
##http://aspect.antarctica.gov.au/data

examplefile <- function(x = 1) {
  c("http://aspect.antarctica.gov.au/__data/assets/text_file/0019/59122/ASPECT_Allvoys_obs_mindist0.txt")[x]
}
parse_header <- function(x) {
  ## all the descriptors have a colon
  desc <- grep(":", x)
  headers <- strsplit(gsub("^%", "", x[desc]), ":")
  ## etc etc
}

## need to parse the header fully, not sure how general this needs to be
readsitd <- function(x, ...) {
  if (missing(x)) x <- examplefile(1)
  txt <- readLines(x)
  dat <- read.table(text = gsub("^\\s+", "", txt), comment.char = "%")
  ## parse header
  comments <- grep("^ %", txt) 
  header <- parse_header(comments)
  idents <- grep("^% ASPECT", txt)
  ## starting rows of each identifier in dat
  strows <- idents - min(idents) + 1 - cumsum(c(0, rep(1, length(idents)-1)))
  dat$program <- rep(gsub("% ", "", txt[idents]), c(diff(strows), nrow(dat) - strows[length(strows)] + 1))
  ## remove duplicates (there's one at 1994    4   91.125    98.283  -64.100   )
  dat <- dat[!duplicated(dat), ]
  ## also duplicated times
  ## lapply(split(dat, dat$program), function(x) which(duplicated(x$gmt)))
#   $ASPECT_HH010298
#   [1]  21  24  75  78  82  85  88  94  97 103 106 111 114 118 121 124 128 131 134 137 141 145 148 152 155 158 162 165 168 171 175
#   
#   $ASPECT_HH120299
#   [1]    4    7   12   22   25   45   74   81  222  280  284  295  310  401  426  437  559  566  576  599  607  621  652  656  670  674  689  699  703  719  734
#   [32]  769  771  776  778  822  824  898  935  943  956  968  985  987 1002 1011 1052 1059 1064 1066 1080 1093 1101 1134 1150 1189 1197 1199 1203 1229 1262 1264
#   [63] 1267 1276 1278 1280 1293 1295
#   
  
 dat
}

library(trip)

coordinates(dat) <- c("V4", "V5")
proj4string(dat) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 +over")
dat$gmt <- ISOdatetime(dat$V1, dat$V2, 1, 0, 0, 0, tz = "UTC") + dat$V3 * 24 * 3600
dat <- dat[order(dat$program, dat$gmt), ]
dat$fixgmt <- adjust.duplicateTimes(dat$gmt, dat$program)
tr <- trip(dat, c("fixgmt", "program"))
library(rgdal)
proj <- "+proj=laea +lat_0=-60 +ellps=WGS84 +lon_0=147"
ptr <- spTransform(tr, CRS(proj))
library(rworldmap)

data(countriesLow)
wm <- spTransform(countriesLow, CRS(proj))
