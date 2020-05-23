library(httr)
library(GEOmetadb)
library(rhdf5)
library(tidyr)

isGPLAnnotGood <- function (gpl) {
  stub = gsub('\\d{1,3}$','nnn',gpl,perl=TRUE)
  gplurl <- 'https://ftp.ncbi.nlm.nih.gov/geo/platforms/%s/%s/annot/%s'
  myurl <- sprintf(gplurl,stub,gpl,paste0(gpl,'.annot.gz'))
  
  req <- HEAD(myurl)
  return (status_code(req) != 404)
}

if(!file.exists('GEOmetadb.sqlite')) getSQLiteFile()
con <- dbConnect(SQLite(),'GEOmetadb.sqlite')
allGSEGPLs <- dbGetQuery(con,'select * from gse_gpl')

message('Size GSE_GPL: ')
message(nrow(allGSEGPLs))

res <- apply(allGSEGPLs, 1, FUN = function(x) {
  a <- isGPLAnnotGood(x[['gpl']])
  message(x,a)
  a
})
res <- cbind(allGSEGPLs, res)

getArchs4Files <- function(cacheDir) {
  list.files(paste(file.path(cacheDir), 'archs4', sep = .Platform$file.sep), '\\.h5$', full.names = TRUE)
}
archs4_files <- getArchs4Files('~/cache')
archs4_samples <- lapply(archs4_files, function (file) {
  h5read(file, "meta/Sample_geo_accession")
})

gsm_base <- dbGetQuery(con, 'select gsm,series_id,gpl from gsm')
gsm_base <- gsm_base[!is.na(gsm_base$series_id),]
splited_gsm_base <- separate_rows(gsm_base, series_id)

checkGSE_ARCHS4 <- function (x) {
  GSMs <- splited_gsm_base[splited_gsm_base$series_id == x[['gse']] & splited_gsm_base$gpl == x[['gpl']],]$gsm
  message(x[['gse']], x[['gpl']])
  for (samplebase in archs4_samples) {
    sampleIndexes <- match(GSMs,  samplebase)
    a <- sum(!is.na(sampleIndexes)) == length(GSMs)
    if (a) {
      message('hit')
      return (TRUE)
    }
  }
  
  return (FALSE)
}
arch_res <- apply(res, 1, function (x) {
  if (x[['res']]) {
    return (FALSE)
  }
  
  checkGSE_ARCHS4(x)
})

full <- cbind(res, arch_res)
dbDisconnect(con)