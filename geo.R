devtools::load_all()
load('./Full.rda')
options(phantasusCacheDir='D://datasets/phantasus_cache/')
for(i in 1:nrow(full)) {
    a <- full[i, ]
    message(i)
    if (a$res || a$arch_res) {
        tryCatch({
            GSEs <- full[full$gse == a$gse,]
            if (nrow(GSEs) > 1) {
                for(j in 1:nrow(GSEs)) {
                    platformedGSE <- GSEs[j,]
                    phantasus:::loadGEO(paste0(platformedGSE$gse, '-', platformedGSE$gpl))
                }
            } else {
                phantasus:::loadGEO(a$gse)
            }
        }, error=function(e) {
            message('Failed to load: ', a$gse, e)
        })
    }
}
