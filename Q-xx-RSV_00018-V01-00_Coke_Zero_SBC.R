# beverage parameter ####
setwd(this.path::this.dir())
source.file <- print("Rsource_Coke_Zero_V01.R")
source( paste0(getwd(), "/", source.file) )

# spectra ####
setwd(dt$wd)
setwd( "./221220_Coke_Zero")
setwd( "./spc")

dt$para$files <- dir(pattern = ".csv$")
dt$para$txt <- txt.file(dt$para$files)

dt$raw <- lapply(dt$para$files, \(x) fread(x, sep = ";", dec = ","))
names(dt$raw) <- paste0(dt$para$txt$type, "_", dt$para$txt$loc.line)

dt$para$trs <- lapply(dt$raw, transfer_csv.num.col)
dt$para$wl <- 190 : 598

dt$raw  <- mapply(function(x , y) y[ , c(1 : (min(x$numcol) - 1), x$numcol[ x$wl %in% dt$para$wl ]), with = F]
                  , x = dt$para$trs
                  , y = dt$raw)

dt$para$trs <- lapply(dt$raw, transfer_csv.num.col)

# validate drk ####
# par( mfrow = mfrowp( length( grep( "drk", names(dt$para$trs)))))
# for(i in grep( "drk", names(dt$para$trs)))
#   matplot(dt$para$wl
#           , t(dt$raw[[ i ]][ , dt$para$trs[[ i ]]$numcol, with = F])
#           , lty = 1, type = "l", xlab = lambda, ylab = "Counts", main = dt$para$txt$loc.line[ i ])

for(i in grep( "drk", names(dt$para$trs))){
  
  dt$val$drk[[ i ]] <- apply(dt$raw[[ i ]][ , dt$para$trs[[ i ]]$numcol, with = F], 1, spectra.validation.drk)
  print( unique(dt$val$drk[[ i ]]) )
  dt$raw$spc <- dt$raw$spc[ spectra.validation.range(valid.vector = dt$val$drk[[ i ]]
                                                     , drkref.datetime = dt$raw[[ i ]]$datetime
                                                     , spc.datetime = dt$raw$spc$datetime
                                                     , pattern = "invalid") , ]
  dt$val$drk[[ i ]] <- apply(dt$raw[[ i ]][ , dt$para$trs[[ i ]]$numcol, with = F], 1, spectra.validation.drk)
  dt$raw$spc <- dt$raw$spc[ spectra.validation.range(valid.vector = dt$val$drk[[ i ]]
                                                     , drkref.datetime = dt$raw[[ i ]]$datetime
                                                     , spc.datetime = dt$raw$spc$datetime
                                                     , pattern = "empty") , ]
}

# validate ref ####
# par( mfrow = mfrowp( length( grep( "drk", names(dt$para$trs)))))
# for(i in grep( "ref", names(dt$para$trs)))
#   matplot(dt$para$wl
#           , t(dt$raw[[ i ]][ , dt$para$trs[[ i ]]$numcol, with = F])
#           , lty = 1, type = "l", xlab = lambda, ylab = "Counts", main = dt$para$txt$loc.line[ i ])

# PCA ####
for(i in grep( "spc", names(dt$para$trs))) dt$pca[[ i ]] <- dt$raw[[ i ]][ , pca(.SD), .SDcols = dt$para$trs[[ i ]]$numcol]

for(i in grep( "spc", names(dt$para$trs))){
  print( nrow( dt$raw[[ i ]]))

  dt$raw[[ i ]] <- dt$raw[[ i ]][ which( dt$pca[[ i ]]$calres$T2[ , 1] < dt$pca[[ i ]]$T2lim[ 2, 1 ]) , ]
  dt$raw[[ i ]] <- dt$raw[[ i ]][ which( dt$pca[[ i ]]$calres$Q[ , 1] < dt$pca[[ i ]]$Qlim[ 2, 1 ]) , ]
  
  print( nrow( dt$raw[[ i ]]))
}

# validate spc ####
par( mfrow = mfrowp( length( grep( "spc", names(dt$para$trs)))))
dt$para$nm <- 220; for(i in grep( "spc", names(dt$para$trs))){ boxplot(dt$raw[[ i ]][ , grep( dt$para$nm, names( dt$raw[[ i ]] )), with = F]) }

grep( "spc", names(dt$para$trs))
i = 3; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .6, type = "larger")
i = 7; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .6, type = "larger")
i = 11; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .6, type = "larger")
i = 15; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .6, type = "larger")

i = 11; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .8, type = "smaller")
i = 15; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .8, type = "smaller")

dt$para$nm <- 250; for(i in grep( "spc", names(dt$para$trs))){ boxplot(dt$raw[[ i ]][ , grep( dt$para$nm, names( dt$raw[[ i ]] )), with = F]) }
i = 3; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .32, type = "larger")
i = 7; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .32, type = "larger")
i = 11; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .32, type = "larger")
i = 15; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .32, type = "larger")

dt$para$nm <- 350; for(i in grep( "spc", names(dt$para$trs))){ boxplot(dt$raw[[ i ]][ , grep( dt$para$nm, names( dt$raw[[ i ]] )), with = F]) }

dt$para$nm <- 450; for(i in grep( "spc", names(dt$para$trs))){ boxplot(dt$raw[[ i ]][ , grep( dt$para$nm, names( dt$raw[[ i ]] )), with = F]) }

dt$para$nm <- 550; for(i in grep( "spc", names(dt$para$trs))){ boxplot(dt$raw[[ i ]][ , grep( dt$para$nm, names( dt$raw[[ i ]] )), with = F]) }

i = 3; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .06, type = "smaller")
i = 7; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .06, type = "smaller")
i = 11; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .06, type = "smaller")
i = 15; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = .06, type = "smaller")

i = 3; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = 0, type = "larger")
i = 7; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = 0, type = "larger")
i = 11; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = 0, type = "larger")
i = 15; dt$raw[[ i ]] <- subset.data.table( raw = dt$raw, i = i, nm = dt$para$nm, threshold = 0, type = "larger")

# export ####
# export clean spc csv ####
for(i in grep( "spc", names(dt$para$trs)))
  fwrite(dt$raw[[ i ]]
         , gsub("_spc.csv", "_spc_validated.csv", dt$para$files[ i ])
         , sep = ";", dec = ",")





