require(ffbase)
require(LaF)
require(ETLUtils)
options(ffmaxbytes = min(getOption("ffmaxbytes"),.Machine$integer.max * 12))
#pre processing
setwd("C:/Users/CORE_DM/Desktop/edgar/rasters/all_in_one/csv/")
files = list.files(pattern = 'F.*')
files_laf =lapply(files, function (x)laf_open_csv(filename = x,
                                                  column_types = c("integer", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","categorical"),
                                                  column_names = c('order', 'x','y','red','green','blue','red_m','green_m','edge_red','nir','ndvi','classification')
                                                  ,skip = 1))
files_ffdf =lapply(files_laf, function (x)laf_to_ffdf(laf = x))

for (i in 1:6) {
  if (i == 1) {
    file = ffdfappend(files_ffdf[[i]], files_ffdf[[i+1]], adjustvmode=F)   
  } else {
    file = ffdfappend(file, files_ffdf[[i+1]], adjustvmode=F)   
  }
  
}

rm(files, files_laf, files_ffdf)

idx <- ffwhich(file,classification == 5)

negative <- file[idx,]

idx <- ffwhich(file,classification == 3)

positive <- file[idx,]

rm(file, idx)

#positive
#generates 8000 items of samples from 100 to 2000 samples of the ffdf, 400 of each, pos_list contains the positions
# not the actual sample
for (number in 1:400) {
  print(number)
  for (i in seq(100,2000, by=100)) {
    assign(paste0('index_pos_',i,'_', number), sample(length(positive$order),i))
  }
}
pos_list <- mget(ls(pattern = "index_pos.*"))
rm(list = ls(pattern = 'index_pos.*'))
pos_list <-pos_list[order(sapply(pos_list,length),decreasing=F)]
#generates the 8000 samples of the ffdf, pos_samples are the samples of the ffdf
for (i in 1:8000) {
  print(i)
  assign(paste0('positive_',i), positive[pos_list[[i]],])
}
pos_samples <- mget(ls(pattern = "positive_.*"))
pos_samples <-pos_samples[order(sapply(pos_samples,function(x){length(x[,1])}),decreasing=F)]
rm(list =ls(pattern = "positive_.*") )
# generates the the df with the summarized values (mean and sd) for each band for each sample, getting a total of 64k obs
df_pos = data.frame(matrix(vector(), 0, 4,
                           dimnames=list(c(), c('Band','Sample_size','mean','sd'))),
                    stringsAsFactors=F)
band_names <- colnames(pos_samples[[1]])
band_names <- band_names[4:11]

for (i  in 1:8000) {
  print(i)
  for (name in band_names) {
    
    temp <- data.frame(matrix(vector(), 0, 4,dimnames=list(c(), c('Band','Sample_size','mean','sd'))),stringsAsFactors=F)
    temp[1,1] <- name
    temp[1,2] <- length(pos_samples[[i]]$order)
    temp[1,3] <- mean(pos_samples[[i]][,name])
    temp[1,4] <- sd(pos_samples[[i]][,name])
    df_pos <- rbind(df_pos, temp)
  }
  
}

write.csv(df_pos, 'C:/Users/CORE_DM/Dropbox/upch/TDR_approaches_data/CPS_pos_app2.csv')

#negative
#generates 8000 items of samples from 100 to 2000 samples of the ffdf, 400 of each, neg_list contains the positions
# not the actual sample
for (number in 1:400) {
  print(number)
  for (i in seq(100,2000, by=100)) {
    assign(paste0('index_neg_',i,'_', number), sample(length(negative$order),i))
  }
}
neg_list <- mget(ls(pattern = "index_neg.*"))
rm(list = ls(pattern = 'index_neg.*'))
neg_list <-neg_list[order(sapply(neg_list,length),decreasing=F)]
#generates the 8000 samples of the ffdf, pos_samples are the samples of the ffdf
for (i in 1:8000) {
  print(i)
  assign(paste0('negative_',i), negative[neg_list[[i]],])
}
neg_samples <- mget(ls(pattern = "negative_.*"))
neg_samples <-neg_samples[order(sapply(neg_samples,function(x){length(x[,1])}),decreasing=F)]
rm(list =ls(pattern = "negative_.*") )
# generates the the df with the summarized values (mean and sd) for each band for each sample, getting a total of 64k obs
df_neg = data.frame(matrix(vector(), 0, 4,
                           dimnames=list(c(), c('Band','Sample_size','mean','sd'))),
                    stringsAsFactors=F)
band_names <- colnames(neg_samples[[1]])
band_names <- band_names[4:11]

for (i  in 1:8000) {
  print(i)
  for (name in band_names) {
    
    temp <- data.frame(matrix(vector(), 0, 4,dimnames=list(c(), c('Band','Sample_size','mean','sd'))),stringsAsFactors=F)
    temp[1,1] <- name
    temp[1,2] <- length(neg_samples[[i]]$order)
    temp[1,3] <- mean(neg_samples[[i]][,name])
    temp[1,4] <- sd(neg_samples[[i]][,name])
    df_neg <- rbind(df_neg, temp)
  }
  
}

write.csv(df_neg, 'C:/Users/CORE_DM/Dropbox/upch/TDR_approaches_data/CPS_neg_app2.csv')
