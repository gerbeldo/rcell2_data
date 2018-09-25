source("./rcell2.R")


d <- load_cell_data(path = "/home/german/code/rcell2/rcell2/sample_data")

file.info("./#temp.R#")


a <- read_tsv("~/code/rcell2/rcell2/sample_data/Position01/out_all")

b <- read.table("~/code/rcell2/rcell2/sample_data/Position01/out_all", sep = "\t",
                colClasses = "numeric",
                header = T)

m <- read_tsv("~/code/rcell2/rcell2/sample_data/Position01/out_bf_fl_mapping")

.mk.flag.table(m)
