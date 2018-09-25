## #Rcell: R package for analysis of CellID datasets
## #ToDo: documentation on cell.data object

## ##################### Package Constants #################################
## .conflicts.OK=TRUE
## .CELLID_ID_VARS=c("pos","t.frame","cellID")
## .CELLID_ID_VARS_DERIV=c(.CELLID_ID_VARS,"ucid","time")
## .CELLID_DROP_VARS=c("flag","num.pix","con.vol.1")
## ##################### cell.data functions ###############################

## ## #Strong dependence on ggplot2 package
## ## .onAttach <- function(lib, pkg, ...) {
## ## 		theme_set(Rcell::theme_Rcell())
## ## }

## #*************************************************************************#
## #public
## #loads a cellID output to a cell.data object
## #ToDo: fix bf as fluorescence option of cellID, with image.info table
## #ToDo: when bf_as_fl string BF_ appears as channel identifier

## ## if(getRversion() >= "2.15.1") utils::globalVariables(c("cellID",
## ##                                                        "pos",
## ##                                                        "flag",
## ##                                                        "fluor",
## ##                                                        "bright",
## ##                                                        "channel"))

library(dplyr)
library(readr)

load_cell_data <-
    function(path = getwd(),
             pattern = "^[Pp]{1}os[:alpha:]*[:digit:]*",
             basename = "out",
             select = NULL,
             exclude = NULL,
             load.vars = "all",
             split.image = FALSE) {

        on.exit(gc())

        # transform path to absolute and canonical path in a OS-dependant way
        path <- normalizePath(path)

        # HERE IS THE POSTA
        # Searching for folders that match arg. 'pattern'
        # this makes a char vector with the names of all the position folders
	posdir = dir(pattern = pattern, path = path)
        print(posdir)

        #ToDo: #autoload and associate pdata file

        #############
        ## initialize variables
        #############
        # vector with the loaded positions, for output
        loaded.pos = c()

        # list with the loaded position directory
	loaded.pos.dir = list()

        # data frame with the position, flag, ch name, number of frames for that flag, and is.bf
        flag.table = data.frame()

        #ToDo:
	data <- c()

        # each element corresponds to a single position data
        pos.data <- list()

        # mapping between a bf image to its corresponding fluorescence images.
        # each element corresponds to a position
	bf.fl.mapping <- list()

        # count and array to correct numbers if position folders don'y have them
        # count = 0
        # posdir.index = array(-1, dim = c(length(posdir)))

        #variable to assert all output_all have the same columns
        column.names = c()

        ######## ASSERT
        #checking if there are Pos folders to be loaded
	if(length(posdir) == 0) stop("No Pos folder found in specified path or working directory.")
        ########

	cat("reading positions...\n")

        for(i in 1:length(posdir)) {

            curr_dir = paste(path, "/", posdir[i], "/", sep = "")


            ######## ASSERT
            # check that current path is a folder, and assign cellID out filenames
            if(!file.info(curr_dir)$isdir) {stop("curr_dir is not a directory")}
            ########


            # paths for out_* files defined
            out_all <- paste(curr_dir, basename, "_", "all", sep = "")
            out_mapping <- paste(curr_dir, basename, "_bf_fl_mapping", sep = "")


            ######## ASSERT
            # check that file exists
            if(!file.exists(out_all)) {
                stop(paste("Missing file:", out_all, "\n Position not loaded\n"))}
            ########


            # rcell had a check for folders with no number, and assigned an ordinal if there
            # wasn't one. I found no use for it, so I removed it


            ####################
            ## reading data
            ####################

            # print position being processed
            cat(gsub("[a-zA-Z_]", "", posdir[i])," ")
            if(i %% 10 == 0) cat("\n")

            pos.data[[i]] <- readr::read_tsv(out_all)

            # ToDo: check if there's a difference between using Hmisc::import.cleanup or not.

            ######### ASSERT
            # asserting that previously loaded positions have the same number and column names
            if(length(column.names) == 0){ #first position
                column.names <- names(pos.data[[i]])

            } else { #not first position
                curr_names <- names(pos.data[[i]])

                if(length(curr_names) != length(column.names)) {
                    stop(out_all," has different number of colums than previous position\n")
                }

                if(sum(column.names == curr_names) != length(column.names)) {
                    stop(out_all," has different column names than previous positions\n")
                }
            }
            ########

            # updates contents of positions loaded.
            loaded.pos <- c(loaded.pos, i)
            loaded.pos.dir[[i]] <- posdir[i]

            #reading output_bf_fl_mapping
            if(file.exists(out_mapping)) {

                # assuming that all mapping files have the same column types
                bf.fl.mapping[[i]] <- readr::read_tsv(out_mapping,
                                                      col_types = "ciici")


                #creating flag table
                pos.flag <- .mk.flag.table(bf.fl.mapping[[i]], pos = i)
                flag.table <- dplyr::bind_rows(pos.flag, flag.table)

            } else warning(out_mapping, "not found")
        }


        ####################
        # creating variables
        ####################

        # creates the creates the variables:
        #    - pos: position number associated to all cells in a position df.
        #    - ucid: unique number associated to each cell
        #    - qc: "quality control", used for filtering


        cat("\ncreating variables...\n")

        for (ipos in loaded.pos) {
            pos.data[[ipos]] <- dplyr::mutate(pos.data[[ipos]],
                                              pos = ipos,
                                              ucid = ipos * 1e6 + cellID,
                                              qc = T)
        }

        pos.data
    }







#*************************************************************************#
#private
#generates a table mapping a channel name (3 first characters of the image file)
# to a flag number for a given bf.fl.mapping data.frame
#(read from an output_bf_fl_mapping file)

#g: input: bf.fl.mapping corresponding to a single position! so it's a data.frame

.mk.flag.table <- function(bf.fl.mapping, pos = NULL){

    flag.name = vector(mode = "character", length = 0)
    flag = c()
    flag.frame.n = c()
    flag.is.bf = c()
    flag.count = 0
    output = data.frame()


    #g: for each fluorescence image (rows in out_bf_fl_mapping)
    for(i in 1:nrow(bf.fl.mapping)){

        print(paste("ciclo", i))

        # g: separates path to fluorescence image into its components
        part.path <- strsplit(as.character(bf.fl.mapping[i, 1]), "[/\\]")[[1]]

        tmpstr <- substr(part.path[length(part.path)], 1, 3)
        print(tmpstr)
        flag.name.index = which(flag.name == tmpstr)
        print(flag.name.index)

        if(length(flag.name.index) == 0){ #new flag
            tmpflag = as.integer(bf.fl.mapping[i, 2])
            if(length(which(flag == tmpflag)) == 0){#cheking consistency
                flag = c(flag, tmpflag)
                flag.name = c(flag.name, tmpstr)
                flag.frame.n = c(flag.frame.n, 1)
                flag.count = flag.count + 1

                if(tmpstr == substr(bf.fl.mapping[i, 4], 1, 3)){
                    #if(bf.fl.mapping[i,5]==1){
                    flag.is.bf = c(flag.is.bf,TRUE)
                } else {
                    flag.is.bf = c(flag.is.bf,FALSE)
                }
            } else {
                cat(".mk.flag.table: Flag name ambiguity.\n")
            }
        } else if(length(flag.name.index) == 1) { #flag all ready assing
            flag.frame.n[flag.name.index] = flag.frame.n[flag.name.index] + 1
        } else {
            cat(".mk.flag.table: Ambiguous flag name\n")
        }

    }

    output = data.frame(flag = flag,
                      channel = flag.name,
                      frame.n = flag.frame.n,
                      is.bf = flag.is.bf)

    if(!is.null(pos)){
        output = data.frame(pos = rep(pos, flag.count), output)
    }

    return(output)
}


#*************************************************************************#
# private

.assert_file_names <- function(fname) {}
