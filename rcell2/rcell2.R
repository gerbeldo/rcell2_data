#Rcell: R package for analysis of CellID datasets
#ToDo: documentation on cell.data object

##################### Package Constants #################################
.conflicts.OK=TRUE
.CELLID_ID_VARS=c("pos","t.frame","cellID")
.CELLID_ID_VARS_DERIV=c(.CELLID_ID_VARS,"ucid","time")
.CELLID_DROP_VARS=c("flag","num.pix","con.vol.1")
##################### cell.data functions ###############################

## #Strong dependence on ggplot2 package
## .onAttach <- function(lib, pkg, ...) {
## 		theme_set(Rcell::theme_Rcell())
## }

#*************************************************************************#
#public
#loads a cellID output to a cell.data object
#ToDo: fix bf as fluorescence option of cellID, with image.info table
#ToDo: when bf_as_fl string BF_ appears as channel identifier

## if(getRversion() >= "2.15.1") utils::globalVariables(c("cellID",
##                                                        "pos",
##                                                        "flag",
##                                                        "fluor",
##                                                        "bright",
##                                                        "channel"))


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
            fname <- paste(curr_dir, basename, "_", "all", sep = "")
            fname2 <- paste(curr_dir, basename, "_bf_fl_mapping", sep = "")


            ######## ASSERT
            # check that file exists
            if(!file.exists(fname)) {
                stop(paste("Missing file:", fname, "\n Position not loaded\n"))}
            ########


            # rcell had a check for folders with no number, and assigned an ordinal if there
            # wasn't one. I found no use for it, so I removed it


            ####################
            ## reading data
            ####################

            # print position being processed
            cat(gsub("[a-zA-Z_]", "", posdir[i])," ")
            if(i %% 10 == 0) cat("\n")

            pos.data[[pos.index]] <- readr::read_tsv(fname)

            # ToDo: check if there's a difference between using Hmisc::import.cleanup or not.

            ######### ASSERT
            # asserting that previously loaded positions have the same number and column names
            if(length(column.names) == 0){ #first position
                column.names <- names(pos.data[[pos.index]])

            } else { #not first position
                curr_names <- names(pos.data[[pos.index]])

                if(length(curr_names) != length(column.names)) {
                    stop(fname," has different number of colums than previous position\n")
                }

                if(sum(column.names == curr_names) != length(column.names)) {
                    stop(fname," has different column names than previous positions\n")
                }
            }
            ########

            # updates contents of positions loaded.
            loaded.pos <- c(loaded.pos, pos.index)
            loaded.pos.dir[[pos.index]] <- posdir[i]

            #reading output_bf_fl_mapping
            if(file.exists(fname2)) {

                # assuming that all mapping files have the same column types
                bf.fl.mapping[[pos.index]] <- readr::read_tsv(fname2,
                                                              col_types = "ciici")


                #creating flag table
                pos.flag <- .mk.flag.table(bf.fl.mapping[[pos.index]], pos = pos.index)
                flag.table <- dplyr::bind_rows(pos.flag, flag.table)

            } else warning(fname2, "not found")
