############# generating humdrum data ####


# Align analyses with humdrum data.
#
# This function takes one or more harmonic analyses
# (output by [analyzeChorales]) and aligns them with the appropriate humdrum
# data. The output is new humdrum data: the original **kern with added harmonic
# analyses spines.
#
# @param analyses A list of analyses output by [analyzeChorales]. (Each vector
# of chord symbols should be the same length as `nrow(ChoraleTable_Slices)`.)
# [analyzeChorales] outputs a single analysis, but multiple analyses can be concatenated and fed to `generateHumdrum`:
# i.e., `generateHumdrum(c(analysis1, analysis2))`
#
# @return A `data.table` with two columns (`Records` and `FileName`) and `nrow(ChoraleTable_Slices)` rows.
# `Records` contains a character vector of new humdrum records.
# `FileName` a character vector indicating the (original) humdrum filename for each record/slice.
#
generateHumdrum <- function(analyses) {
  names(analyses) <- LETTERS[((seq_along(analyses) - 1) %% 26) + 1]

  ChoraleTable_Slices <- cbind(ChoraleTable_Slices, as.data.table(analyses))
  ChoraleTable_Slices[ ,
                       { filename <- unique(Slice_FileName)
                         func <- getandprepHumdrum(filename)
                         anals <- lapply(names(analyses), get, envir = environment())
                         list(Records = func(anals),
                              FileName = filename)},
                       by = Slice_FileNumber] -> humdrum




}

getandprepHumdrum <- function(filename) {
 filename <- system.file('extdata',
                         filename,
                         package = 'FlexibleChoraleHarmonicAnalysis')
 records <- readLines(filename)

 global       <- grepl('^!!', records)
 data         <- grepl('^[^!=*]', records) & grepl('[^r]', gsub('[^A-Ga-gr]*', '', records))
 localnondata <- !data & !global

 firstspine <- gsub('\t.*', '' , records)

 # create the interpretations, barlines, and comments for the new analysis spines
 newSpine <- rep('.', length(records))
 newSpine[global] <- ''

 newSpine[global] <- records[global]
 newSpine[localnondata] <- substr(firstspine[localnondata], start = 1L, stop = 1L)

 globalInterp <- grepl('^\\*\\*', firstspine)
 newSpine[globalInterp] <- gsub('\\*\\*.*', '**chordsymbol', firstspine[globalInterp])

 tandemInterps <- grepl('^\\*[A-Ga-g][#-]*:', firstspine) |
                  grepl('^\\*M[1-9][1-9]*/[1-9][0-9]*', firstspine) |
                  grepl('^\\*met\\(.\\)', firstspine) |
                  grepl('^\\*>', firstspine) |
                  grepl('^\\*-', firstspine) |
                  grepl('^=', firstspine) |
                  grepl('^\\*MM[1-9][0-9]*', firstspine) |
                  grepl('^!..*ian$', firstspine)


 newSpine[tandemInterps] <- firstspine[tandemInterps]

 function(analyses) {
  if (any(lengths(analyses) != sum(data))) stop(paste("Analysis isn't right length for file", filename))

  newSpines <- lapply(analyses,
                      function(anal) {
                        newSpine[data] <- anal
                        newSpine
                      })

  newSpines <- do.call('paste', c(newSpines, sep = '\t'))




  records[!global] <- paste(records[!global], newSpines[!global], sep = '\t')

  paste(records, collapse = '\n')

 }


}


############# writing ####

#' Write annotated humdrum data to files
#'
#' This function writes new humdrum data (original **kern with added harmonic
#' analyses spines) to text files.
#' The original krn file names are used for the output files with the addition an optional label specified by the user and a different extension (\code{.hum}).
#'
#' @param analyses An \code{analyses} object, created by \code{\link{analyzeChorales}}.
#'     [analyzeChorales] outputs a single analysis, but multiple analyses can be concatenated and fed to `writeHumdrum`:
#'     i.e., `writeHumdrum(c(analysis1, analysis2))`
#' @param path A single character string indicating the file path to write the files into.
#'     Defaults to `NULL`, in which case files are written to the current working directory.
#' @param label A single character string to append to each filename.
#'     Defaults to `NULL`, which causes no label to be appended. Use this to differentiate different batches
#'     of analyses you generate.
#'
#' @return A `data.table` with two columns (`Records` and `FileName`) and `nrow(ChoraleTables_Slices)` rows.
#'   `Records` contains a character vector of new humdrum records.
#'   `FileName` a character vector indicating the new humdrum filenames, with the added `path` and `label`,
#'    for each record/slice.
#' @export
writeHumdrum <- function(analyses, path = NULL, label = NULL) {
  fileTable <- generateHumdrum(analyses)

  label <- if (is.null(label)) "" else paste0('_', label)
  fileTable$FileName <- gsub('\\.krn$', paste0(label, '.hum'), fileTable$FileName)

  file.sep <- .Platform$file.sep

  if (is.null(path) || path == '') path <- paste0('.', file.sep)
  if (stringr::str_sub(path, -1, -1) != file.sep) path <- paste(path, file.sep, sep = '')
  fileTable$FileName <- paste(path, fileTable$FileName, sep = '')

  fileTable[ ,
             {Records <- paste(Records,
                              '!!! **chordsymbol annotations generated by the FlexibleChoraleHarmonicAnalysis R package.',
                              '!!! Condit-Schultz, Ju, and Fujinaga, ISMIR (2018)',
                              paste('!!! Created on', date()), sep = '\n')
             writeLines(Records, con = FileName)},
             by = 1:nrow(fileTable)]

  invisible(fileTable)
}


#' @export
print.analysis <- function(analyses) {
  name <- substitute(analyses)
  cat("572 chorales with ", length(analyses), " harmonic ", if (length(analyses) > 1) "analyses" else "analysis",
      ".\n\t\tUse viewHumdrum(composer = ___, chorale = N, analysis = ", name, ") to view analyses.\n",
      "\t\tUse writeHumdrum(", name, ") to export to new humdrum files.", sep = '')
}


#' Inspect a humdrum file from the dataset.
#'
#' This function prints humdrum data in the R console.
#' It can be used to inspect the "raw" humdrum files from the dataset, or to view the annotated humdrum generated by the package.
#' When this function is called, the humdrum file(s) are printed on the screen.
#'
#' @param composer Character string: The name of the desired composer (Bach or Praetorius). Partial matches are allowed; Case insensitive.
#' @param chorale  Numeric (integer): The desired chorale number(s).
#' @param analyses An \code{analyses} object, created by \code{\link{analyzeChorales}}. If \code{NULL} the original humdrum files are printed.
#'     [analyzeChorales] outputs a single analysis, but multiple analyses can be concatenated and fed to `viewHumdrum`:
#'     i.e., `viewHumdrum('bach', 10, c(analysis1, analysis2))`
#'
#' @return A list of character vector: one for each viewed chorale. Each element is a vector of humdrum
#' records/slices.
#'
#' @examples
#' viewHumdrum('Bach', 11)
#' # Prints the 11th Bach chorale, unanalyzed.
#'
#' viewHumdrum('Praetorius', 1:10)
#' # Prints the first 10 Praetorius chorales, unanalyzed.
#'
#' viewHumdrum('Pr', 1:10, analyses = analyzeChorales(rank = list(Chord ~ Count)))
#' # Prints the first 10 Praetorius chorales, with a particular harmonic analysis.
#'
#'
#' @export
viewHumdrum <- function(composer = 'Bach', chorale = 1, analyses = NULL) {
  chorale <- as.integer(chorale)
  chorale <- paste0(strrep('0', 3L - nchar(as.character(chorale))), chorale)

  composer <- c('Bach', 'Praetorius')[pmatch(tolower(composer), c('bach', 'praetorius'))]

  filename <- paste0('Chorales_', composer,'_', chorale, '.krn')

  str <- if (is.null(analyses)) {
    filename <- system.file(paste0('extdata', .Platform$file.sep, filename),
                            package = 'FlexibleChoraleHarmonicAnalysis')

    paste(unlist(lapply(filename, readLines)), collapse = '\n')
  } else {
    humdrumTable <- generateHumdrum(analyses)
    humdrumTable[FileName %in% filename]$Records

  }


  cat(str, sep = '\n')

  invisible(lines)
}


#' Raw humdrum data directory
#'
#' This character string stores the directory where the package's raw humdrum data is stored on your machine.
#'
#' @export
humdrumDirectory <- system.file('extdata', package = 'FlexibleChoraleHarmonicAnalysis')


#' Raw humdrum data paths.
#'
#' This character vector stores the path to all 572 of the package's raw humdrum data files.
#'
#' @export
humdrumPaths <- dir(system.file('extdata', package = 'FlexibleChoraleHarmonicAnalysis'), pattern = 'Chorales_.*\\.krn$', full.names = TRUE)
