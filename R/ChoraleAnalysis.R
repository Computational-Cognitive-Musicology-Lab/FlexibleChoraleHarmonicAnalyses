#' Produce harmonic analyses of chorales
#'
#' This is the master API for the package, which generates specific harmonic
#' analyses of the 571 chorales. You may specify
#' one or more criteria for either \emph{filtering} or \emph{ranking} analyses. These criteria
#' will be used to produced specific harmonic analyses of the 571 chorales, which you may inspect within an R session (\code{\link{viewHumdrum}})
#' or export to new humdrum data files (\code{\link{writeHumdrum}}).
#'
#'
#' # Filtering and Ranking
#'
#' This package contains (internally) precomputed "permutational" analyses of the 571 chorales.
#' These "permutations" are *every* possible legal harmonic analysis of each of the 24,852 "contextual windows" in the [chorale dataset][ChoraleTable_Slices].
#' Most windows are a single chord or slice and have only one possible analysis (13,466 cases).
#' However, other windows are more complicated and can have hundreds of possible analyses (1,546 is the maximum!).
#' Most windows (21,241 out of 24,852) have five or fewer possible analyses.
#' We can extract our "flexible" output analyses by specifying criteria for filtering and/or ranking the various possible analyses.
#'
#' Filtering and ranking criteria are specified as R formulae, using the `~` (tilde) operator.
#' Each formula must be expression that evaluates using the fields of the (hidden) ChoraleAnalyses data object.
#' `ChoraleAnalyses`` contains parsed features about each of the individual harmonic *analysis* of each contextual window in a (shallow) tree format.
#' You will specify criteria for harmonic analyses by referring to the fields in the `ChoraleAnalyses` tree;
#' thus, getting to know the fields/features of the `ChoraleAnalyses` tree is essential to making use of the package.
#'
#' `ChoraleAnalyses` is a nested `list`, so R's `$` operator can be used to step through levels in the tree:
#' Note that some of the field names in the table below end with `$`---these are non-terminal branches of the tree.
#' Terminal branches (actual features you can use for filtering/ranking) are followed by a parenthetical indicating their data type.
#' The top level of the tree divides analysis features into four categories: windows, chords, chord-tones, non-chord tones.
#' The full tree's fields (and their respective data types) are:
#'
#' + `Window$`: Information about the window itself.
#'     + `Duration` (numeric): duration of window in tactus units.
#'     + `Number` (integer): enumeration of window.
#'     + `Slices` (integer): number of slices in the window.
#' + `Chord$`: Information about the chords in the analysis.
#'     + `CompletionDelay$` (list of two):
#'         + In some cases all the notes of a harmony don't appear at the same time, yet we recognize the chord to have "arrived."
#'           The completion delay indicates how long it takes for the "complete" chord to be sounded after the chord arrives.
#'         + `Durations` (list of numeric):
#'             + Completion delay in tactus units.
#'         + `Slices` (list of integer):
#'             + Completion delay in number of slices.
#'     + `Count` (integer): How many chords in the window?
#'     + `Durations` (list of numeric): The duration of the chord(s) in tactus units.
#'     + `Inversions` (list of integer): The inversion of the chord:
#'         + `0`: root position
#'         + `1`: first inversion
#'         + `2`: second inversion
#'         + `3`: third inversion.
#'     + `Qualities` (list of character):
#'         The quality of the chord(s).
#'         Complete triads and 7th chords are indicated:
#'         + `"M"` (major)
#'         + `"m"` (minor)
#'         + `"d"` (diminished)
#'         + `"MM"` (major 7th)
#'         + `"Mm"` (dominant 7th)
#'         + `"mm"` (minor 7th)
#'         + `"dm"` (half-diminished 7th)
#'         + `"dd"` (fully-diminished 7th)
#'
#'         Incomplete triads are indicated:
#'
#'         + `"m(?5)"`: a minor third with missing 5th.
#'         + `"M(?5)"`: a major third with missing 5th.
#'         + `"P5"`: perfect 5th with no 3rd.
#'
#'         Incomplete seventh chords are indicated:
#'
#'         + `"(MM)"`: Apparent `"MM"` but missing 5th; i.e., major 3rd and major 7th are present.
#'         + `"(mm)"`: minor 3rd and minor 7th but missing 5th. Could be `"mm"` or `"dm"`.
#'         + `"(Mm)"`: major 3rd and minor 7th but missing 5th. Only makes sense as incomplete `"Mm"`.
#'         + `"(dm)"`: diminished 5th and minor 7th but missing 3rd. Only makes sense as incomplete `"dm"`.
#'         + `"(dd)"`: diminished 5th and diminished 7th but missing 3rd. Only makes sense as incomplete `"dd"`.
#'         + `"(?m)"`: perfect 5th and minor 7th but missing 3rd. Could be `"Mm"` or `"mm"`.
#'         + `"(??)"`: minor 7th but no fifth *or* third.
#'     + `Roots` (list of character): The root of the chord(s): combinations of the letters A-G with zero or more sharps (`#`) or flats (`-`).
#'        For example, `"G"`, `"A-"`, or `"F##"`.
#'     + `SeventhResolves` (list of logical): A logical (TRUE/FALSE) indicating if 7ths in the chord resolve downward by step (eventually).
#'         If there is no 7th in the chord, value is `NA`.
#'     + `Slices` (list of integer): How many slices per chord?
#' + `CTs$` (chord tones)
#'     + `Contour` (data.table; 11 columns of integers):
#'        How many of each contour type are counted as chord tones, each column being one type.
#'        See the [chorale table][ChoraleTable_Slices] documentation for a list of the types.
#'     + `Count` (integer):
#'          + How many chord tones are there in the window?
#'     + `Durations` (list):
#'          + Duration (in tactus units) of the chord-tones in the analysis. Ordered by voice (soprano to bass), then by slice.
#' + `NCTs$` (non-chord tones)
#'     + `Contour` (data.table; 11 columns of integers):
#'        How many of each contour type are counted as non-chord tones, each column being one type.
#'        See the [chorale table][ChoraleTable_Slices] documentation for a list of the types.
#'     + `Count` (integer):
#'          + How many non-chord tones are there in the window?
#'     + `Durations` (list):
#'          + Duration (in tactus units) of the chord-tones in the analysis. Ordered by voice (soprano to bass), then by slice.
#'
#' (Read the [choraleTable][ChoraleTable_Slices] documentation for additional information about the features.)
#'
#'
#' To illustrate how this works, we can refer to `~ NCTs$Contour$PT` to get the field containing the count of non-chord-passing-tones in each analysis.
#' Similarly, we can access the field indicating the inversion of chords using `~Chord$Inversion`.
#' As a shorthand, you can place some of the tree nodes (including `$`s) on the left-hand side of a formula's `~`:
#' Variables on the right-hand side are then evaluated within the left-hand node.
#' For example, we write `NCTs$Contour ~ PT` instead of `~ NCTs$Contour$PT`.
#' This is most useful when referring to multiple fields within one branch of the tree: for example, `NCTs$Contour ~ PT > NT`
#' is a legal way of using *both* the `NCTs$Contour$PT` and `NCTs$Contour$NT` fields.

#'
#'
#'
#' ## Creating Filters/Rankings
#'
#' You can compose any R expression you want using fields of the `ChoraleAnalyses` tree!
#' This can include arithmetic, comparison operators (`>`, `<`, etc.), or even your own arbitrary functions.
#' Since you have the entire `ChoraleAnalyses` feature tree (above) at your finger tips, you can make a huge variety of
#' possible criteria for selecting the harmonic analyses you prefer.
#' For example, `NCTs$Contour ~ PT > NT` accesses the `NCTs$Contour$PT` and `NCTs$Contour$NT` and asks which analyses
#' have more non-chord-passing tones than non-chord-neighboring tones (using `>`).
#' Used as a rank or filter, this formula would favor analyses where there are more passing tone NCTs than neighboring tone NCTs.
#'
#' Filtering formulae must return logical (`TRUE` or `FALSE`) values: only harmonic analyses for which the filter(s) return `TRUE` are used.
#' Ranking formulae, on the other hand, can return any numeric values: harmonic analysis are ranked from best to worst (high to low rank)
#' according the results of your ranking formulae
#' and the highest rank analysis for each window is used in the output analyses.
#' When analyses are tied in rank you'll just get a more or less random pick from the ties---the solution is to be more specific
#' in your ranking/filtering!
#' Logical values in a ranking formulae are treated as numeric 0 (FALSE) and 1 (TRUE).
#'
#' Watch out!
#' Overly zealous filters can easily lead to some harmonic slices with no legal analysis, which are then marked "X" in the output.
#' Prefer using ranking formulae over filters to avoid this problem---ranking formulae will always return something.
#
#'
#' Having ranking/filtering criteria expressed as R formulae makes it possible to store, combine, and compose criteria,
#' as R formulae can be held in lists.
#' You can concatenate formulae using `c(...)`.
#' For example, if you have ranking formula that you use a lot you could assign it to a variable, like `ccount = Count ~ Chord`.
#' You can then mix `ccount` with other formula by concatenating them: `c(ccount, ~ another criteria, ~ a third criteria, etc)`.
#' You may want to try out various combinations of 4--6 different ranking criteria.
#'
#'
#' If you find this all confusing, that is understandable! Some examples should help:
#'
#' #### More Chords
#'
#' Lets say you prefer analyses that use as many chords as possible (generally minimizing non-chord tones).
#' You could just use a ranking formulae of `rank = Chord ~ Count`.
#' This formula will return the value of `ChoraleAnalyses$Chord$Count`, which are integers indicating how many chords there are in the window.
#' These numbers will be used to rank the analysis, with higher numbers being better---the result will be analyses with as many chords as possible.
#' If you want to be more specific you could add another ranking criteria, minimizing the number of non-chord tones:
#' `rank = c(Chord ~ Count, NCTs ~ Count * -1)`.
#' This will return the number of chords *and* the negative number of non-chord tones---the maximum
#' rank will be the analysis with the most chords *and* the least non-chord tones.
#'
#' #### Fewer chords
#'
#' Lets say you instead want to *minimize* the number of chords, getting a more abstract, "high-level" analysis.
#' You can just reverse the formulae in the last example: `rank = Chord ~ Count * -1`, or `rank = c(Chord ~ Count * -1, NCTs ~ Count)`.
#'
#' ## Lists of X
#'
#' You'll note that many fields of the `ChoraleAnalyses` tree are "lists of" vectors.
#' These are vectors of varying lengths corresponding to the number of chords in each analysis.
#' If, for a particular analysis, `Chord$Count` has a value if `3` that means the window is being analyzed with three chords.
#' Thus, `Chord$Roots` will be a vector of length three, representing the roots of the three chords in order.
#' Similarly, `Chord$SeventhResolves` will be a vector of length three indicating if 7ths in each of the three chords resolve or not (`NA` if there is no 7th).
#' All of these fields have plural names, so be careful: Its `Chord$Durations` not `Chord$Duration`.
#'
#' The package performs some extra magic to help us work with these lists of vectors.
#' If you use calls to `any`, `all`, `max`, `min`, `mean`, or `sum` in your filter/rank formulae
#' they will automatically apply to the nested values.
#' For example, if you say `rank = Chord ~ any(SeventhResolves)`, the analyses will be ranked to prefer analyses where are at least one (i.e., any) of
#' the 7ths in the chords (if any) resolve.
#' Similarly,  `rank = Chord ~ all(SeventhResolves)` will prefer analyses where *all* 7ths resolve.
#' For a finer-grain ranking: `rank = Chord ~ mean(SeventhResolves)` will return the average of the logicals treated as `0`s and `1`s, so you get
#' the proportion of chords sevenths which resolve---more resolving sevenths the better.
#'
#' Another example would be to use `rank = ~ Chord ~ min(Durations) >= 1`, which
#' will prefer analyses where the minimum chord duration is a tactus or greater.
#'
#' If more than one list fields are included in the nested call (to `any`, `all`, etc.), they will all be mapped across in parallel.
#' Non-nested fields will also be mapped across as well.
#' (Note that all these special nested calls are called with the `na.rm = TRUE`.)
#'
#' ### Ranking
#'
#' Another useful function is the [%ranks%] function.
#' The [%ranks%] function is most useful for ranking `Chord$Qualities`.
#' You can also use the standard R [base][cut] function to cut
#' numeric values into ranked categories (set `ordered_result = TRUE`).
#'
#' # Standard Analyses
#'
#' You will find that there are a few ranking criteria that you almost always want to use to get reasonable analyses.
#' These include various combinations of:
#'
#' + `Chord ~ mean(SeventhResolves)`: more sevenths resolving is good!
#' + `Chord ~ min(Qualities %ranks% c('M|m|d|Mm'=4, 'MM|mm|dm|dd' = 2, 0))`: prefer analyses without any
#'    rarer chords.
#' + `NCTs ~ -1 * max(Durations)`: having long-duration non-chord tones is a little weird, so prefer shorter ones.
#'    You might alternatively use `mean(Duration)`.
#' + `Chord ~ abs(mean(Durations) - 1)`: favor analyses where chord durations average close to 1 (the tactus).
#' + `Chord$CompletionDelay ~ max(Durations) * -1`: prefer analyses with short completion delay...i.e., transition directly to complete chords.
#'
#' # Write Output
#'
#' If you want to directly output your analyses to humdrum files, add a `path` (and optional `label`) argument to your call.
#' This will pass the analysis and the `path`/`label` arguments directly to [writeHumdrum].
#'
#'
#' @param rank A single formula or a list of formulae used to rank the harmonic analyses of each contextual window.
#' @param filter A  single formula or a list of formulae used to filter out undesired harmonic interpretations.
#' @param ... If an optional `path` argument is included, the analysis is sent directly to [writeHumdrum], using
#' the `path` (and `label` argument if included) from `...`.
#'
#' @value An \code{analysis} object, which is just a list containing one character vector.
#'     `analyzeChorales` always outputs a single analysis, but since the output is a list, these analyses
#'     can be concatenated to create multi-analyses: i.e., c(analyzeChorales(xyz), analyzeChorales(abc)).
#'     Such multi-analyses can be fed to [writeHumdrum] or [viewHumdrum].
#'
#' @examples
#'
#' analysis1 <- analyzeChorales(Chord ~ Count)
#'
#'
#' viewHumdrum('bach', 10, analysis1)
#' writeHumdrum(analysis1, label = 'analysis1')
#'
#' ###
#'
#' shortNCTs    <- NCTs ~ -max(Durations)
#' triads       <- Chord ~ min(Qualities %ranks% c('M|m|d|Mm'=4, 'MM|mm|dm|dd' = 2, 0))
#' resolved7ths <- Chord ~ mean(SeventhResolves)
#'
#' analysis2 <- analyzeChorales(c(shortNCTs, triads, resolved7ths, Chord$CompletionDelay ~ max(Durations) * -1 ))
#'
#' ###
#'
#' analyses <- c(analysis1, analysis2)
#' viewHumdrum('bach', 10, analyses)
#'
#' writeHumdrum(analyses, label = 'twoAnalyses')
#'
#' @export
analyzeChorales <- function(rank = list(), filter = list(), ...) {
 if (rlang::is_formula(rank))   rank   <- list(rank)
 if (rlang::is_formula(filter)) filter <- list(filter)


 filtered <- filterAnalyses(rank, filter)

 analyses <- list(getChordSymbols(filtered))

 class(analyses) <- c("analysis", class(analyses))

 if (length(list(...)) > 0 && "path" %in% names(list(...))) writeHumdrum(analyses, ...)

 analyses

}

# Filter permutational analyses to select specific analyses
#
# This function odes the heavy lifting for `analyzeChorales`.
# It selects one analysis for each window in the
# \code{\link{PermutationalChoraleAnnotations}} object. It also references
# \code{\link{ChoraleTable_Notes}} and \code{\link{ChoraleTable_Slices}} objects.
#
#
# @param rank A list of formulae
# @param filter A list of formulae
#
filterAnalyses <- function(rank = list(), filter = list()) {
  rank   <- parseFormulae(rank)
  filter <- parseFormulae(filter)

  rank <- lapply(rank, formEval)
  rank <- as.data.table(rank)

  perms <- PermutationalChoraleAnnotations
  perms$RN <- seq_len(nrow(perms))

  #
  if (length(filter) > 0) {
    filter <- do.call('cbind', lapply(filter, formEval))
    filter <- rowSums(filter) == ncol(filter)
    perms$Filter <- filter
    perms <- perms[ ,
                    if (any(Filter, na.rm = TRUE)) {
                      .SD[!is.na(Filter) & Filter ]
                      } else {
                        out <- .SD[1]
                        out$Root[[1]] <- list(rep('X', length(Root[[1]])))
                        out$Quality[[1]] <- list(rep('?', length(Quality[[1]])))
                        out
                        }
                    ,  by = Slice_WindowNumber
                    , .SDcols = c('Root', 'Quality', 'Chord_NumberOf_Slices',
                                  'Slice_WindowNumber', 'Filter', 'RN')]
   rank <- rank[perms$RN]
  }

  if (length(rank) > 0) {
    perms <- perms[do.call('order', c(rank, decreasing = TRUE))]

    perms <- perms[order(perms$Slice_WindowNumber)]


  }

  perms[ , .SD[1], by = Slice_WindowNumber]




}








# Parse formulae used by \code{\link{filterAnalyses}}
#
# This function takes a list of formulae and groups them
# into three categories: "Windows", "Notes", "Slices." Each formula's
# category is determined by the left-side of the formula. If the left side
# is empty, the default is "Windows." Left-sides that don't \code{\link{base::pmatch}}
#  with one of the three categories are removed.
#
#  @param forms A list of formulae.
#
#  @return A list (of length three) of lists of expressions.
parseFormulae <- function(forms) {
 lapply(forms,
        function(form) {
          lh <- rlang::f_lhs(form)
          rlang::f_env(form) <- if (is.null(lh))  {
            prefix <- 'ChoraleAnalyses$'
            ChoraleAnalyses
          } else {
            prefix <- paste0('ChoraleAnalyses$', deparse(lh))
             eval(parse(text = prefix)[[1]])
          }
          prefix <- paste0(gsub('\\$', '\\\\$', prefix), '\\$')

          rlang::f_lhs(form) <- NULL
          rlang::f_rhs(form) <- nestedForms(rlang::f_rhs(form), prefix)
          form
        })
}

formEval <- function(form) {
 eval(rlang::f_rhs(form), envir = rlang::f_env(form))
}


checkForFields <- function(form) {


}

nestedTilde <- function(form) {
  if (!rlang::is_formula(form)) return(form)

  if (!is.null(rlang::f_lhs(form))) {
    form <- rlang::new_formula(call('$', rlang::f_lhs(form), rlang::f_rhs(form)))
  }

  form
}

nestedForms <- function(expr, prefix) {
  if (!is.call(expr)) return(expr)

  call <- deparse(expr[[1]])

  if (call %in% c('max', 'min', 'sum', 'all', 'any', 'mean')) {
    fields <- findFields(expr, prefix)

    subexpr <- parse(text = gsub('\\$', '.', deparse(expr[[2]])))[[1]]

    args <- setNames(alist(x = )[rep('x', length(fields))], paste0(gsub('\\$', '.', fields)))

    rlang::expr({
      f <- rlang::new_function(alist(!!!args),
                               quote({
                                 if (any(lengths(list(!!!(rlang::syms(names(args))))) == 0)) return(NA)
                                 (!!expr[[1]])(!!subexpr, na.rm = TRUE)

                                 }))

      unlist(Map(f, !!!rlang::parse_exprs(fields)))


    })

  } else {
    if (!call %in% '$') {
      for (i in 2:length(expr)) {
        expr[[i]] <- Recall(expr[[i]], prefix)

      }
    }


    expr
  }

}


findFields <- function(expr, prefix){
    dexpr <- deparse(expr)

    if (length(expr) == 1) {
      fields <- c(paste0('Window$', names(ChoraleAnalyses$Window)),
                  paste0('Chord$', c(Filter(function(nam) nam != 'CompletionDelay', names(ChoraleAnalyses$Chord)),
                                     paste0('CompletionDelay$', names(ChoraleAnalyses$Chord$CompletionDelay)))),
                  paste0('CTs$', c(Filter(function(nam) nam != 'Contour', names(ChoraleAnalyses$CTs)),
                                   paste0('Contour$', names(ChoraleAnalyses$CTs$Contour)))),
                  paste0('NCTs$', c(Filter(function(nam) nam != 'Contour', names(ChoraleAnalyses$NCTs)),
                                    paste0('Contour$', names(ChoraleAnalyses$NCTs$Contour))))
      )
      fields <- gsub(prefix, '', paste0('ChoraleAnalyses$', fields))

      if (dexpr %in% fields) return(dexpr)
    } else {
      unlist(lapply(expr[-1], findFields, prefix = prefix))
    }

}

#' Apply arbitrary ranks to values
#'
#' The `%ranks%` infix function allows you to apply arbitrary ranks to an input vector.
#' This is mainly used with the `Chord$Qualities` field.
#'
#' Each of the names of the right-side `patterns` argument is treated as one or more character strings to match exactly the left-side input vector `x`.
#' The output is assigned the value of the last `pattern` that matches.
#' Multiple patterns can be given the same weight by putting them in the same name, but separated by `"|"`.
#'
#' If one, unnamed, pattern value is included on the right-side, it is used as the default value for anything that matches no patterns.
#'
#' @param x A vector. Usually the `Chord$Qualities` field. This field will always be coerced to a character vector.
#' @param patterns A named vector of numbers.
#' @examples
#'
#' test <- c('M', 'm', 'd', 'Mm', 'mm', 'MM', 'P5')
#'
#' test %ranks% c('M' = 5, 'm|d' = 4, 'Mm' = 3, 'mm|MM' = 2, 0)
#'
#' # returns: c(5, 4, 4, 3, 2, 2, 0)
#'
#'
#' analyzeChorales(Chord ~ min(Qualities %ranks% c('M' = 5, 'm|d' = 4, 'Mm' = 3, 'mm|MM' = 2, 0)))
#'
#' @rdname ranks
#' @export
`%ranks%` <- function(x, patterns = c('M|m|d|Mm' = 3, 'mm|MM' = 2)) {
  if (is.null(names(patterns))) stop("The patterns argument of the ranks function must have at least one named value.")

  nomatch <- if (any(names(patterns) == "")) patterns[names(patterns) == ""][1] else 0L
  patterns <- patterns[names(patterns) != ""]

  output <- rep(nomatch, length(x))
  if (length(patterns) == 0L) return(x)

  pats <- strsplit(names(patterns), split = '\\|')
  for (i in 1:length(patterns)){
  matches <- lapply(pats[[i]], grepl, x = x)

    output[Reduce('|', matches)] <- patterns[i]
  }

  output
}



# Extracts vector of chord symbols from annotation table
#
getChordSymbols <- function(DTpermutations) {
  DTpermutations[ , rep(paste(Root[[1]], Quality[[1]], sep = ''),
                        Chord_NumberOf_Slices[[1]]),
                  by = .(1:nrow(DTpermutations))]$V1
}

