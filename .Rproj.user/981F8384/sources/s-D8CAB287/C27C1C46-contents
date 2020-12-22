#' Flexible Chorale Harmonic Analysis
#'
#' This package is a "flexible" dataset of harmonic analyses of baroque chorale music by J.S. Bach and M. Praetorius.
#' What makes it "flexible" is that it doesn't just include ONE analysis of the chorales, but a system for generating
#' countless valid analyses of the chorales, based on constraints you choose.
#'
#' This package is a companion to the paper presented at the International
#' Society of Music Information Retrieval (ISMIR) 2018 conference entitled "A Flexible Approach to Automated
#' Harmonic Analysis: Multiple Annotations of Chorales by Back and Praetorius" (Condit-Schultz, Ju, and Fujinaga).
#' See the paper \url{http://ismir2018.ircam.fr/doc/pdfs/283_Paper.pdf} for more details!
#'
#' @section Raw Data:
#'
#' The \code{FlexibleChoralHarmonicAnalysis} package includes lots of useful data.
#' The original 572 "raw" humdrum/kern files, representing 571 chorales, are included in the package: 371 chorales by Bach and 200 by Praetorius.
#' (Praetorius' 130th chorale is divided between two files, resulting in one additional file compared to the number of chorales).
#' You can inspect individual files using the \code{\link{viewHumdrum}} command.
#' The directory where the raw data files are located on your computer is contained in the \code{\link{humdrumDirectory}} variable.
#' The actual paths to all 572 files are stored in the \code{\link{humdrumPaths}} variable.
#'
#' In addition to the "raw" data, the package contains various features that have already been parsed from the humdrum data.
#' The parsed data comes in two equivalent forms, "by slice" and "by note", stored in the \code{\link{ChoraleTable_Slices}} and \code{\link{ChoraleTable_Notes}} data.table objects respectively.
#'
#' @section Generating Harmonic Analyses:
#'
#' To generate specific harmonic (roman numeral) analyses of the 572 chorales, use the function \code{\link{analyzeChorales}}.
#' Once you've generated analyses, you can view them with the \code{\link{viewHumdrum}} command, or write them to new
#' humdrum files using the \code{\link{writeHumdrum}} command.
#'
#'
#'
#'
#' @keywords internal
#' @docType package
#' @name FlexibleChoraleHamonicAnalysisPackage
"_PACKAGE"

#' Features parsed from 572 chorale **kern files
#'
#' The FlexibleChoraleHarmonicAnalysis package's "ChoraleTables" contain musical features parsed from the 572 "raw" chorale (**kern) files.
#' These features are used as the basis for the harmonic analyses.
#' The package actually defines two "ChoraleTable" objects: \code{ChoraleTable_Slices} and \code{ChoraleTable_Notes}.
#' These tables contain the same information, parsed "by slice" and "by note" respectively.
#'
#' @section Slice vs Note:
#'
#' A "slice" refers to the set of notes (or rests) occuring at the same time in a chorale.
#' A new slice is formed whenever any voice in the chorale articulates a new onset (or rests).
#' (Each data record in the original humdrum data is one slice).
#' There are 42,928 slices in the 571 chorales, corresponding to the 42,928 rows of the \code{ChoraleTable_Slices} table.
#' These slices do not all contain/represent the same number of notes: there are 567 four-voice chorales (370 by Bach) and 4 five-voice chorales (1 by Bach).
#' In addition, Praetorius' 130th chorale is divided between a three-voice part and a four-voice response.
#' In total, the 42,928 slices in \code{ChoraleTable_Slices} include: 99 three-note slices, 42,527 four-note slices, and 302 five-note slices.
#' As a result, many columns in the \code{ChoraleTable_Slices} table are actually lists of vectors varying in length from three to five (details below).
#'
#' The \code{ChoraleTable_Notes} table contains the same information as the the \code{ChoraleTable_Slices} table, except each row represents a single note or rest.
#' There are 171,915 notes/rests in the data, corresponding to the 171,915 rows of the \code{ChoraleTable_Notes} table.
#'
#' Each of the 42,928 slices and 171,915 notes in the dataset are enumerate in the \code{Slice_Number} and \code{Note_Number} fields.
#' This enumeration begins with the first slice/note of the Bach Chorales, proceeds through each Bach chorale in turn (following their numbering), and then proceeds with the first slice/note of
#' the first Praetorius Chorale, etc.
#' Alignment between the \code{ChoraleTable_Notes} and \code{ChoraleTable_Slices} tables can be determined using the \code{Slice_Number} and \code{Note_Number} fields.
#'
#' @section Windows and Phrases:
#'
#' The Bach chorales indicate phrase boundaries using fermata marks.
#' The Praetorius chorales indicate phrases using barlines.
#' In total, the dataset consists of 3,225 phrases: 2,277 phrases in the Bach; 948 phrases in the Praetorius.
#'
#' The 42,928 slices in the chorales are also parsed into 24,852 "contextual windows."
#' These windows were identified through a combination of programmatic rules and expert hard-coding of some edge cases.
#' These windows vary in "size" from 1 to 10 slices---or in more musical terms, from half a tactus
#' (usually an 8th note) to 32 tactus beats (four measures).
#' The vast majority (17,200) of windows are one tactus in length.
#' The vast majority of windows are either one slice (13,276), two slices (8,154), three slices (1,544), or four slices (1,142).
#'
#' All the package's "flexible" harmonic analysis take place within these windows.
#' See the paper \url{http://ismir2018.ircam.fr/doc/pdfs/283_Paper.pdf} for more details.
#' The window enumeration is indicated in the \code{Slice_WindowNumber} field of the chorale tables.
#'

#' @section Fields:
#'
#' The chorale tables each have 40 columns of information, parsed from the raw chorale (**kern) scores.
#'
#' Twenty-three columns (prefixed "Slice_") contain features that pertain
#' to an entire slice---for instance, the metric position.
#' (These data points are duplicated for each note in each slice in the \code{ChoraleTable_Notes} table.)
#' The remaining seventeen columns (prefixed "Note_") contain features specific to individual notes.
#' In the \code{ChoraleTable_Slices} table, these 17 columns actually contain lists of nested vectors varying in length from three to five (depending on the number of voices/notes in the slice):
#' The first element in each vector represents the highest voice (e.g., the soprano) in the slice, with the last element representing the lowest voice (e.g., the bass).
#' All 40 columns of the \code{ChoraleTable_Notes} table are vectors not lists.
#'
#' The twenty-three columns of slice-specific features are:
#'
#' \describe{
#' \item{Slice_Composer}{Character: Either "Bach" or "Praetorius."}
#' \item{Slice_Duration}{Numeric: Duration of slice in tactus-beat units.}
#' \item{Slice_Duration}{Numeric: Duration, or "offset," of slice from the first downbeat of the chorale, in tactus units. Pickups at the beginning of a piece receive negative values.}
#' \item{Slice_FileName}{Character: Name of chorale humdrum file.}
#' \item{Slice_FileNumber}{Integer: The chorale-file "number," as enumerated from 1 to 572. 1--371 are Bach. 372--572 are Praetorius.}
#' \item{Slice_isNewWindow}{Logical: Is this slice the beginning of one of the 24,852 "contextual windows" used in the analysis process?}
#' \item{Slice_Measure}{Integer: The measure number within the chorale. Pickup notes are in measure 0.}
#' \item{Slice_Measure}{Character: The mensuration sign, if any, included in the humdrum score.
#' Values are "[2]", "[3]", "[3/2]", "[c]", "3", "c", "C", "c[3]", "c|", "c|2", "c|3", "c2", or "c3." This field is \code{NA} for all slices in scores that have no mensuration sign.}
#' \item{Slice_Meter}{Character: The time signature indicated in the humdrum score. Values are "2/1", "2/2", "3/1", "3/2", "3/4", and "4/4."}
#' \item{Slice_MetricLevel}{Numeric: The "metric level" of each slice indicates the duration of the highest metric beat that the slice lands on, in tactus-note units. For instance, the
#' downbeat of a 4/4 measure corresponds to a whole-note beat, and thus has a metric level of 4. Eighth-note offbeats have a metric level of 0.5. The third beat in a triple meter is accorded a metric level 1.1.}
#' \item{Slice_Metric_Position}{Numeric: The duration from the beginning of the measure, in tactus units. The first slice in each measure has a Metric_Position of 0.}
#' \item{Slice_Number}{Integer: The slice's "number," enumerated from the beginning of the first Bach chorale through the rest of the Bach chorales,
#'  then proceeding through the Praetorius chorales. The maximum value is 42,928.}
#' \item{Slice_NumberOf_NewPitchClasses}{Integer: How many pitch classes (i.e., pitches regardless of octave) didn't appear in the previous slice of the chorale? If a pitch class that wasn't present before appears more than once,
#' it is counted as many times as it appears. The first slice of each chorale always counts all the notes present.}
#' \item{Slice_NumberOfNewPitchClasses_Unique}{Integer: How many unique pitch classes (i.e., pitches regardless of octave) didn't appear in the previous slice of the chorale? If a pitch class that wasn't present before appears more than once,
#' it is counted only once. The first slice of each chorale always counts all the unique notes present.}
#' \item{Slice_NumberOf_Onsets}{Integer: How many note onsets occur in the slice? This excludes rests and notes that are tied from the previous slice.}
#' \item{Slice_NumberOfSoundingNotes}{Integer: How many notes are being sounded? This only excludes rests.}
#' \item{Slice_NumberOfVoices}{Integer: How many voices are present in this chorale, whether or not they are singing at this moment?}
#' \item{Slice_NumberOfVoices_Active}{Integer: How many voices are "active" in the chorale during this passage, even if they might be resting during this slice?
#' This feature is useful for a view chorales (mostly by Praetorius) which engage in antiphony (e.g., call and response) between voices.
#' We may want to register that some of the voices are still "active" when they rest briefly in between phrases.
#' }
#' \item{Slice_PhraseBoundary}{Logical: Is this slice the boundary at the end of a phrase? Phrases are indicated by fermatas (Bach) and barlines (Praetorius).}
#' \item{Slice_Phrase_Number}{Integer: The number of the current phrase, enumerated from the beginning of the Bach chorales and continuing through the Praetorius.
#' There are a total of 3,225 phrases in the dataset.}
#' \item{Slice_Record}{Integer: The record (line) number of the slice in it's original humdrum file.}
#' }
#'
#' The seventeen columns of note-specific features are:
#'
#' \describe{
#' \item{Note_Accented_Relative}{Logical: Is this note rhythmically accented relative to the previous note? Tied notes inherit their status from their onset.
#' Rests have a value of <NA>. }
#' \item{Note_CircleOfFifths}{Integer: The pitch-classes position in the line of fifths; i.e., C = 0, G = 1, D = 2, F# = 6, Eb = -3.}
#' \item{Note_Duration}{Numeric: Note's duration in tactus units.}
#' \item{Note_Duration_Remaining}{Numeric: Note's duration in tactus units (redundant).}
#' \item{Note_ExtendsPastNextBeat}{Logical: Does this note sustain past the next beat stronger than the beat it attacked on?}
#' \item{Note_isLongerThanSlice}{Logical: Does this note sustain longer than the duration of the current slice? }
#' \item{Note_isNewPitchClass}{Logical: Is this note a pitch class (i.e., pitch regardless of octave) a pitch class which wasn't sounding in the previous slice? }
#' \item{Note_isOnset}{Logical: Is this note an onset? Excludes rests and ties.}
#' \item{Note_isRest}{Logical: Is this note a rest? There are 904 rests in total in the dataset. }
#' \item{Note_Metric_Level_Relative}{Integer: What is the metric level of this note (see the \code{Slice_MetricLevel} definition above) compared to the metric level of the previous note?
#' Values are -1 (note is lower than previous note), 0 (note is same level as previous note), 1 (note is higher than previous note). Rests and notes are phrase boundaries are <NA>.}
#' \item{Note_Number}{Integer: Notes are enumerated from highest voice to lowest voice within each slice, from the beginning of the first Bach chorale through the end of the last Praetorius
#' Chorale. The maximum value is 171,915. Rests are numbered too!}
#' \item{Note_PotentialNonChordToneType}{Character: A string indicating what type of non-chord-tone this note COULD be. Notes which do not match ANY possible non-chord pattern are <NA>.
#' Options are: `"Ant"` (anticipation), `"App"` (appogiatura), `"Camb1"` (first note of cambiatta), `"Camb2"` (second note of cambiatta), `"CT"` (chord tone; this note cannot legally be a non-chord tone),
#' `"DPT1"` (dirst note of double passing tone), `"DPT2"` (second note of double passing tone), `"Esc"` (escape tone), `"Inc"` (incomplete neighbor), `"NT"` (neighbor tone), `"NTchrom"` (chromatic neighbor tone),
#' `"Ped"` (pedal tone), `"PT"` (passing tone), `"Ret"` (retardation), `"Sus"` (suspension), `"Sus+"` followed by `"SusInc"` (a suspension decorated by a lower incomplete neighbor to the resolution),
#' `"syncNT"` (syncopated neighbor), and `"syncPT"` (syncopated passing tone).
#' }
#' \item{Note_Semits}{Integer: The note's pitch in semitones from middle-C; i.e., middle-C = 0, and the G at the top of the treble clef = 19. Rests are <NA>.}
#' \item{Note_TonalName}{Character: The "tonal name" of the pitch regardless of octave. Often referred to as the "pitch class" in this documentation.
#'  Tonal names are combinations of one of seven letter names (A, B, C, D, E ,F, G) and zero or more accidentals (\code{#} = sharp, \code{-} = flat). Rests are <NA>.}
#' \item{Note_VoiceLeading_Approach}{Integer: What is the interval (in semitones) between the previous note and the current note? If the previous note is G5 and the current
#' note is C5, than the VoiceLeading_Approach is \code{-7}. Rests are <NA>.}
#' \item{Note_VoiceLeading_Departure}{Integer:  What is the interval (in semitones) between the current note and the next? If the current note is G5 and the next
#' note is C5, than the VoiceLeading_Approach is \code{-7}. Rests are <NA>.}
#' \item{Note_VoiceNumber}{Integer: Which voice, numbered from high to low, is this note sung by. For instance, the Tenor voice in a four voice chorale would be VoiceNumber = 3.}
#' }
#'
#' Note that the "tactus" for each chorale was expertly labeled, and all durations are encoded relative to the tactus.
#' In the vast majority of Bach cases, the tactus is the quarter-note, so the duration units are quarter notes.
#' However, some Bach chorales and many Praetorius chorales have a half-note tactus, so 1 = half note.
#'
#'
#'
#'
#'
#' @name ChoraleTable
"ChoraleTable_Slices"



#' @name ChoraleTable
"ChoraleTable_Notes"

# Numerous harmonic analyses of the 572 chorales.
#
# This data table contains numerous permutational analyses
# of the 572 chorales. The columns \code{Slice_WindowNumber}
# and \code{Slice_Number} correspond with the same columns in the
#  \code{\link{ChoraleTable_Notes}} and \code{\link{ChoraleTable_Slices}}
# objects. The \code{CT_Note_Number} and \code{NCT_Note_Number} columns
# contain lists of integers which correspond to the \code{Note_Number} column
# in the ChoraleTable_* objects.
#
# @format A data table with 98,453 rows and 12 columns.
"PermutationalChoraleAnnotations"

#' @rdname analyzeChorales
"ChoraleAnalyses"
