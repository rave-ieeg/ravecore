#' @name import-signals
#' @title Import signal data into 'RAVE'
#' @description
#' Import signal data from different file formats; supports \code{'EDF'},
#' \code{'BrainVision'}, \code{'BlackRock'}, \code{'HDF5'}, and \code{'Matlab'}
#' formats under either native or 'BIDS' standard. It is recommended to use
#' 'RAVE' user interfaces to import data.
#' @param subject a 'RAVE' subject or subject ID, consists of a project name, a
#' forward slash, followed by a subject code; for example,
#' \code{'demo/DemoSubject'} refers to a 'RAVE' native subject
#' \code{'DemoSubject'} under the project \code{'demo'}; while
#' \code{'demo@bids:ds001/01'} refers to subject \code{'sub-01'} from the
#' 'BIDS' data set \code{'ds001'}, under its 'RAVE' project \code{'demo'}
#' under the derivative folder.
#' @param blocks recording block; see Section 'Recording Blocks' for details
#' @param electrodes electrode (channels) to import, must be a vector of
#' integers (channel numbers) or a character that can be interpreted as
#' integers; for example, integer vector \code{1:10} stands for the first 10
#' channels, while \code{'1,3-10,15'} refers to channels 1, 3 to 10, then 15.
#' Notice some formats might not have definition of the channel numbers,
#' see Section 'Channel Numbers' for details
#' @param sample_rate sampling frequency of the channel, must be positive.
#' 'RAVE' only accepts unified consistent sample rate across all channels
#' with the same type. For example, if one 'LFP' channel is 2000 Hz, then
#' all 'LFP' channels must be 2000 Hz. Channels with different sample rates
#' will be either \code{\link[ravetools]{decimate}} (if possible, with a
#' 'FIR' filter) or \code{\link[ravetools]{resample}} (fractional ratio) during
#' import. Different channel types (such as 'Spike', 'Auxiliary', ...) can
#' have different sample rates
#' @param add whether the operation is to add new channels; default is
#' false to protect data integrity
#' @param data_type channel signal data type, can be \code{'LFP'},
#' \code{'Spike'}, or \code{'Auxiliary'}
#' @param skip_validation whether to skip data validation, default is false
#' (recommended)
#' @param ... passed to or reserved for other methods
#'
#' @section Recording Blocks:
#'
#' The term "recording block" is defined as a continuous block of signals
#' recorded during the experiment, typically during one run of the
#' experiment, depending on the setups and format standards:
#'
#' In the context of native standard, the raw 'RAVE' data is typically stored
#' in the \code{'~/rave_data/raw_dir'} directory (\code{'~'} stands for your
#' home directory, or documents directory under Windows). Each subject is
#' stored under a folder named after the subject code. For example, subject
#' \code{'DemoSubject'} has a raw folder path
#' \code{'~/rave_data/raw_dir/DemoSubject'}. The block folders are stored under
#' this subject folder (such as \code{'008'}, \code{'010'}, ...). Each
#' block folder contains a 5-min recording from an experiment.
#'
#' In the context of 'BIDS' standard, there is no official definition of a
#' 'block', instead, 'BIDS' has an explicit definition of sessions, tasks,
#' and runs. We typically consider that a combination of a session, a task,
#' and a run consists of a recording block. For example,
#' \code{'ses-01_task-01_run-01'} or \code{'ses-01_run-01'}, depending on
#' the existence of the 'BIDS' entities.
#'
#' @section Channel Numbers:
#'
#' (Electrode) channel numbers refer to vectors of integers, or characters
#' that can be interpreted as integers. For integers, this is straightforward:
#' \code{c(1:10, 21:30)} refers to channel 1 to 20, then 21 to 30. For
#' characters, this is converted to integer internally via an unexported
#' function \code{ravecore:::parse_svec}.
#'
#' The channel numbers must be an integer. In some data formats (such as 'EDF')
#' or some standards ('BIDS'), the channel number is not officially explicitly
#' defined: they use the channel labels as the identifiers. To deal with this
#' situation, 'RAVE' treats the channel order as their numbers. In some cases,
#' this is less ideal because the channel labels might implicitly encode the
#' channel numbers. 'RAVE' will ignore such information for consistent behavior.
NULL
