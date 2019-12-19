#' hdf5_name_from_pars
#'
#' This function allows you create the file name from parameters.
#' @param pars Cluster parameters Age BinaryFraction SigmaHB.
#' @keywords parss
#' @export
#' @examples
#' hdf5_name_from_pars()

hdf5_name_from_pars <- function(pars) #parameter separated by '_' to find the rigth grid in synth folder
{
    paste(pars, collapse = "_")   # output: 'pars1_pars2_pars3'
}
