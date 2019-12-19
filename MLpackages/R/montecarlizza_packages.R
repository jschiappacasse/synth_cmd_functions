#' hdf5_name_from_pars
#'
#' This function allows you create the file name from parameters.
#' @param pars Cluster parameters Age BinaryFraction SigmaHB.
#' @keywords parameters
#' @export
#' @examples
#' hdf5_name_from_pars(c(1,23,2))
 
hdf5_name_from_pars <- function(pars) #parameter separated by '_' to find the rigth grid in synth folder
{
  paste(pars, collapse = "_")   # output: 'pars1_pars2_pars3'
}

#' addslash_ifneeded
#'
#' This function allows you add slash symbol, if it is needed, to the path of folder which contains the hdf5 file.
#' @param folder folder where is located the hdf5 file.
#' @keywords folder
#' @export
#' @examples
#' addslash_ifneeded(folder="synth_cmd/synth/")

addslash_ifneeded <- function(folder) # add '/' to the folder if it is needed 
{
  if(!endsWith(folder,"/")) return(paste(folder, "/", sep=""))
  return(folder)
}


#' hdf5_file_from_pars
#'
#' This function allows you create a path to the .hdf5 file in the folder given the parameters in pars variable. You have to indicate parameters and folder.
#' @param folder folder where is located the hdf5 file.
#' @keywords folder
#' @export
#' @examples
#' hdf5_file_from_pars(c(1,23,2),folder="synth_cmd/synth/")

hdf5_file_from_pars <- function(pars, folder) #create a path to the .hdf5 file in the folder given the parameters in pars variable. This path doesn't exist necessarilly.
{
  paste(c(addslash_ifneeded(folder), hdf5_name_from_pars(pars), ".hdf5"), collapse = "") # output: 'synth_cmd/synth/pars1_pars2_pars3.hdf5'
}

#' read_from_hdf5
#'
#' This function allows you read the hdf5 file needed.
#' @param pars Cluster parameters Age BinaryFraction SigmaHB.
#' @keywords pars
#' @export
#' @examples
#' read_from_hdf5(c(1,23,2))

read_from_hdf5 <- function(pars)
{
  file_name <- hdf5_file_from_pars(pars) #path to the file
  if(file.exists(file_name)) return(h5read(hdf5_file_from_pars(pars),hdf5_name_from_pars(pars))) #read  the .hdf5 file
  stop(paste("hdf5 file does not found,", file_name))
}


#' pars_from_hdf5_file
#'
#' This function allows you get the cluster parameters from the hdf5 file name.
#' @param pars Cluster parameters Age BinaryFraction SigmaHB.
#' @keywords pars
#' @export
#' @examples
#' pars_from_hdf5_file("3.33_45_0.02.hdf5")

pars_from_hdf5_file <- function(name)
{
  split <- strsplit(str_sub(name,0, str_length(name)-5),"_")[[1]]
  c(name, sapply(split, as.numeric))
  #strsplit(strsplit(name, ".")[[1]][1], "_")[[1]]
}

#' list_hdf5_file
#'
#' This function allows you get list of hdf5 file in the folder.
#' @param folder folder where are located the hdf5 files.
#' @keywords folder, hdf5 files.
#' @export
#' @examples
#' list_hdf5_file(folder="synth_cmd/synth")

list_hdf5_file <- function(folder) #list of .hdf5 file in the folder
{
  files <- list.files(addslash_ifneeded(folder))
  return(files)
}

#' pars_to_name_diccionary
#'
#' This function allows you create a diccionary of parameters.
#' @param folder folder where are located the hdf5 files.
#' @keywords folder, hdf5 files.
#' @export
#' @examples
#' pars_to_name_diccionary(folder="synth_cmd/synth")

pars_to_name_diccionary <- function(folder)
{        
  t(data.frame(sapply(list_hdf5_file(addslash_ifneeded(folder)),pars_from_hdf5_file)))
  
}