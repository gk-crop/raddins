#' raddins - Addins for RStudio
#'
#'
#'
#'
#'
#'
NULL

#' Replaces selected text in actual source editor
#'
#' @param search text to be replaced
#' @param replace replacement text
#'
replaceSelectedText <- function(search, replace)
{
  ec <- rstudioapi::getSourceEditorContext()
  newtext <- gsub(search, replace, ec$selection[[1]]$text)
  rstudioapi::modifyRange(
    ec$selection[[1]]$range,
    newtext,
    id=ec$id
  )
}

#' Converts single backslashes from windows path to forward slashes
#'
#' @export
ConvertWindowsPath <- function()
{
  replaceSelectedText("\\\\", "/")
}

#' Converts double quotes to single quotes
#'
#' @export
ToSingleQuotes  <- function()
{
  replaceSelectedText('"', "'")
}

#' Converts single quotes to double quotes
#'
#' @export
ToDoubleQuotes  <- function()
{
  replaceSelectedText("'", '"')
}
