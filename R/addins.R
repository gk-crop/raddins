#' raddins - Addins for RStudio
#'
#' Some Addins for RStudio like conversion from backslashes to
#' forward slashes or double to single quotes.
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

#' Converts single backslashes to forward slashes (e.g. in Windows paths)
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
