#' @title Convert a plot to a TikZ figure
#' @description Convert a R plot to a TikZ figure
#' @param code a function without argument generating the plot
#' @param filename the name of the tex file, without extension
#' @param outdir output directory
#' @param overwrite logical, whether to overwrite if the file exists
#' @param compile logical, whether to compile the tex file; automatically \code{TRUE} if \code{format="eps"}
#' @param format the output format, one of \code{"pdf"}, \code{"ps"}, or \code{"eps"}
#' @param lua logical, wheter to use \code{lualatex}
#' @param packages \code{NULL} to use default packages, otherwise see examples
#' @param addDefaultTikZoptions logical; for \code{format="pdf"} only and if \code{packages} is not \code{NULL}, then this adds the default packages given in \code{getOption("tikzLatexPackages")}
#' @param clean logical, whether to remove the auxiliary files
#' @param ... arguments passed to \code{\link[tikzDevice]{tikz}}, such as \code{documentDeclaration}, \code{width} and \code{height}.
#' @return No value, only prints some messages.
#' @importFrom tikzDevice tikz
#' @importFrom tools texi2dvi
#' @importFrom stringr str_replace
#' @importFrom grDevices dev.off
#' @export
#' @seealso Examples in \code{\link{datify}}.
#' @examples \dontrun{
#' # first write a function creating the plot
#' plotCode <- function(){
#'   curve(x^2, from=-1, to=1, asp=1, axes=FALSE, xlab="$x$", ylab=NA)
#'   xlabs_at <- seq(-1, 1, by=0.5)
#'   axis(1, at=xlabs_at, labels=dollarify()(xlabs_at))
#'   ylabs_at <- seq(0, 1, by=0.2)
#'   axis(2, at=ylabs_at, labels=dollarify()(ylabs_at))
#'   text(0.65, 0.8, "$f(x)=x\\\\^2$") # note the four backslashes!
#'   return(invisible())
#' }
#' # try the plot:
#' plotCode()
#' # create the LaTeX file with default preamble for pdflatex
#' plot2tikz(plotCode, outdir=tempdir(), compile=FALSE,
#'   documentDeclaration ="\\documentclass[12pt]{standalone}\n",
#'   width=7, height=5)
#' # add some packages, or other commands in the preamble
#' plot2tikz(plotCode, compile=FALSE, outdir=tempdir(), overwrite = TRUE,
#'           packages=c("\\usepackage[active,tightpage,psfixbb]{preview}\n",
#'                      "\\PreviewEnvironment{pgfpicture}\n",
#'                      "\\setlength\\PreviewBorder{10pt}\n",
#'                      "\\usepackage{amssymb}\n"),
#'           documentDeclaration ="\\documentclass[12pt]{standalone}\n",
#'           width=7, height=5)}
plot2tikz <- function(code, filename="Rplot", outdir=getwd(),
                      overwrite=FALSE, format="pdf", lua=FALSE,
                      packages=NULL, addDefaultTikZoptions=TRUE,
                      compile=TRUE, clean=FALSE, ...){
  format <- match.arg(format, choices = c("pdf", "ps", "eps"))
  texfile <- paste0(filename, ".tex")
  owd <- setwd(outdir); on.exit(setwd(owd))
  if(overwrite || !file.exists(texfile)){
    #     if(!"packages" %in% names(list(...))){
    #       packages <- getOption("tikzLatexPackages")
    #       extra.args <- list(...)
    #     } else {
    #       extra.args0 <- list(...)
    #       extra.args <- extra.args0[!names(extra.args0) %in% "packages"]
    #       packages <- extra.args0$packages
    #     }
    #     do.call(function(...) tikz(texfile, standAlone=TRUE, onefile=FALSE, packages=packages, ...), extra.args)
    if(is.null(packages)){
      if(format=="pdf") packages <- getOption("tikzLatexPackages")
      if(format %in% c("ps", "eps")) packages <- c("\\thispagestyle{empty}\n", "\\usepackage{tikz}\n")
    } else {
      if(!"\\usepackage{tikz}\n" %in% packages){
        packages <- c("\\usepackage{tikz}\n", packages)
        if(format=="pdf" && addDefaultTikZoptions){
          packages <- union(packages, getOption("tikzLatexPackages"))
        }
      }
    }
    tikz(texfile, standAlone=TRUE, onefile=FALSE, packages=packages, ...)
    code()
    grDevices::dev.off()
  }
  if(compile || format=="eps"){
    message("Compilation...")
    if(format=="pdf"){
      # pdf compilation
      pdffile <- stringr::str_replace(texfile, ".tex", ".pdf")
      if(overwrite || !file.exists(pdffile)){
        if(lua){
          command <- sprintf("lualatex %s", texfile)
          system(command)
        }else{
          tools::texi2dvi(texfile, pdf=TRUE, clean=clean)
        }
        message(sprintf("Output pdf file: %s.pdf", filename))
      }
    } else if(format %in% c("ps", "eps")){
      psfile <- stringr::str_replace(texfile, ".tex", ".ps")
      if(overwrite || !file.exists(psfile)){
        tools::texi2dvi(texfile, pdf=FALSE, clean=clean)
        command <- sprintf("dvips %s.dvi", filename)
        system(command)
        message(sprintf("Output ps file: %s.ps", filename))
        if(format=="eps"){
          command <- sprintf("ps2epsi %s.ps %s.epi", filename, filename)
          system(command)
          file.rename(sprintf("%s.epi", filename), sprintf("%s.eps", filename))
          message(sprintf("Output eps file: %s.eps", filename))
        }
      }
    }
  }
  #
  message(sprintf("Output tex file: %s", normalizePath(texfile, winslash=.Platform$file.sep)))
  return(invisible())
}

# library(tikzDevice)
# library(stringr)
# plot2tikz(fplot, compile=FALSE,
#    documentDeclaration ="\\documentclass[12pt]{standalone}\n",
#    width=5, height=5, bg="black", fg="black")
