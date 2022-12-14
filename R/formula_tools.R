#' Create formulas using `glue` syntax
#'
#' This function is specifically designed for dynamically evaluating formulas with time indices. As such, it will delete any formula elements referring to times less than 0.
#'
#' @param string chr. Expressions enclosed in braces will be evaluated as R code (see `?glue`).
#' @param ... additional arguments. Here you can pass variables to be evaluated in enclosed expressions in `string`.
#'
#' @return
#' @export
#'
#' @examples
#'
#' glue_formula('~L{t}*L{t-1}+L{t-2}', t=2)
#' glue_formula('~L{t}*L{t-1}+L{t-2}', t=1)
#'
glue_formula = function(string, glue_vars) {

  trimmed_string = stringr::str_remove_all(string, '[:space:]')
  glued_trimmed_string = glue::glue(string, .envir = glue_vars)

  remove_negatives = function(glued_string) {
    #this is to get rid of references to variables before time 0
    result = glued_string %>%
      stringr::str_remove_all('\\+[:alpha:]+-[:digit:]+') %>%#first remove those starting with +
      stringr::str_remove_all('\\*[:alpha:]+-[:digit:]+') %>%#or a *
      stringr::str_remove_all('\\:[:alpha:]+-[:digit:]+') %>%#etc.
      stringr::str_remove_all('[:alpha:]+-[:digit:]+') %>%
      stringr::str_remove_all('\\+[:alpha:]+[:digit:]+-[:digit:]+') %>%#first remove those starting with +
      stringr::str_remove_all('\\*[:alpha:]+[:digit:]+-[:digit:]+') %>%#or a *
      stringr::str_remove_all('\\:[:alpha:]+[:digit:]+-[:digit:]+') %>%#etc.
      stringr::str_remove_all('[:alpha:]+[:digit:]+-[:digit:]+')

    if(substr(result, nchar(result), nchar(result)) == '~') result = paste0(result, '1') #all variables were before time 0 so were removed, need intercept.

    return(result)
  }

  positive_glued_trimmed_string = remove_negatives(glued_trimmed_string)

  return(as.formula(positive_glued_trimmed_string))
}





#' Create formulas by expanding out string vector arguments
#'
#' @param formula An R formula containing terms referencing objects defined in the parent environment or in ... Terms must each consist of a single lowercase or uppercase letter.
#' @param ... Variables referenced in `formula`. Can be glue-style, with glue terms also defined in ... (see examples)
#'
#' @return chr.
#' @export
#'
#' @examples
#' expand_formula(~a:b + d, a=c('a1','a2'), b=c('b1','b2'), d='d1')
#'
#' #we could have equivalently defined any of the variables in the formula in the calling environment:
#' a=c('a1','a2')
#' b=c('b1','b2')
#' d='d1'
#' expand_formula(~a:b + d)
#'
#' #variables can be specified in glue-style
#' expand_formula(~a:b, a=c('a{h}', 'a{h+1}'), b='a{h}', h=1)
expand_formula <- function(formula, ...) {

  term_maker = function(letter) {
    if(! letter %in% c(letters, LETTERS)) stop('formula terms must be a-z or A-Z')
    letter_object = eval(parse(text = letter), envir = list(...))
    if (length(letter_object) > 1) {
      return( paste0('(', paste(letter_object, collapse= " + "), ")") )
    } else {
      return ( letter_object )
    }
  }

  all_terms = attr(terms.formula(formula), 'term.labels')
  formula_string = paste(stringr::str_replace_all(all_terms, '[a-z]', term_maker), collapse=" + ")
  return ( as.character(glue( formula_string, .envir = ... )) )
}
