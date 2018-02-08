#' @name sanitize_latex
#' @title Escape Characters for Printing in LaTeX Output
#' 
#' @description \code{sanitize_latex} translates particular items in 
#' character strings to LaTeX format, e.g., makes \code{a^2 = a\$^2\$} 
#' for superscript within variable labels. LaTeX names of greek letters 
#' (e.g., "alpha") will have backslashes added if \code{greek==TRUE}. 
#' Math mode is inserted as needed. \code{sanitize_latex} assumes that 
#' input text always has matches, e.g. \code{[) [] (] ()}, and that 
#' surrounding by \$\$ is OK.
#' 
#' @param object \code{character} vector of strings to translate.
#'    Any NAs are set to blank strings before conversion.
#' @param inn \code{character} vector. Additional strings to translate.
#' @param out \code{character} vector the same length as \code{inn}.
#'   This gives the translated value of the corresonding element in 
#'   \code{inn}
#' @param greek \code{logical(1)}. set to \code{TRUE} to have 
#'   \code{sanitize_latex} put names for greek letters in math mode and 
#'   add backslashes.
#' @param pb \code{logical(1)} If \code{pb=TRUE}, \code{sanitize_latex} also 
#'   translates \code{[()]} to math mode using \code{\\left}, \code{\\right}.
#' @param na \code{character(1)} Single character string to translate 
#'   \code{NA} values to.
#' @param ... Additional arguments for other methods. Currently ignored.
#' 
#' @return 
#' Vector of chracter strings.
#' 
#' @author 
#' This code is lifted from the \code{Hmisc} package in order to 
#' avoid depending on that package.
#' 
#' Frank E. Harrell Jr.\cr
#' Department of Biostatistics,\cr
#' Vanderbilt University,\cr
#' f.harrell@@vanderbilt.edu\cr
#' 
#' Richard M. Heiberger,\cr
#' Department of Statistics,\cr
#' Temple University, Philadelphia, PA.\cr
#' rmh@@temple.edu\cr
#' 
#' David R. Whiting,\cr
#' School of Clinical Medical Sciences (Diabetes),\cr
#' University of Newcastle upon Tyne, UK.\cr
#' david.whiting@@ncl.ac.uk\cr
#' 
#' @seealso \code{Hmisc::latexTranslate}, \code{Hmisc::sedit}
#' 
#' @examples 
#' sanitize_latex("75% of the cars were | more than $20,000 Delta = 1.30", greek = TRUE)
#' 
#' @export

sanitize_latex <- function(object, inn=NULL, out=NULL, pb=FALSE,
                           greek=FALSE, na='', ...)
{
  text <- ifelse(is.na(object), na, as.character(object))
  
  inn <- c("|",  "%",  "#",   "<=",     "<",  ">=",     ">",  "_", "\\243",
           "&", inn, 
           if(pb)
             c("[", "(", "]", ")"))
  
  out <- c("$|$", "\\%", "\\#", "$\\leq$", "$<$", "$\\geq$", "$>$", "\\_",
           "\\pounds", "\\&", out, 
           if(pb)
             c("$\\left[", "$\\left(", "\\right]$", "\\right)$"))
  
  text <- sedit(text, '$', 'DOLLARS', wild.literal=TRUE)
  text <- sedit(text, inn, out)
  
  ##See if string contains an ^ - superscript followed by a number
  ## (number condition added 31aug02)
  
  dig <- c('0','1','2','3','4','5','6','7','8','9')
  
  for(i in seq_along(text)) {
    lt <- nchar(text[i])
    x <- substring(text[i], 1 : lt, 1 : lt)
    j <- x == '^'
    if(any(j)) {
      is <- ((1 : lt)[j])[1]  #get first ^
      remain <- x[-(1 : is)]
      k <- remain %in% c(' ',',',')',']','\\','$')
      if(remain[1] %in% dig ||
         (length(remain) > 1 && remain[1] == '-' && remain[2] %in% dig))
        k[-1] <- k[-1] | !remain[-1] %in% dig
      
      ie <- if(any(k)) is + ((1 : length(remain))[k])[1]
      else
        length(x)+1
      
      ##See if math mode already turned on (odd number of $ to left of ^)
      dol <- if(sum(x[1 : is] == '$') %% 2) ''
      else '$'
      
      substring2(text[i],is,ie-1) <-
        paste(dol, '^{', substring(text[i], is + 1, ie - 1), '}', dol,sep='')
    }
    
    if(greek) {
      gl <- c('alpha','beta','gamma','delta','epsilon','varepsilon','zeta',
              'eta','theta','vartheta','iota','kappa','lambda','mu','nu',
              'xi','pi','varpi','rho','varrho','sigma','varsigma','tau',
              'upsilon','phi','carphi','chi','psi','omega','Gamma','Delta',
              'Theta','Lambda','Xi','Pi','Sigma','Upsilon','Phi','Psi','Omega')
      for(w in gl)
        text[i] <- gsub(paste('\\b', w, '\\b', sep=''),
                        paste('$\\\\',w,'$',   sep=''),
                        text[i])
    }
  }
  
  sedit(text, 'DOLLARS', '\\$', wild.literal=TRUE)
}


# UNEXPORTED --------------------------------------------------------

sedit <- function(text, from, to, test=NULL, wild.literal=FALSE)
{
  to <- rep(to, length=length(from))
  for(i in seq_along(text)) {
    s <- text[i]
    if(length(s))
      for(j in 1:length(from)) {
        old <- from[j]
        front <- back <- FALSE
        if(!wild.literal) {
          if(substring(old,1,1) == '^') {
            front <- TRUE;
            old <- substring(old,2)
          }
          
          if(substring(old,nchar(old)) == '$') { 
            back <- TRUE; old <- substring(old, 1, nchar(old)-1)
          }
        }
        
        new <- to[j]
        
        lold <- nchar(old)
        if(lold > nchar(s))
          next
        
        ex.old <- substring(old, 1:lold, 1:lold)
        if(!wild.literal && any(ex.old == '*')) 
          s <- replace.substring.wild(s, old, new, test=test, front=front, back=back)
        else {
          l.s <- nchar(s)
          is <- 1:(l.s-lold+1)
          if(front)
            is <- 1
          
          ie <- is + lold - 1
          if(back)
            ie <- l.s
          
          ss <- substring(s, is, ie)
          k <- ss == old
          if(!any(k))
            next
          
          k <- is[k]
          substring2(s, k, k+lold-1) <- new
        }
      }
    
    text[i] <- s
  }
  
  text
}

'substring2<-' <- function(text, first, last=100000, value)
{
  if(is.character(first)) {
    if(!missing(last))
      stop('wrong # arguments')
    
    return(sedit(text, first, value))  ## value was setto 25May01
  }
  
  lf <- length(first)
  
  if(length(text) == 1 && lf > 1) {
    if(missing(last))
      last <- nchar(text)
    
    last <- rep(last, length=lf)
    for(i in 1:lf) {
      text <- paste(if(first[i]>1) 
        substring(text, 1, first[i]-1),
        value,
        substring(text, last[i]+1), sep='')
      
      if(i < lf) {
        j <- (i+1):lf
        w <- nchar(value) - (last[i]-first[i]+1)
        first[j] <- first[j] + w  
        last[j] <- last[j] +  w
      }
    }
    
    return(text)
  }
  text <- paste(ifelse(first>1,substring(text, 1, first-1),''), value,
                substring(text, last+1), sep='')
  text
}


replace.substring.wild <- function(text, old, new, test=NULL, 
                                   front=FALSE, back=FALSE)
{
  if(length(text)>1)
    stop('only works with a single character string')
  
  if(missing(front) && missing(back)) {
    if(substring(old,1,1) == '^') {
      front <- TRUE;
      old <- substring(old,2)
    }
    
    if(substring(old, nchar(old)) == '$') {
      back <- TRUE
      old <- substring(old, 1, nchar(old)-1)
    }
  }
  if((front || back) && old!='*') 
    stop('front and back (^ and $) only work when the rest of old is *')
  
  star.old <- substring.location(old,'*')
  if(length(star.old$first)>1)
    stop('does not handle > 1 * in old')
  
  if(sum(star.old$first) == 0)
    stop('no * in old')
  
  star.new <- substring.location(new,'*')
  if(length(star.new$first)>1)
    stop('cannot have > 1 * in new')
  
  if(old == '*' && (front | back)) {
    if(front && back)
      stop('may not specify both front and back (or ^ and $) with old=*')
    
    if(length(test) == 0)
      stop('must specify test= with old=^* or *$')
    
    et <- nchar(text)
    if(front) {
      st <- rep(1, et);
      en <- et:1
    } else {
      st <- 1:et;
      en <- rep(et,et)
    }
    
    qual <- test(substring(text, st, en))
    if(!any(qual))
      return(text)
    
    st <- (st[qual])[1]
    en <- (en[qual])[1]
    text.before <- if(st == 1)''
    else substring(text, 1, st-1)
    
    text.after  <- if(en == et)''
    else substring(text, en+1, et)
    
    text.star   <- substring(text, st, en)
    new.before.star <-
      if(star.new$first>1) 
        substring(new, 1, star.new$first-1)
    else ''
    
    new.after.star <- if(star.new$last == length(new))''
    else substring(new, star.new$last+1)
    
    return(paste(text.before, new.before.star, text.star, new.after.star,
                 text.after, sep=''))
  }
  
  old.before.star <- if(star.old$first == 1)''
  else substring(old, 1, star.old$first-1)
  
  old.after.star  <- if(star.old$last == nchar(old))''
  else substring(old, star.old$first+1)
  
  if(old.before.star == '')
    loc.before <- list(first=0, last=0)
  else {
    loc.before <- substring.location(text, old.before.star)
    loc.before <- list(first=loc.before$first[1], last=loc.before$last[1])
  }
  
  if(sum(loc.before$first+loc.before$last) == 0)
    return(text)
  
  loc.after <- if(old.after.star == '') list(first=0, last=0)
  else {
    la <- substring.location(text, old.after.star, 
                             restrict=c(loc.before$last+1,1e10))
    lastpos <- length(la$first)
    la <- list(first=la$first[lastpos], last=la$last[lastpos])
    if(la$first+la$last == 0)
      return(text)
    
    la
  }
  
  loc.star <- list(first=loc.before$last+1, 
                   last=if(loc.after$first == 0) nchar(text)
                   else loc.after$first-1)
  
  star.text <- substring(text, loc.star$first, loc.star$last)
  if(length(test) && !test(star.text))
    return(text)
  
  if(star.new$first == 0)
    return(paste(if(loc.before$first>1)substring(text,1,loc.before$first-1),
                 new, sep=''))
  
  new.before.star <- if(star.new$first == 1)''
  else substring(new, 1, star.new$first-1)
  new.after.star  <- if(star.new$last == nchar(new)) ''
  else substring(new, star.new$first+1)
  
  paste(if(loc.before$first>1)substring(text,1,loc.before$first-1),
        new.before.star,
        substring(text,loc.star$first,loc.star$last),
        new.after.star,
        if(loc.after$last<nchar(text) && loc.after$last>0) 
          substring(text,loc.after$last+1),
        sep='')
}

substring.location <- function(text, string, restrict)
{
  if(length(text) > 1)
    stop('only works with a single character string')
  
  l.text <- nchar(text)
  l.string <- nchar(string)
  if(l.string > l.text)
    return(list(first=0,last=0))
  
  if(l.string == l.text)
    return(if(text == string)
      list(first=1,last=l.text)
      else 
        list(first=0,last=0))
  
  is <- 1:(l.text-l.string+1)
  ss <- substring(text, is, is+l.string-1)
  k <- ss == string
  if(!any(k))
    return(list(first=0,last=0))
  
  k <- is[k]
  if(!missing(restrict))
    k <- k[k>=restrict[1] & k<=restrict[2]]
  
  if(length(k) == 0)
    return(list(first=0,last=0))
  
  list(first=k, last=k+l.string-1)
}


## if(version$major < 5)  14Sep00
substring2 <- function(text, first, last=100000L)
  base::substring(text, first, last)