diversity <- function(
    x, 
    type = c("effective.number", "richness", "shannon", "simpson", 
             "gini.simpson", "unb.gini", "eveness.simpson", "eveness.pielou",
             "inv.simpson", "renyi", "hill"),
    q = NULL
) {
  type <- match.arg(type)
  
  if(type %in% c("renyi", "hill")) {
    if(is.null(q)) stop("if type is 'renyi' or 'hill', then 'q' must be specified")
    if(!is.numeric(q)) stop("'q' must be numeric")
    if(length(q) != 1) stop("'q' must be a vector of length 1")
    if(q < 0) stop("'q' must be >= 0")
  }
  
  .hill <- function(p, q) {
    if(sum(p) == 0) return(0)
    if(q == 1) {
      exp(-sum(p * log(p), na.rm = TRUE)) 
    } else {
      sum(p ^ q, na.rm = TRUE) ^ (1 / (1 - q))
    }
  }
  
  .diversity <- function(x, type, q) {
    x <- x[!is.na(x)]
    p <- if(length(x) == 0) {
      0
    } else if(is.numeric(x)) {
      if(sum(x) == 0) 0 else x / sum(x) 
    } else {
      proportions(table(x))
    }
    p <- p[p > 0]
    
    switch(
      type,
      richness = .hill(p, 0),
      effective.number = .hill(p, 1),
      shannon = log(.hill(p, 1)),
      eveness.simpson = log(.hill(p, 1)) / .hill(p, 0),
      eveness.pielou = log(.hill(p, 1)) / log(.hill(p, 0)),
      simpson = 1 / .hill(p, 2),
      inv.simpson = .hill(p, 2),
      gini.simpson = 1 - (1 / .hill(p, 2)),
      unb.gini = length(x) * (1 - (1 / .hill(p, 2))) / (length(x) - 1),
      renyi = log(.hill(p, q)),
      hill = .hill(p, q)
    )
  }
  
  if(is.matrix(x)) {
    apply(x, 2, .diversity, type = type, q = q)
  } else if(is.vector(x)) {
    .diversity(x, type = type, q = q)
  } else stop("'x' must be a matrix or vector")
}