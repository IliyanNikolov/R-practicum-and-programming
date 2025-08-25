# Problem 1


#1.1
sample_raw_moment_factory <- function(k) {
  
  function(arr, na.rm = FALSE) {
    result <- 0
    len <- length(arr)
    for(n in arr) {
      if(na.rm == TRUE && is.na(n)) {
        len <- len - 1
        next
      }
      if(na.rm == FALSE && is.na(n)) {
        result <- NA
        result
      }
      result <- result + n^k
    }
    if(len == 0) {
      0
    } else {
      result / len
    }
  }
  
}


#1.2
raw_moment_2 <- function(arr, na.rm = FALSE) {
  sample_raw_moment_factory(2)(arr, na.rm)
}


#1.3
calculate_numeric_column_stats <- function(fun) {
  
  function(data_set) {
    purrr::keep(purrr::map_if(data_set, is.numeric, fun), is.numeric)
  }
  
}


#1.4
calculate_raw_moment_2 <- function(data_set) {
  calculate_numeric_column_stats(raw_moment_2)(data_set)
}


#1.5
calculate_raw_moment_2(mtcars)






#Problem 2
library(methods)


#2.1 and 2.2
setClass("Shape",
  slots = c(
    name = "character",
    side_lengths = "numeric"
  ),
  prototype = list(
    name = "",
    side_lengths = numeric(0)
  )
)


#2.3
setValidity("Shape", function(object) {
  if(object@name == "") {
    stop("@name can't be empty")
  }
  if(any(object@side_lengths <= 0)) {
    stop("All sides should be positive")
  }
  TRUE
})


#2.4
Shape <- function(name, side_lengths) {
  new("Shape", name = as.character(name), side_lengths = side_lengths)
}


#2.5
setMethod("show", "Shape", function(object) {
    cat("Shape", "\n",
        "  Name: ", object@name, "\n",
        "  Side Lengths: ", paste(object@side_lengths, collapse = ", "), "\n")
})


#2.6
setGeneric("name", function(shape) standardGeneric("name"))
setMethod("name", "Shape", function(shape) shape@name)

setGeneric("name<-", function(shape, value) standardGeneric("name<-"))
setMethod("name<-", "Shape", function(shape, value) {
  shape@name <- as.character(value)
  validObject(shape)
  shape
})


setGeneric("side_lengths", function(shape) standardGeneric("side_lengths"))
setMethod("side_lengths", "Shape", function(shape) shape@side_lengths)

setGeneric("side_lengths<-", function(shape, value) standardGeneric("side_lengths<-"))
setMethod("side_lengths<-", "Shape", function(shape, value) {
  shape@side_lengths <- value
  validObject(shape)
  shape
})


#2.7
setGeneric("is_rhombus", function(shape) standardGeneric("is_rhombus"))
setMethod("is_rhombus", "Shape", function(shape) {
  if(length(shape@side_lengths) == 4 && length(unique(shape@side_lengths)) == 1) {
    TRUE
  } else{
    FALSE
  }
})



#s1 <- Shape(name = "qwerty", side_lengths = c(1:4))
#s2 <- Shape(name = "asd", side_lengths = c(5,5,5,5))
#is_rhombus(s1)
#is_rhombus(s2)
#s3 <- Shape(name = "", side_lengths = c(1:4))
#s4 <- Shape(name = "qwe", side_lengths = c(1:4, -9))

