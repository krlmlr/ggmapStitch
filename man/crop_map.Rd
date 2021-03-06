% Generated by roxygen2 (4.0.2): do not edit by hand
\name{crop_map}
\alias{crop_map}
\title{Crops a map to a new bounding box}
\usage{
crop_map(map, new_bb)
}
\arguments{
\item{map}{An object of class \code{ggmap}}

\item{new_bb}{A data frame with one row and four columns \code{\{ll|ur\}.\{lat|lon\}}}
}
\description{
This function reduces the dimensions of a map to fit a given bounding box
as closely as possible.  The bounding box is readjusted to fit pixel
boundaries.
}

