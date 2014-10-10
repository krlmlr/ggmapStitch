\name{NEWS}
\title{News for Package 'ggmapStitch'}
\encoding{UTF-8}
\section{Changes in ggmapStitch version 0.0-0.4 (2014-10-10)}{
  \itemize{
    \item Use Mercator projection to compute location of bounding box on raster
  }
}
\section{Changes in ggmapStitch version 0.0-0.3 (2014-10-10)}{
  \itemize{
    \item Revert "fix cropping" -- the orientation of a raster is indeed top->bottom (matrix)
  }
}
\section{Changes in ggmapStitch version 0.0-0.2 (2014-10-10)}{
  \itemize{
    \item Fix cropping: the orientation of a raster is bottom->top (cartesian) and not top->bottom (matrix)
  }
}
\section{Changes in ggmapStitch version 0.0-0.1 (2014-10-10)}{
  \itemize{
    \item Function \code{crop_map} crops a \code{ggmap} object to a specified bounding box
  }
}
