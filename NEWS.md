v0.0-0.4 (2014-10-10)
===

* Use Mercator projection to compute location of bounding box on raster

v0.0-0.3 (2014-10-10)
===

* Revert "fix cropping" -- the orientation of a raster is indeed top->bottom (matrix)

v0.0-0.2 (2014-10-10)
===

* Fix cropping: the orientation of a raster is bottom->top (cartesian) and not top->bottom (matrix)

v0.0-0.1 (2014-10-10)
===

* Function `crop_map` crops a `ggmap` object to a specified bounding box
