# sfdct 0.3.0

* Updated plot to remove issue on CRAN vignette. 

* Recreated antarctica dataset. 


# sfdct 0.2.0

* Fix namespace thanks to CRAN. 

* Updated tests. 


# sfdct 0.1.0

* Fixed failing test due to new more-robust GEOS (3.9.0), thanks Roger Bivand https://github.com/hypertidy/sfdct/issues/13. 

# sfdct 0.0.6

* Fixed polygon repair tests due to changes in `st_is_valid` operation in sf. 

# sfdct 0.0.5

* fix tests after st_cast changes in sf

# sfdct 0.0.4

* inserted spaces after the word POLYGON

# sfdct 0.0.3

* first CRAN release 

* fixed lurking bug in all-POINT logic, ... was passed to pslg 

# sfdct 0.0.2

* GEOMETRYCOLLECTION is now supported with an internal function only

* GEOMETRYCOLLECTION now supported, but only as sub-geometries treated like features - a future release will triangulate a simplicial complex of the GC

* introduced use of sp::over so that intersection tests are fast enough, will 
clean this up in future

* now properly returns GEOMETRYCOLLECTION of POLYGON triangles for sfg, sfc, and sf classes. Previous versions always returned either sf or sfc, so this wasn't consistent. 

* more coverage of types, will at least work for the non-exotics in an sf dataframe

* first release



