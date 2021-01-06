# sfdct 0.1.0

* Fixed for CRAN, a failing test due to new more-robust GEOS (3.9.0), thanks 
 Roger Bivand https://github.com/hypertidy/sfdct/issues/13. 


* testthat changes, `is_false()` to `expect_false()` to avoid warnings. 


This is about the Debian check errors, currently only for
r-devel-linux-x86_64-debian-gcc: presumably from a recent system upgrade
to GEOS 3.9 ...

Check Details
Version: 0.0.6
Check: tests
Result: ERROR
     Running ‘testthat.R’ [8s/13s]
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
     > library(testthat)
     > library(sfdct)
     >
     > test_check("sfdct")
     ══ Warnings ════════════════════════════════════════════════════════════════════
     ── Warning (test-basic-ct.R:46:3): different inputs work ───────────────────────
     `is_false()` is deprecated. Please use `expect_false()` instead.
     Backtrace:
     1. testthat::expect_that(lstri %>% is_empty(), is_false()) test-basic-ct.R:46:2
     2. testthat:::condition(object)
     ── Warning (test-basic-ct.R:80:4): we can triangulate a geometrycollection ─────
     `is_false()` is deprecated. Please use `expect_false()` instead.
     Backtrace:
     1. testthat::expect_that(...) test-basic-ct.R:80:3
     2. testthat:::condition(object)
     ── Warning (test-basic-ct.R:82:4): we can triangulate a geometrycollection ─────
     `is_false()` is deprecated. Please use `expect_false()` instead.
     Backtrace:
     1. testthat::expect_that(...) test-basic-ct.R:82:3
     2. testthat:::condition(object)
    
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Failure (test-prepair-benchmarks.R:41:3): Square with wrong orientation ─────
     sf::st_as_text(st_geometry(fix_wkt(wkt))) == wkt is not FALSE
    
     `actual`: TRUE
     `expected`: FALSE
    
     [ FAIL 1 | WARN 3 | SKIP 0 | PASS 45 ]
     Error: Test failures
     Execution halted
Flavor: r-devel-linux-x86_64-debian-gcc


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



