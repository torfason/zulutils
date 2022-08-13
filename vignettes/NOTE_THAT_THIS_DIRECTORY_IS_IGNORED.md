
# This directory is excluded from builds

This directory contains utility files for vignette building. However, this
package does not currently contain any vignettes, causing a warning on R-check.
Therefore, the `vignettes` directory has been excluded from builds in the
`.Rbuildignore` file.

If a vignette is added, this directory should be removed from the
`.Rbuildignore` file (and this note should be removed as well)
