# APCalign 1.0.1

First major release of APCalign.  A preprint is available at 
https://www.biorxiv.org/content/10.1101/2024.02.02.578715v1. 
Article has been accepted for publication at Australian journal of Botany.

Following review, a number of changes have been implemented. These have sped & 
streamlined the package.

* Update function documentation
* Speed up `extract_genus`
* Write a replacement function for `stringr::word` that is much faster.
* Additional speed up and accuracy of fuzzy_match function by
  - Restricting reference list to names with the same first letter as input string.
  - Switch from using `utils::adist` to `stringdist::stringdist(method = "dl")`
* Rework `standardise_names` to remove punctuation from the start of the string
* Rework `strip_names_extra` (previously `strip_names_2`) to just perform 
additional functions to `strip_names`, rather than repeating those performed by `strip_names`.
* Avoid importing entire packages by using package::function format throughout 
and removing functions from @import
* Add fuzzy match arguments to `create_taxonomic_update_lookup`
* Add 3 additional family-level APC matches to `match_taxa`.
* Refine tests
* Make messages to console optional
* Fix issue with fails when github is down (https://github.com/traitecoevo/APCalign/issues/205)
* Update installation instructions 
* Added how to cite and version APCalign as an article
* Exported `default_version`
* Add citing method for R package
* Update GitHub Actions
* Improved family alignments 
* Added `standardise_taxon_rank`
* Improved messaging during alignment
