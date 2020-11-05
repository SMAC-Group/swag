## Resubmission
This is a resubmission. In this version I have:

* Change link in NEWS.md from <http://www.github.com/SMAC-Group/SWAG-R-Package/> to <https://github.com/SMAC-Group/SWAG-R-Package/> (http -> https)
* Add trailing slashes to urls in `URL` and `BugReports` in `DESCRIPTION`
* Create folder and file `inst/REFERENCES.bib` 
* Add `Imports: Rdpack (>= 0.7)` and `RdMacros: Rdpack` in `DESCRIPTION`
* In the documentation of the `swag` function in `swag.R`, I have:
  + modifications of `@description`, `@details` and `@return` for more clarity
  + new `@reference` field
  + new `@importFrom Rdpack reprompt` entry
