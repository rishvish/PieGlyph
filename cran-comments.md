# Version 1.0.0

## This is an update (v1.0.0) to the PieGlyph package

### Submission 2

* Fixed the note about revdep directory being present in root
* Fixed note about missing anchor for external links in .rd files


## R CMD check results

There were no ERRORS or WARNINGS when building the package on my local machine. I encountered the following note.

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## Test environments

I checked that the package builds successfully on the following systems without any errors or warnings.

- local Windows install, R 4.3.2
- win-builder (devel and release)
- macOS builder
- linux (r-devel)
- ubuntu 
