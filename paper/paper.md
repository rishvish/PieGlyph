---
title: "PieGlyph: An R package for creating axis invariant pie-glyphs in pie-chart scatterplots"
tags:
  - R
  - visualisation
  - pie-charts
  - pie-glyphs
  - glyphs
authors:
  - name: Rishabh Vishwakarma
    orcid: 0000-0002-4847-3494
    corresponding: true 
    affiliation: 1 
  - name: Caroline Brophy
    affiliation: 1
  - name: Catherine Hurley
    affiliation: 2
affiliations:
 - name: School of Computer Science and Statistics, Trinity College Dublin, Ireland
   index: 1
 - name: Department of Mathematics and Statistics, Maynooth University, Maynooth, Ireland
   index: 2
date: 03 May 2024
bibliography: paper.bib
---

# Summary

Effective visualisations can be crucial for understanding and deriving insights from multidimensional data. However, the inability of humans to comprehend beyond three dimensions hinders the rapid recognition of patterns in high dimensions. Over the past few decades, a multitude of approaches have been suggested to visualise high dimensional structural information, employing either dimension reduction or visual encoding techniques [@liu2016visualizing]. Glyph based visualisations are one such technique where multiple data dimensions are encoded onto aesthetic attributes of a graphical symbol [@chen2008multivariate]. A major benefit of glyphs is the ability to offer a compact and direct representation of data observations, thereby facilitating richer pattern recognition.

Pie glyphs are a glyph-based visualisation approach that show the different data dimensions as a pie-chart. Pie-charts are best suited for proportional data as the size of the sector (or slice) within the pie-chart corresponds to relative magnitude of the respective data attributes. Pie-chart scatterplots can be created by replacing points in a traditional scatterplot with pie-chart glyphs (pie-glyphs) to create a hybrid visualisation with the benefits of both techniques. The `PieGlyph` package [@Vishwakarma:2024] developed for `R` software [@RCore:2023] enables users to create pie-glyph scatterplots. \autoref{fig:fig1} shows the additional insights offered by a pie-glyph scatterplot over a traditional scatterplot using data consisting of ratings and sales figures of selected video games [@gamesdata]. While the scatterplot in \autoref{fig:fig1}a can shows the general positive relationship between user and critic scores for various games, the overlayed pie-glyphs in \autoref{fig:fig1}b give additional information about the relative sales for each game in North America, Europe, Japan, and the rest of the world. \autoref{fig:fig1}b shows that North America and Europe account for the majority of sales for almost all games. Two pie-glyphs are labelled to illustrate the effects of user preferences on the distribution of game sales. For example, *FIFA 17* has majority sales in Europe while *NBA 2K17* has majority sales in North America. Generally, games with a lower critic score did not sell across multiple regions (expected as users would refrain from purchasing a poorly reviewed game), while those with higher critic scores had sales in multiple regions. Thus, the inclusion of pie-glyphs in the scatterplot provides additional insights on this multi-dimensional data.

![Scatterplot of critic versus user scores for selected games released in 2016. The data for this plot comes from @gamesdata. (a) and (b) both show the same data, however, points in (b) are overlayed with pie-glyphs showing the proportions of game unit sales across the four regions of North America, Europe, Japan, and the rest of the world. Two observations are labelled to highlight the distribution of sales for specific games. \label{fig:fig1}](Figure%201.png){width="95%" height="8in"}

# Statement of need

`PieGlyph` is developed under the Grammar of Graphics [@wilkinson2012grammar] paradigm using the `ggplot2` [@Wickham:2016] plotting framework. Thus, in addition to showing data attributes along the pie-chart slices, it is also possible to facet the pie-glyph scatterplot (i.e., subset the data and show each subset in a different panel) based on additional data attributes; or to map additional data attributes onto aesthetics such as the size, colour, or style of the pie-glyphs to jointly visualise additional data dimensions and offer extended insights into multi-dimensional data. For example, \autoref{fig:fig1}b could be faceted based on game genre, allowing for the visualization of user and critics scores across different genres whilst also showing regional preferences for each genre.

@cleveland1985graphical demonstrated that visualisations with length based visual encodings (such as bar-charts) are generally superior to angle based encodings (pie-charts) for comparing different attributes in data. However, there are some situations in which pie-charts can be a better visualisation choice. Pie-glyph scatterplots are one such case, where using pie-charts offers the following benefits over bar-charts,

(i) The circular shape of the pie-glyphs centred at the x-y location conveys additional visual information whilst maintaining the simplicity of a scatterplot. 
(ii) A pie-glyph expresses the attributes within the glyph using angles and the position of the glyph using length, maintaining a clear separation between the two entities. A bar-chart glyph expresses both entities using a measure of length which could lead to confusion when viewing the plot.

Other packages in `R` including `scatterpie` [@Yu:2024] and `ggforce` [@Pedersen:2022] offer functionality for creating pie-glyph scatter plots. However, the pie-glyphs created using these functions are linked to the axes of the plot. Thus, the shape of the pie-glyphs changes with the aspect ratio of the plot and stretching the plot in either dimension results in the pie-glyphs getting squished into ellipses (see [vignette](https://rishvish.github.io/PieGlyph/articles/time-series-example.html) for an example). `PieGlyph` solves this problem by creating the pie-glyphs independent of the axes, thus they will always be circles with fixed radii, even as the underlying coordinate system or aspect ratio of the plot changes.

A limitation of static pie-chart glyphs is that only the relative proportion of different attributes can be visualised and not their raw counts (where there are associated raw counts). `PieGlyph` handles this by creating interactive pie-glyphs which, when hovered over, show a tooltip highlighting the raw count and percentage of each attribute shown in the pie-glyph (see [example vignette](https://rishvish.github.io/PieGlyph/articles/interactive-pie-glyphs.html) for more information). This interactivity is incorporated by using the `ggiraph` [@Gohel:2024] package.

`PieGlyph` is designed to be flexible with all features and extensions offered by `ggplot2` and `ggiraph`, thereby allowing users to customise every component of the visualisation. `PieGlyph` can also be used to effectively present spatial and temporal data, or interpret the results of regression and classification models (see [vignettes](https://rishvish.github.io/PieGlyph/articles/) for additional examples). `PieGlyph` shines most when visualising and interpreting statistical models fit to compositional data (as there is a natural sum to one constraint on the data variables) and visualisations created using `PieGlyph` have already been used in multiple publications [for example, @Moral:2023; @Finn:2024]. 

# Acknowledgements

All authors were supported by the Science Foundation Ireland Frontiers for the Future programme, grant number 19/FFP/6888 award to CB.

# References
