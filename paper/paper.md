---
title: "PieGlyph: An R package for creating axis invariant pie-glyphs in pie-chart scatterplots"
tags:
  - R
  - visualisation
  - pie-charts
  - glyphs
authors:
  - name: Rishabh Vishwakarma
    orcid: 0000-0002-4847-3494
    corresponding: true 
    equal-contrib: true
    affiliation: 1 
  - name: Caroline Brophy
    equal-contrib: true 
    affiliation: 1
  - name: Catherine Hurley
    equal-contrib: true
    affiliation: 2
affiliations:
 - name: School of Computer Science and Statistics, Trinity College Dublin, Ireland
   index: 1
 - name: Department of Mathematics and Statistics, Maynooth University, Maynooth, Ireland
   index: 2
date: 24 April 2024
bibliography: paper.bib
---

# Summary

Effective visualisations are one of the best tools for understanding and deriving insights from multidimensional data. However, the inability of human cognition to comprehend beyond three dimensions, poses hindrance in the rapid recognition of patterns in high dimensions. Over the past few decades, a multitude of approaches have been suggested to visualise high dimensional structural information, employing either dimension reduction or visual encoding techniques [@liu2016visualizing]. Glyph based visualisations are one such technique where multiple data dimensions are encoded onto attributes of graphical entity [@chen2008multivariate]. A major benefit of glyphs is the ability to offer a compact and direct representation of data observations, thereby facilitating richer pattern recognition. 

Pie glyphs are a glyph-based visualisation approach that show the different data dimensions as a pie-chart. Pie-charts are best suited for proportional data as the size of the sector (or slice) within the pie-chart corresponds to relative magnitude of the respective data attributes. Pie-chart scatterplots can be created by replacing points in a traditional scatterplot with pie-chart glyphs to create a hybrid visualisation with the benefits of both techniques. The `PieGlyph` `R` [@RCore:2023] has been developed to enable users create these pie-chart scatterplots. \autoref{fig:fig1} shows the additional insights offered by a pie-chart scatterplot over a traditional scatterplot using data consisting of ratings and sales figures of selected video games [@gamesdata]. While the scatterplot in Figure 1a can show the general positive relationship between user and critic scores for various games, the overlayed pie-glyphs in Figure 1b give additional information about the relative regional sales (in million units) for each game in North America, Europe, Japan, and Rest of the World. A quick glance at \autoref{fig:fig1}b shows that North America and Europe account for the majority of sales for almost all games. It can also be seen that generally, games with a lower critic score don't sell across multiple regions (expected as users would refrain from purchasing a poorly reviewed game), while those with higher critic scores have sales in multiple regions.

![Scatterplot of user and critic scores for a selected few games released in 2016. The data for this plot comes from @gamesdata. (a) and (b) both show the same data, however, points in (b) are overlayed with pie-chart glyphs showing the proportions of game unit sales across the four regions of North America, Europe, Japan, and Rest of the World. Certain games of interest (games with highest sales, worst reviews, best reviews, etc.) are labelled for better identification. \label{fig:fig1}](Figure 1.png){ width=80% }

# Statement of need

`PieGlyph` is developed under a Grammar of Graphics [@wilkinson2012grammar] paradigm using the `ggplot2` [@Wickham:2016] plotting framework. Thus, in addition to showing data attributes along the pie-chart slices, it is also possible to facet the pie-chart scatterplot (i.e., subset the data and show each subset in a different panel) based on additional data attributes or map additional data attributes onto aesthetics like the size, colour, or style of the pie-charts to jointly visualise even more data dimensions and offer extended insights into multidimensional data. For example, \autoref{fig:fig1}b could be facetted based on game genre, allowing for the visualization of user and critics scores across different genres whilst also showing regional preferences for each genre.

@cleveland1985graphical demonstrated that visualisations with length based visual encodings (such as bar-charts) are generally superior to angle based encodings (pie-charts) for comparing different attributes in data. However, there are some situations in which pie-charts can be a better visualisation choice. Pie-chart scatterplots are one such case, where using pie-charts offers the following three benefits over bar-charts, 

  (i) The circular shape of the pie-chart glyphs centred at the x-y location conveys additional visual information whilst maintaining the simplicity of a scatterplot. Using a bar-chart as a glyph in a scatterplot could clutter the plot at times and render it more strenuous to look at (compared with a plot using pie-glyphs). 
  (ii) The total area occupied by a pie-chart glyph is constant irrespective of the number of attributes shown within it. A bar-chart glyph on the other hand could take up more space on the plot if there are too many attributes (\autoref{fig:fig2}b), further aggravating the problem of overplotting in a scatterplot. 
  (iii) The length of the different bars in a bar-chart glyph could be conflated with the variable shown on the X or Y axes as both, the attributes within the glyph as well as the position of the glyph are visualised by a measure of length. A pie-chart glyph on the other hand measures the attributes within the glyph using angles, and the position of the glyph using length, thereby maintaining a clear separation between the two entities.
  
\autoref{fig:fig2} shows a visual representation of these issues. \autoref{fig:fig2}a uses stacked bar-charts while \autoref{fig:fig2}b uses dodged bar-charts as glyphs respectively. Issues (i) and (iii) are evident across both panels. Stacked bar-charts don't suffer from issue (ii) and would always occupy a constant area but it could sometimes be difficult to compare the relative values of categories (see the glyph for "Call of Duty: Infinity Warfare"). Dodged bar-charts do not suffer from this problem and it's easier to compare across the categories in a glyph, but glyph occupies a larger area on the plot as the number of attributes increase.

![A scatterplot of user and critic scores for select games from 2016, but the points are replaced with bar-chart glyphs showing the relative sales (in million units) for each game across the four regions of North America, Europe, Japan, and Rest of the World. (a) uses stacked bar-charts while (b) uses dodged bar-charts as glyphs. Certain games of interest (games with highest sales, worst reviews, best reviews, etc.) are labelled for better identification. \label{fig:fig2}](Figure 2.png){ width=80% }

<br>
Other packages in `R` like `scatterpie` [@Yu:2024] and `ggforce` [@Pedersen:2022] offer functionality for creating pie-chart scatter plots. However, the pie-glyphs created using these functions are linked to the axes of the plot. Thus, the shape of the pie-glyphs changes with the aspect ratio of the plot and stretching the plot in either dimension results in the pie-glyphs getting squished into ellipses (see [vignette](https://rishvish.github.io/PieGlyph/articles/time-series-example.html) for an example). `PieGlyph` solves this problem by creating the pie-glyphs independent of the axes, thus they will always be circles with fixed radii, even as the underlying coordinate system or aspect ratio of the plot changes.

A shortcoming of static pie-chart glyphs is only the relative proportion of different attributes can be visualised and not their raw counts. `PieGlyph` handles this by creating interactive pie-chart glyphs which, when hovered over, show a tooltip highlighting the raw count and percentage of each attribute shown in the pie-chart (\autoref{fig:fig3}). This interactivity is incorporated by using the `ggiraph` [@Gohel:2024] package under the hood. 

![A interactive pie-chart scatterplot showing user and critic scores for a select few games released in 2016 with pie-chart glyphs showing the proportional sales (in million units) for each game across the four regions of North America, Europe, Japan, and Rest of the World. Hovering over a pie-glyph would show a tooltip containing information about the raw counts (and percentages) of sales across each region (The game "FIFA 17" has been highlighted). Certain games of interest (games with highest sales, worst reviews, best reviews, etc.) are labelled for better identification. \label{fig:fig3}](Figure 3.png){ width=85% }

<br>
`PieGlyph` is designed to be flexible with all features and extensions offered by `ggplot2` and `ggiraph`. Thus, all visualisations can be finely tailored to meet user specifications. It has already been used in multiple publications [@Moral:2023; @Finn:2024] to create informative visualisations showing multiple data dimensions on a static plot enabling researchers to identify patterns which weren't possible to capture using traditional visualisation techniques. 


# Acknowledgements

All authors were supported by the Science Foundation Ireland Frontiers for the Future programme, grant number 19/FFP/6888 award to CB.

# References
