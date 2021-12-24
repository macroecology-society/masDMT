<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

### Towards interoperable workflows
<p align="justify">
<a href="https://www.idiv.de/en/groups-and-people/core-groups/macroecosocial.html">The Macroecology & Society (MAS) is a research group of the German Centre for Integrative Biodiversity Research (iDiv)</a>, composed by an interdisciplinary team of scientists. Its members have backgrounds in geography, statistics, macro- and community ecology, environmental resource management, environmental social science, crop science, and agricultural biology. In this rich environment, the lab aims to detect and understand global dynamics in human-environment systems, with a focus on human land use, its underlying societal drivers, and its ecological consequences. 
</p>
<p align="justify">
To answer our questions, We rely rely on massive amounts of data, from statistical surveys to global, gridded and vector layers. These data are frequently shared across projects, creating a need for a common knowledge infrastructure. With this in mind, we developed `masDMT`. This R package provides the members of the MAS lab with standardized access points their common database, as well as tools to make research projects scalable. It helps transfer a project between small, local analysis in a desktop, to global analysis in High Performance Computing (HPC) environment.
</p>

## Installation
<p align="justify">
`masDMT` can be installed with `devtools` using the following code snipet:
</p>

```r
devtools::install_github('macroecology-society/masDMT')
```

### What not to expect from masDMT
<p aligh='justify'>
`masDMT` provides <a href="">tools to find data in our database</a>, but not to dedicated ways to open it. This is motivated by the vast amount of options to deal with spatial data and their extensive documentation. For those less experienced with spatial data, <a href="https://geocompr.robinlovelace.net/">"Geocomputation with R"</a> is a good place to start. It provides vast information on the use of spatial data in R, including discussions on issues of scalability. Another important resource is <a href="https://www.earthdatascience.org/">"Earth Labs's data science"</a>, which documents generic data science applications in R (e.g. automated reporting), and goes deeper into remote sensing data and spatial analysis through concrete practical examples.
</p>
<p aligh='justify'>
Among them, we highlight 
</p>

### Acknowledgements
<p align="justify">
This work was funded by the Volkswagen Foundation through a Freigeist Fellowship to <a href="https://orcid.org/0000-0003-3927-5856">Carsten Meyer <i class="fab fa-orcid"></i></a>, with additional support by iDiv (FZT-118, DFG). `masDMT` and the data infrastructure it serves were developed with the High-Performance Computing (HPC) Cluster EVE, a joint effort by the Helmholtz Centre for Environmental Research (UFZ) and the German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig.
</p>

<p align="center">
<a href="https://www.idiv.de/en/groups-and-people/core-groups/macroecosocial.html"><img src="https://www.idiv.de/fileadmin/templates/images/socialimage.png" width="400" height="200"/></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.volkswagenstiftung.de/en/funding/our-funding-portfolio-at-a-glance/freigeist-fellowships"><img src="https://www.volkswagenstiftung.de/sites/default/files/images/2018Freigeist_Logo_web_blau.jpg" width="400" height="200"/></a>
</p>
