<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## Towards interoperable workflows
<p align="justify">
At the Macroecology and Society (MAS), a research group of the German Centre for Integrative Biodiversity Research (iDiv). we compose an interdisciplinary lab, combining backgrounds in geography, statistics, macro- and community ecology, environmental resource management, environmental social science, crop science, and agricultural biology. Across our multiple research fields, we are interested in detecting and understanding global dynamics in human-environment systems, with a focus on human land use, its underlying societal drivers, and its ecological consequences. 
</p>
<p align="justify">
To answer our questions, We rely rely on massive amounts of data with different characteristics and thematic scopes, from statistical surveys to global gridded and vector layers. We often share this data across projects as well as the code to analyze it, making the need for a common infrastructure imperative. With this in mind, we developed `masDMT`. This R package provides standardized access points to the common database of MAS. Additionally, it provides tools to integrate these data within an High Performance Computing (HPC) environment.
</p>

## Instalation
<p align="justify">
`masDMT` takes advantage of the MAS group database with EVE, a high-performance computing platform hosted at UFZ and supported by iDiv. Due to this dependency, the package was not submitted to CRAN and must be installed from it's Github repository. To do, we can use `devtools` as following:
</p>

```r
devtools::install_github('macroecology-society/masDMT')
```

## Requirements
The package requires R, a running GDAL instalation and SQLite.

## Acknowledgements
<p align="justify">
This work was funded by the Volkswagen Foundation through a Freigeist Fellowship to <a href="https://orcid.org/0000-0003-3927-5856">Carsten Meyer <i class="fab fa-orcid"></i></a>, with additional support by iDiv (FZT-118, DFG). `masDMT` and the data infrastructure it supports were developed within the High-Performance Computing (HPC) Cluster EVE, a joint effort by the Helmholtz Centre for Environmental Research (UFZ) and the German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig.
</p>

<p align="center">
<a href="https://www.idiv.de/en/groups_and_people/core_groups/macroecosocial.html"><img src="idiv_promo.png" width="400" height="200"/></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.volkswagenstiftung.de/en/funding/our-funding-portfolio-at-a-glance/freigeist-fellowships"><img src="wg_3.jpg" width="400" height="200"/></a>
</p>
