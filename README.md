# Welcome to the **Data Analysis Tool**!

The **Cognitive-Affective Maps _Extended Logic_** software package provides an open-source solution to analyze Cognitive-Affective Map (CAM) data.

## Getting Started

For recommendations and workflow guidance on using the Data Analysis Tool, please refer to the [online documentation](https://camtools-documentation.readthedocs.io/en/master/Data%20Analysis%20Tool/).

## What's Included in the App

The Data Analysis Tool supports a full CAM workflow from raw-data import to reporting:

- **Import and setup:** Upload CAM data and protocol files, run descriptive views, and clean Valence-format data when needed.
- **Visualization:** Draw and inspect CAMs (R-based graph rendering), including options to review and organize maps.
- **Pre-processing modules:** Semi-automatic concept summarization with approximate matching, search terms (regex), synonym support, and word2vec-assisted grouping.
- **Reliability workflow:** Create rater training word lists and compute inter-rater reliability statistics (including Cohen's Kappa variants).
- **Analysis modules:** Compute network and neighborhood indicators, explore concept-level co-occurrence/valence patterns, and slice CAMs into substructures.
- **Outputs and reporting:** Generate word lists, word clouds, concept-level summaries, aggregate CAMs, and an APA-style report.

For complete module details, examples, and technical notes, see the [full documentation](https://camtools-documentation.readthedocs.io/en/master/Data%20Analysis%20Tool/).

## Run Locally or Online

You can use the Data Analysis Tool in two ways:

- **Local (recommended for speed):**
  - Clone or download this repository.
  - Install [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/) if needed.
  - Open `app.R`, install missing packages, and click **Run App**.
- **Online (slower):**
  - Open: [https://fennapps.shinyapps.io/DataAnalysis/](https://fennapps.shinyapps.io/DataAnalysis/)

## Need Help?

We’re happy to assist with any additional questions or ideas you may have. We’d love to hear from you! Feel free to reach out:

- 📧 **Email us:** [cam.contact@drawyourminds.de](mailto:cam.contact@drawyourminds.de)
- 💬 **Join our community channel:** [Support Page](https://camtools-documentation.readthedocs.io/en/master/Support/)

## Acknowledgments

This software has been developed by:

- **Julius Fenn**
- **Florian Gouret**
- **Michael Gorki**
- **Paul Sölder**
- **Andrea Kiesel**

# Cite Our Software

If you use these materials, please cite the article (see [CITATION.cff](CITATION.cff) for machine-readable metadata):

> Fenn, J., Gouret, F., Gorki, M., Reuter, L., Gros, W., Hüttner, P., & Kiesel, A. (2025). Cognitive-affective maps extended logic: Proposing tools to collect and analyze attitudes and belief systems. _Behavior Research Methods, 57_(6), 174. https://doi.org/10.3758/s13428-025-02699-y

BibTeX:

```bibtex
@article{fenn2025camel,
  author  = {Fenn, Julius and Gouret, Florian and Gorki, Michael and Reuter, Lisa and Gros, Wilhelm and H{\"u}ttner, Paul and Kiesel, Andrea},
  title   = {Cognitive-affective maps extended logic: Proposing tools to collect and analyze attitudes and belief systems},
  journal = {Behavior Research Methods},
  year    = {2025},
  volume  = {57},
  number  = {6},
  pages   = {174},
  doi     = {10.3758/s13428-025-02699-y},
  url     = {https://doi.org/10.3758/s13428-025-02699-y}
}
```