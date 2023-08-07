Welcome to the folder "additional scripts" of the Cognitive-Affective Maps tools *CAM-App*!
======================================================

## What you can do here

If you want to implement your own functionalities you can start running the scripts included in this folder. Please always start with the R script "aa_start_load_draw". Scripts starting with the nae "pp" belongs to the preprocessing part of the CAM app and scripts starting with "ap" to the analysis part. 


## Included files
*For more details see description within the single files!*


* "aa_start_load_draw": start always here, at top include your filename (add your data set to the "data" folder beforehand)

**pre processing part functions**
* "pp - get reliability": compute reliability coefficients
* "pp - Search for Synonyms": find synonyms within your data set

**analysis part functions**
* "ap - aggregate CAMs": aggregate CAMs
* "ap - network indicators": compute network indicators

**additional functions**
* "check non-sense CAMs": using dictionaries automatically identify CAMs where participant have not drawn one known word
* "save CAMs as JSON, R format": save CAMs as .json, .png format


