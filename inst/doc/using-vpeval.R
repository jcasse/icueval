## ------------------------------------------------------------------------
library(vpeval)
vpeval_encinfo
vpeval_demog
basic_metrics(enc_info = vpeval_encinfo, demographics = vpeval_demog)

## ------------------------------------------------------------------------
head(mtcars)
mi_cluster(mtcars, x_col = "cyl", y_col = "am")

## ------------------------------------------------------------------------
library(vpeval)
cats = load_drug_categories()
head(cats)

## ------------------------------------------------------------------------
library(vpmunge)
vpmunge_drugs[, c(1,2,5)]
summarize_drug_categories(vpmunge_drugs, categories = cats)

