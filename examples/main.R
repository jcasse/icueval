library(icueval)

################################################################################
### load data
################################################################################

pathname = system.file("extdata", "DS 2.1.1 Curated.rds", package = "icueval")
system.time(data <- readRDS(pathname))

################################################################################
### code used to generate the "curated" data
################################################################################

################################################################################
### mechanical ventilation rate
################################################################################

mv <- on_intervention(data$enc_info, data$interventions,
                      "Mechanical Ventilation (encounter info)")

################################################################################
### length of mechanical ventilation
################################################################################

lomv <- length_of_intervention(data$enc_info, data$interventions,
                               "Intubation (encounter info)",
                               "days")

################################################################################
### days off mechanical ventilation
################################################################################

domv <- time_off_intervention(data$enc_info, data$interventions,
                              "Mechanical Ventilation (encounter info)",
                              "days")

################################################################################
### worst oxigen saturation index (OSI)
################################################################################

interventions <- data$interventions
interventions$Value <- as.numeric(interventions$Value)
interventions <- interventions[complete.cases(interventions[, "Value"]), ]
system.time(wosi <- worst_osi(data$enc_info, interventions, data$vitals))

################################################################################
### Worst Inotrope Score (IS)
################################################################################

wis <- worst_is(data$enc_info, data$drugs)

################################################################################
### Median Worst INR
################################################################################

winr <- worst_inr(data$enc_info, data$labs)

################################################################################
### Median Worst GCS
################################################################################

wgcs <- worst_gcs(data$enc_info, data$vitals)

################################################################################
### compute basic metrics
################################################################################

enc_info <- data$enc_info
enc_info$patient_id <- substr(enc_info$patient_id_encounter, 0, 7)
enc_info$patient_id <- as.integer(enc_info$patient_id)
basic_metrics <- icueval::basic_metrics(enc_info, data$demographics)
basic_metrics$mortality[basic_metrics$mortality == 0] <- "lived"
basic_metrics$mortality[basic_metrics$mortality == 1] <- "died"
basic_metrics$mortality <- as.factor(basic_metrics$mortality)

################################################################################
### select episodes that remained in the icu after 12 hours (0.5 days)
################################################################################

# 774 episodes did not last more than 12 hours
encounters <- subset(basic_metrics,
                     length_of_stay >= 0.5,
                     select = c("patient_id_encounter"))

################################################################################
### metrics
################################################################################

metrics_first_12_hr <- Reduce(function(x, y) merge(x, y), list(basic_metrics,
                                                              wisf12,
                                                              winrf12,
                                                              wgcsf12))
saveRDS(metrics_first_12_hr, file = "inst/extdata/metrics_first_12_hr.rds")

metrics_after_12_hr <- Reduce(function(x, y) merge(x, y), list(basic_metrics,
                                                               wisa12,
                                                               winra12,
                                                               wgcsa12))
saveRDS(metrics_after_12_hr, file = "inst/extdata/metrics_after_12_hr.rds")

metrics_last_12_hr <- Reduce(function(x, y) merge(x, y), list(basic_metrics,
                                                              wisl12,
                                                              winrl12,
                                                              wgcsl12))
saveRDS(metrics_last_12_hr, file = "inst/extdata/metrics_last_12_hr.rds")

################################################################################
### load clustering
################################################################################

pathname = system.file("extdata", "clustering.rds", package = "icueval")
system.time(clustering <- readRDS(pathname))

################################################################################
### code used to create clustering.rds
################################################################################

clustering_pathname <- paste("/Users/jcasse/Experiments/T-2016-01-22",
                             "pre-process/pcim-features-3/feature-selection",
                             "co-cluster-2/encounter_id_clustering.csv",
                             sep = "/")
clustering <- read.csv(file = clustering_pathname, stringsAsFactors = FALSE)
clustering$cluster_id <- clustering$cluster_id + 1
names(clustering)[names(clustering) == "encounter_id"] <- "patient_id_encounter"
clustering$cluster_id <- as.factor(clustering$cluster_id)
saveRDS(clustering, file = "inst/extdata/clustering.rds")

################################################################################
### add cluster_id to metrics (may have fewer encounters than metrics)
################################################################################

df <- merge(metrics, cluster_labels)

################################################################################
### sort by cluster size
################################################################################

`%>%` <- magrittr::`%>%`

# compute cluster sizes
sizes <- df %>% group_by(cluster_id) %>% summarise(size = n())
# sort by cluster size
sizes <- sizes[order(-sizes$size), ]
# set factor levels for cluster_id, so that clusters are plot in order
df$cluster_id <- factor(df$cluster_id, levels = sizes$cluster_id)

################################################################################
### Output
################################################################################

file <- paste(output_dir, "metrics.csv", sep = "/")
write.csv(x = df, file = file, row.names = FALSE)
