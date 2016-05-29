#' @importFrom magrittr %>%
NULL

################################################################################
#' Intervention performed indicator.
#'
#' \code{on_intervention} returns an indication of whether a particular
#' intervention was performed during an encounter, for every encounter.
#'
#' Determines whether a particular intervention was performed at any point in
#' time, per encounter. A column in the resulting data frame indicates whether
#' the intervention was performed (with a 1) or not (with a 0).
#'
#' @param enc_info_df \code{data frame}: provides the encounters of interest
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    }
#'
#' @param interventions_df \code{data frame}: provides intervention events
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{chr}: Variable.Name
#'    }
#'
#' @param intervention_name \code{chr}: names the intervention
#' of interest
#'
#' @return data_frame:
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{num}: intervention_name
#'    }
#'
#' @export
################################################################################
on_intervention <-
  function(
    enc_info_df,
    interventions_df,
    intervention_name
  ) {

    # check user input
    expectations = list(
      a = setNames(list("character"),
                   c("patient_id_encounter")),
      b = setNames(list("character", "character"),
                   c("patient_id_encounter", "Variable.Name")),
      c = "character"
    )

    ret <- unique(
      subset(interventions_df, interventions_df["Variable.Name"] ==
               intervention_name,
             select = c("patient_id_encounter")))
    ret[intervention_name] <- 1
    ret <- merge(enc_info_df["patient_id_encounter"], ret, all.x = TRUE)
    ret[intervention_name][is.na(ret[intervention_name])] <- 0
    return(ret)
  }
################################################################################

################################################################################
#' Total length of a specific intervention.
#'
#' \code{length_of_intervention} returns the total length of the specified
#' intervention.
#'
#' Computes the total length of intervention, per encounter. Encounters with
#' no events of the specified interventions are assumed to not have had the
#' intervention, and a value of zero is given. The function currently works
#' only for interventions that have both a start time and end time.
#'
#' @param enc_info_df \code{data frame}: provides the encounters of interest
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    }
#'
#' @param interventions_df \code{data frame}: provides intervention events
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{chr}: Variable.Name
#'    \item \code{POSIXct}: Start.Time
#'    \item \code{POSIXct}: End.Time
#'    }
#'
#' @param intervention_name \code{chr}: names the intervention of interest
#'
#' @param time_units \code{chr}: time units to compute time difference in
#'
#' @return data_frame:
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{num}: length of intervention_name
#'    }
#'
#' @export
################################################################################
length_of_intervention <-
  function(
    enc_info_df,
    interventions_df,
    intervention_name,
    time_units
  ) {

    # check user input
    expectations = list(
      a = setNames(list("character"),
                   c("patient_id_encounter")),
      b = setNames(list("character", "character", "POSIXct", "POSIXct"),
                   c("patient_id_encounter", "Variable.Name",
                     "Start.Time", "End.Time")),
      c = "character",
      d = "character"
    )

    ret <- subset(interventions_df,
                  Variable.Name == intervention_name)
    ret <- subset(ret,
                  select = c("patient_id_encounter", "Start.Time", "End.Time"))
    end_times <- unique(ret$End.Time)
    if (length(end_times) == 1 & is.na(end_times[1])) {
      stop(paste(
        paste("Intervention", intervention_name, sep = " "),
        "has no End.Times", sep = " "))
    }
    colname <- paste("length of",
                     paste(intervention_name, time_units, sep = " "), sep = " ")
    ret[colname] <- difftime(ret$End.Time, ret$Start.Time, units = time_units)
    ret <- ret[c("patient_id_encounter", colname)]
    ret <- aggregate(ret[colname],
                     by=ret[("patient_id_encounter")], FUN = sum)
    ret <- merge(enc_info_df["patient_id_encounter"], ret, all.x = TRUE)
    ret[colname][is.na(ret[colname])] <- 0
    ret[colname] <- as.numeric(ret[[colname]])
    return (ret)
  }
################################################################################

################################################################################
#' Time off a specific intervention, within the first 28 days.
#'
#' \code{time_off_intervention} returns the time an encounter has been off the
#' specified intervention within the first 28 days.
#'
#' Computes time time off since the last event of the specified intervention,
#' in the first 28 days, per encounter. Missing values are imputed with NAs,
#' as encounters with no events of the specified interventions are assumed
#' to not have had that intervention. The function currently works
#' only for interventions that have both a start time and end time.
#'
#' @param enc_info_df \code{data frame}: provides the encounters of interest
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{POSIXct}: Episode.Start.Timestamp
#'    }
#'
#' @param interventions_df \code{data frame}: provides intervention events
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{chr}: Variable.Name
#'    \item \code{POSIXct}: Start.Time
#'    \item \code{POSIXct}: End.Time
#'    }
#'
#' @param intervention_name \code{chr}: names the intervention of interest
#'
#' @param time_units \code{chr}: time units to compute time difference in
#'
#' @return data_frame:
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{num}: time off intervention_name
#'    }
#'
#' @export
################################################################################
time_off_intervention <-
  function(
    enc_info_df,
    interventions_df,
    intervention_name,
    time_units
  ) {

    # check user input
    expectations = list(
      a = setNames(list("character", "POSIXct"),
                   c("patient_id_encounter", "Episode.Start.Timestamp")),
      b = setNames(list("character", "character", "POSIXct", "POSIXct"),
                   c("patient_id_encounter", "Variable.Name",
                     "Start.Time", "End.Time")),
      c = "character",
      d = "character"
    )

    start_times <-
      subset(enc_info_df,
             select = c("patient_id_encounter", "Episode.Start.Timestamp"))
    ret <-
      subset(interventions_df,
             Variable.Name == intervention_name)
    ret <-
      subset(ret,
             select = c("patient_id_encounter", "Start.Time", "End.Time"))
    end_times <- unique(ret$End.Time)
    if (length(end_times) == 1 & is.na(end_times[1])) {
      stop(paste(
        paste("Intervention", intervention_name, sep = " "),
        "has no End.Times", sep = " "))
    }

    ret <- merge(start_times, ret)

    # look only at the period within the first 28 days
    ret <- ret[difftime(ret$Start.Time,
                        ret$Episode.Start.Timestamp,
                        units = time_units) <= 28, ]

    # keep only the event with the latest end time, if more than one
    ret <- ret[order(ret$patient_id_encounter, ret$End.Time,
                     decreasing = TRUE), ]
    ret <- ret[!duplicated(ret[c("patient_id_encounter")]), ]

    # compute when (days within episode start) the vent ended
    ret$end_day <- difftime(ret$End.Time, ret$Episode.Start.Timestamp,
                             units = time_units)

    # compute days off intervention
    colname <- paste("time off",
                     paste(intervention_name, time_units, sep = " "), sep = " ")
    ret[colname] <- 28 - ret$end_day
    ret[colname][ret[colname] < 0] <- 0 # if lasted more than 28 days

    ret <- merge(enc_info_df["patient_id_encounter"], ret, all.x = TRUE)
    ret <- ret[c("patient_id_encounter", colname)]
    ret[colname] <- as.numeric(ret[[colname]])

    return (ret)
  }
################################################################################

################################################################################
#' Worst Oxygen Saturation Index
#'
#' \code{worst_osi} returns the worst (highest) oxygen saturation
#' index, for each encounter selected.
#'
#' Missing values are imputed with zero: Encounters that do not have all
#' measurements (i.e, mean airway pressure, fio2 or pulseox) get OSI = 0,
#' as per Nelson.
#' NOTE: This function may take up to 5 minutes on the entire DS 2.1.1.
#'
#' @param enc_info_df \code{data frame}: provides the encounters of interest
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{POSIXct}: Episode.Start.Timestamp
#'    }
#'
#' @param interventions \code{data frame}: provides interventions events
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{chr}: Variable.Name
#'    \item \code{num}: Value
#'    \item \code{POSIXct}: Start.Time
#'    }
#'
#' @param vitals \code{data frame}: provides vitals events
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{chr}: Variable.Name
#'    \item \code{num}: Value
#'    \item \code{POSIXct}: End.Time
#'    }
#'
#' @return data_frame:
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{num}: worst_osi
#'    }
#'
#' @export
################################################################################
worst_osi <-
  function(
    enc_info,
    interventions,
    vitals
  ) {

    # check user input
    expectations = list(
      a = setNames(list("character", "POSIXct"),
                   c("patient_id_encounter", "Episode.Start.Timestamp")),
      b = setNames(list("character", "character", "numeric", "POSIXct"),
                   c("patient_id_encounter", "Variable.Name", "Value",
                     "Start.Time")),
      c = setNames(list("character", "character", "numeric", "POSIXct"),
                   c("patient_id_encounter", "Variable.Name", "Value",
                     "End.Time"))
    )

    ret <- oxygen_saturation_index(interventions, vitals)

    # keep only the worse (highest) OSI for each patient
    ret <- ret[order(ret$patient_id_encounter, -ret$oxygen_saturation_index), ]
    ret <- ret[!duplicated(ret[c("patient_id_encounter")]), ]

    ret <- merge(enc_info["patient_id_encounter"], ret, all.x = TRUE)
    ret$time <- NULL # if not removed, the next instruction will throw an error
    ret[is.na(ret)] <- 0

    names(ret)[names(ret) == "oxygen_saturation_index"] <- "worst_osi"

    return(ret)
  }
################################################################################

################################################################################
#' Instantaneous Oxygen Saturation Index (OSI).
#'
#' \code{oxygen_saturation_index} returns the instantaneous oxygen saturation
#' index, for each encounter.
#'
#' As per Nelson Sanchez-Pinto,
#' OSI = 100 * Mean Airway Pressure * FiO2 / Pulse Oximetry,
#' where Meand Ariway Pressure and FiO2 are taken to be the most recent within
#' the last hour preceding Pulse Oximetry. Therefore, each returned OSI
#' will have the time correpsonding to the Pulse Oximetry measurement.
#'
#' @param interventions \code{data frame}: provides interventions events
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{chr}: Variable.Name
#'    \item \code{num}: Value
#'    \item \code{POSIXct}: Start.Time
#'    }
#'
#' @param vitals \code{data frame}: provides vitals events
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{chr}: Variable.Name
#'    \item \code{num}: Value
#'    \item \code{POSIXct}: End.Time
#'    }
#'
#' @return data_frame:
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{num}: oxygen_saturation_index
#'    \item \code{POSIXct}: time
#'    }
#'
#' @export
################################################################################
oxygen_saturation_index <-
  function(
    interventions,
    vitals
  ) {

    # check user input
    expectations = list(
      a = setNames(list("character", "character", "numeric", "POSIXct"),
                   c("patient_id_encounter", "Variable.Name", "Value",
                     "Start.Time")),
      b = setNames(list("character", "character", "numeric", "POSIXct"),
                   c("patient_id_encounter", "Variable.Name", "Value",
                     "End.Time"))
    )

    cols <- c("patient_id_encounter", "Variable.Name", "Value", "time")

    # get intervention events
    vars <- c("Mean airway pressure (cmH2O)", "FiO2")
    #filter_criteria = lazyeval::interp(~ Variable.Name %in% vars)
    #intrvntns = interventions %>% dplyr::filter_(filter_criteria)
    intrvntns <- interventions[interventions$Variable.Name %in% vars, ]
    names(intrvntns)[names(intrvntns) == "Start.Time"] <- "time"
    intrvntns <- subset(intrvntns, select = cols)

    # get vital sign events
    vars <- c("Pulse oximetry (%)")
    #filter_criteria = lazyeval::interp(~ Variable.Name %in% vars)
    #vitls = vitals %>% dplyr::filter_(filter_criteria)
    vitls <- vitals[vitals$Variable.Name %in% vars, ]
    names(vitls)[names(vitls) == "End.Time"] <- "time"
    vitls <- subset(vitls, select = cols)

    # combine
    df <- rbind(intrvntns, vitls)

    # This is a hack to handle cases when there are multiple values given
    # for a particular episode, drug, and timestamp. We just keep the first value
    # listed. Eventually, the drugs data should be cleaned before calling this
    # function.
    #dupes = duplicated(df %>% dplyr::select(-Value))
    #df = df[!dupes, ]
    df <- df[!duplicated(df[, setdiff(names(df), "Value")]), ]
    # End hack

    # align varibles
    df = tidyr::spread_(df, "Variable.Name", "Value")

    # order by decreasing time, by encounter
    df <- df[order(df$patient_id_encounter, -as.numeric(df$time)), ]

    # compute instantaneous OSI
    ret <- compute_osi(df)

    # set time zone
    attr(ret$time, "tzone") <- attr(df$time, "tzone")

    return(ret)
  }
################################################################################

################################################################################
#' Worst Inotrope Score.
#'
#' \code{worst_is} returns the worst (highest) inotrope score, averaged
#' per hour, for each encounter selected.
#'
#' As per Nelson Sanchez-Pinto,
#' Worst Inotrope Score (IS) = highest IS
#' IS = dopamine + dobutamine + (epinephrine + norepinephrine) * 100 +
#' milrinone * 10.
#' The worst inotrope score is selected from among the hourly-averaged scores for
#' each encounter.
#' Encounters with no inotrope score are given 0 as their inotrope score,
#' as per Nelson Sanchez-Pinto.
#'
#' @param enc_info_df \code{data frame}: provides the encounters of interest
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{POSIXct}: Episode.Start.Timestamp
#'    }
#'
#' @param drugs_df \code{data frame}: provides events
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{chr}: Variable.Name
#'    \item \code{num}: Rate.Value
#'    \item \code{POSIXct}: Time.of.Administration
#'    }
#'
#' @return data_frame:
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{num}: worst_is
#'    }
#'
#' @export
################################################################################
worst_is <-
  function(
    enc_info_df,
    drugs_df
  ) {

    # check user input
    expectations = list(
      a = setNames(list("character", "POSIXct"),
                   c("patient_id_encounter", "Episode.Start.Timestamp")),
      b = setNames(list("character", "character", "numeric", "POSIXct"),
                   c("patient_id_encounter", "Variable.Name", "Rate.Value",
                     "Time.of.Administration"))
    )

    # compute instantaneous inotrope score
    score <- inotrope_score(drugs_df)

    # determine hour for each inotrope score
    ret <- merge(score[, c("patient_id_encounter", "Rate.Value", "Time.of.Administration")],
                 enc_info_df[, c("patient_id_encounter", "Episode.Start.Timestamp")],
                 all.y = TRUE)
    ret$hour <- difftime(ret$Time.of.Administration, ret$Episode.Start.Timestamp, units = "hours")
    ret <- ret[, c("patient_id_encounter", "Rate.Value", "hour")]
    ret$hour <- floor(ret$hour) + 1
    ret[is.na(ret)] <- 0 # encounters without inotrope scores get 0
    ret$hour <- as.factor(ret$hour)

    # compute average per hour
    ret <- aggregate(Rate.Value~patient_id_encounter+hour, ret, mean)
    #ret <- dplyr::group_by_(ret, "patient_id_encounter", "hour")
    #dplyr::summarise(ret, mean=mean(Rate.Value))

    # select worse (highest) IS per encounter
    ret <- ret[order(ret$patient_id_encounter, -ret$Rate.Value), ]
    ret <- ret[!duplicated(ret[c("patient_id_encounter")]), ]

    # format
    ret <- data.frame(
      patient_id_encounter = ret$patient_id_encounter,
      worst_is = ret$Rate.Value, stringsAsFactors = FALSE)

    return (ret)
  }
################################################################################

################################################################################
#' Worst INR.
#'
#' \code{worst_inr} returns the worst (highest) INR
#' for each encounter selected.
#'
#' As per Nelson Sanchez-Pinto,
#' worst INR = highest INR
#' Return value will be 1 for encounters with no observations of INR (per
#' Nelson Sanchez-Pinto).
#'
#' @param enc_info_df \code{data frame}: provides the encounters of interest
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    }
#'
#' @param labs \code{data frame}: provides events
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{chr}: Variable.Name
#'    \item \code{num}: Value
#'    }
#'
#' @return data_frame:
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{num}: worst_inr
#'    }
#'
#' @export
################################################################################
worst_inr <-
  function(
    enc_info,
    labs
  ) {

    # check user input
    expectations = list(
      a = setNames(list("character"),
                   c("patient_id_encounter")),
      b = setNames(list("character", "character", "numeric"),
                   c("patient_id_encounter", "Variable.Name", "Value"))
    )

    # obtain relevant data
    ret <- subset(labs, Variable.Name == "INR")
    ret <- subset(ret, select = c("patient_id_encounter", "Value"))

    # select worst (highest) INR per episode
    ret <- ret[order(ret$patient_id_encounter, -ret$Value), ]
    ret <- ret[!duplicated(ret[c("patient_id_encounter")]), ]
    names(ret)[names(ret) == "Value"] <- "worst_inr"

    # impute missing values
    ret <- merge(enc_info["patient_id_encounter"], ret, all.x = TRUE)
    ret[is.na(ret)] <- 1 # patients without inr are set to inr = 1

    return (ret)
  }
################################################################################

################################################################################
#' Worst Glasgow Coma Score.
#'
#' \code{worst_gcs} returns the worst (lowest) Glasgow Coma Score (GCS)
#' for each encounter selected.
#'
#' Glasgow coma score ranges from 3 to 15. 3 being the worst score and 15 the
#' best.
#' Return value will be NA for encounters with no observations of GCS.
#'
#' @param enc_info \code{data frame}: provides the encounters of interest
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    }
#'
#' @param vitals \code{data frame}: provides events
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{chr}: Variable.Name
#'    \item \code{num}: Value
#'    }
#'
#' @return data_frame:
#' \itemize{
#'    \item \code{chr}: patient_id_encounter
#'    \item \code{num}: worst_gcs
#'    }
#'
#' @export
################################################################################
worst_gcs <-
  function(
    enc_info,
    vitals
  ) {

    # check user input
    expectations = list(
      a = setNames(list("character"),
                   c("patient_id_encounter")),
      b = setNames(list("character", "character", "numeric"),
                   c("patient_id_encounter", "Variable.Name", "Value"))
    )

    # obtain relevant data
    gcs <- subset(vitals, Variable.Name == "Glasgow coma scale total")
    ret <- subset(gcs, select = c("patient_id_encounter", "Value"))

    # select worse (lowest) GCS per patient
    ret <- ret[order(ret$patient_id_encounter, ret$Value), ]
    ret <- ret[!duplicated(ret[c("patient_id_encounter")]), ]
    names(ret)[names(ret) == "Value"] <- "worst_gcs"

    # impute missing values as NA
    ret <- merge(enc_info["patient_id_encounter"], ret, all.x = TRUE)

    return (ret)
  }
################################################################################
