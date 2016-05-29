#test_file("test_encounter_metrics.R")

test_that("instantaneous OSI is correct", {
  test_vitals = data.frame(
    patient_id_encounter = c("1_1", "1_1", "1_1"),
    Variable.Name = "Pulse oximetry (%)",
    Value = c(100, 100, 100),
    End.Time = as.POSIXct(c("2030-03-25 14:34:00", "2030-03-25 17:04:00",
                            "2030-03-25 17:10:00"), tz = "GMT"),
    stringsAsFactors = FALSE)
  test_interventions = data.frame(
    patient_id_encounter = "1_1",
    Variable.Name = c("FiO2", "FiO2", "Mean airway pressure (cmH2O)",
                      "Mean airway pressure (cmH2O)"),
    Value = c(0.65, 0.55, 16, 12),
    Start.Time = as.POSIXct(c("2030-03-25 16:15:00", "2030-03-25 17:13:00",
                              "2030-03-25 16:15:00", "2030-03-26 00:00:00"),
                            tz = "GMT"),
    stringsAsFactors = FALSE)
  answer = data.frame(
    patient_id_encounter = c("1_1", "1_1"),
    oxygen_saturation_index = c(10.4, 10.4),
    time = as.POSIXct(c("2030-03-25 17:10:00", "2030-03-25 17:04:00"),
                      tz = "GMT"),
    stringsAsFactors = FALSE)
  expect_that(
    oxygen_saturation_index(test_interventions, test_vitals),
    equals(answer))
})
