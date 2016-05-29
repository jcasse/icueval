#include <limits>
#include <Rcpp.h>

//' Instantaneous Oxygen Saturation Index (OSI).
//'
//' \code{compute_osi} returns the instantaneous OSI per encounter, per
//' timestamp.
//'
//' As per Nelson Sanchez-Pinto,
//' OSI = 100 * Mean Airway Pressure * FiO2 / Pulse Oximetry,
//' where Meand Ariway Pressure and FiO2 are taken to be the most recent within
//' the last hour preceding Pulse Oximetry. Therefore, each returned OSI
//' will have the time correpsonding to the Pulse Oximetry measurement.
//'
//' @param df \code{data frame}: provides the variable values
//' \itemize{
//'    \item \code{chr:patient_id_encounter}
//'    \item \code{POSIXct:time} event timestamp, used as the timestamp for osi
//'    \item \code{num:FiO2}
//'    \item \code{num:Mean airway pressure (cmH2O)}
//'    \item \code{num:Pulse oximetry (\%)}
//'    \item \code{num:hour} time of event, in hours since episode start
//'    }
//'
//' @return data_frame:
//' \itemize{
//'    \item \code{chr:patient_id_encounter}
//'    \item \code{num:oxygen_saturation_index}
//'    \item \code{POSIXct:time}
//'    }
//'
// [[Rcpp::export]]
Rcpp::DataFrame compute_osi(Rcpp::DataFrame df) {

    // get data frame
    Rcpp::CharacterVector e = df["patient_id_encounter"];
    Rcpp::DatetimeVector t = df["time"];
    Rcpp::NumericVector p = df["Pulse oximetry (%)"];
    Rcpp::NumericVector m = df["Mean airway pressure (cmH2O)"];
    Rcpp::NumericVector f = df["FiO2"];

    // initialize result vectors
    Rcpp::CharacterVector eid = Rcpp::CharacterVector::create();
    Rcpp::NumericVector osi = Rcpp::NumericVector::create();
    std::vector<Rcpp::Datetime> tim(0);

    // compute results
    int n = e.size();
    for (int i = 0; i < n - 1; i++) {

        if (isnan(p[i])) continue; // cannot compute OSI

        // get most recent FiO2 and MAP, within the past hour
        double fio2 = std::numeric_limits<double>::quiet_NaN();
        double mapr = std::numeric_limits<double>::quiet_NaN();
        for (int j = i + 1; j < n; j++) {
            if (e[j] != e[i]) break; // is not current encounter id
            if (t[i] - t[j] >= 3600) break; // event before prior hour
            if (isnan(fio2)) fio2 = f[j];
            if (isnan(mapr)) mapr = m[j];
        }
        if (isnan(fio2) || isnan(mapr)) continue;

        // update results
        eid.push_back(e[i]);
        osi.push_back(100 * mapr * fio2 / p[i]);
        tim.push_back(t[i]);
  }

  // return data frame
  return Rcpp::DataFrame::create(
          Rcpp::Named("patient_id_encounter") = eid,
          Rcpp::Named("oxygen_saturation_index") = osi,
          Rcpp::Named("time") = Rcpp::wrap(tim),
          Rcpp::Named("stringsAsFactors") = false
        );
}
