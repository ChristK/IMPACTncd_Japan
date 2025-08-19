/**
 * @file IMPACTncd_sim.cpp
 * @brief Simulation module for the IMPACTncd model in Japan.
 *
 * This file contains the simulation functions for disease incidence, diagnosis,
 * mortality, and state resetting. The simulation logic is modularized within
 * the ImpactSim namespace to improve maintainability and clarity.
 */

 // [[Rcpp::depends(dqrng, BH, sitmo)]]
 // [[Rcpp::plugins(cpp11)]]

#include <mystdint.h>
#include <dqrng_generator.h>
#include <dqrng_distribution.h>
#include <pcg_random.hpp>
#include <convert_seed.h>
#include <R_randgen.h>
#include <minimal_int_set.h>
#include <dqrng.h>
#include <Rcpp.h>
#ifdef __linux__
#include <execinfo.h> // backtrace, backtrace_symbols
#endif

using namespace Rcpp;
using namespace std;

// -------------------------
// Preprocessor Macro
// -------------------------
// Use a safe macro that throws an error with a call stack on out-of-bound access.
#define VECT_ELEM(__thisVector__,__thisElem__) VectElem(__thisVector__,__thisElem__)

// -------------------------
// Utility Functions & RNG Setup
// -------------------------

/**
 * @brief Combine two unsigned ints into one 64-bit integer.
 *
 * @param low Lower 32 bits.
 * @param high Higher 32 bits.
 * @return long long Combined 64-bit integer.
 */
long long u32tou64(const unsigned int low, const unsigned int high) {
  return (((uint64_t) high) << 32) | ((uint64_t) low);
}

// Anonymous namespace for RNG initialization.
namespace {
  dqrng::rng64_t init() {
    Rcpp::RNGScope rngScope;
    Rcpp::IntegerVector seed(2, dqrng::R_random_int);
    return dqrng::generator(dqrng::convert_seed<uint64_t>(seed));
  }
  dqrng::rng64_t rng = init();
  using generator = double(*)();
  dqrng::uniform_distribution uniform{};
  generator runif_impl = [] () { return uniform(*rng); };
}

// -------------------------
// Data Structures
// -------------------------

struct infl {
  vector<IntegerVector> disease_prvl;
  vector<NumericVector> mltp;
  vector<int> lag;
};

struct disease_epi {
  string type;
  IntegerVector prvl;
  NumericVector prbl1; // probability for first-year (incident) cases
  NumericVector prbl2; // probability for prevalent cases
  infl influenced_by;
  CharacterVector aggregate;
  double mm_wt;
  bool can_recur;
  bool flag; // flag for incidence
  int cure;
  int death_code;
};

struct disease_meta {
  disease_epi incd;
  disease_epi dgns;
  disease_epi mrtl;
  bool mrtl1flag;
  int seed;
};

struct simul_meta {
  int init_year;
  int age_low;
  IntegerVector pid;
  IntegerVector year;
  IntegerVector age;
  IntegerVector dead;
  IntegerVector mm_count;
  NumericVector mm_score;
};

/**
 * @brief Prepare simulation metadata from input parameters.
 *
 * @param l List containing simulation parameters.
 * @param dt DataFrame with population data.
 * @return simul_meta Metadata used for simulation.
 */
simul_meta get_simul_meta(const List l, DataFrame dt) {
  simul_meta out = {};
  out.init_year  = as<int>(l["init_year"]);
  out.age_low    = as<int>(l["ageL"]);
  out.pid        = dt[as<string>(l["pids"])];
  out.year       = dt[as<string>(l["years"])];
  out.age        = dt[as<string>(l["ages"])];
  out.dead       = dt[as<string>(l["all_cause_mrtl"])];
  out.mm_count   = dt[as<string>(l["cms_count"])];
  out.mm_score   = dt[as<string>(l["cms_score"])];
  return out;
}

// -------------------------
// Stack Trace Functions
// -------------------------
string GetStackTrace(void);

#ifdef __linux__
string GetStackTrace(void)
{
  const int maxNumStackCalls = 1024;
  void *stackAddresses[maxNumStackCalls];
  int numStackCalls = backtrace(stackAddresses, maxNumStackCalls);
  char **stackCallDescriptions = backtrace_symbols(stackAddresses, numStackCalls);
  if (stackCallDescriptions == NULL)
    return string("[No stack trace available].");
  else
  {
    string stackTrace;
    for (int i = 0; i < numStackCalls; ++i)
      stackTrace += string(stackCallDescriptions[i]) + "\n";
    free(stackCallDescriptions);
    return stackTrace;
  }
}
#elif _WIN32
string GetStackTrace(void)
{
  return string("[No stack trace available on Windows].");
}
#else
string GetStackTrace(void)
{
  return string("[Stack trace not available on this platform]");
}
#endif

// -------------------------
// Safe Vector Element Access
// -------------------------

IntegerVector::Proxy VectElem(IntegerVector &v, int index) {
  try { return v(index); }
  catch(const index_out_of_bounds &e) {
    throw out_of_range(string(e.what()) + "\n" + GetStackTrace());
  }
}

NumericVector::Proxy VectElem(NumericVector &v, int index) {
  try { return v(index); }
  catch(const index_out_of_bounds &e) {
    throw out_of_range(string(e.what()) + "\n" + GetStackTrace());
  }
}

CharacterVector::Proxy VectElem(CharacterVector &v, int index) {
  try { return v(index); }
  catch(const index_out_of_bounds &e) {
    throw out_of_range(string(e.what()) + "\n" + GetStackTrace());
  }
}

// -------------------------
// Disease Metadata Preparation
// -------------------------

/**
 * @brief Prepare a disease_meta object for a specific disease.
 *
 * Populates incidence, diagnosis, and mortality settings from the input list.
 *
 * @param diseaseFields List of disease-specific fields.
 * @param dtSynthPop DataFrame containing synthetic population data.
 * @param l List of simulation parameters.
 * @return disease_meta Configured disease metadata.
 */
disease_meta get_disease_meta(const List diseaseFields, DataFrame dtSynthPop, const List l) {
  string diseaseName;
  bool haveDiseaseName = diseaseFields.containsElementNamed("diseaseName");
  if (haveDiseaseName)
    diseaseName = as<string>(diseaseFields["diseaseName"]);

  disease_meta out = {};
  List incd, dgns, mrtl;
  IntegerVector pid = dtSynthPop[as<string>(l["pids"])];

  // Incidence settings
  if (diseaseFields.containsElementNamed("incidence")) {
    incd = diseaseFields["incidence"];
    out.incd.type = as<string>(incd["type"]);
    if (incd.containsElementNamed("prevalence"))
      out.incd.prvl = dtSynthPop[as<string>(incd["prevalence"])];
    if (incd.containsElementNamed("probability"))
      out.incd.prbl1 = dtSynthPop[as<string>(incd["probability"])];
    if (incd.containsElementNamed("aggregate"))
      out.incd.aggregate = as<CharacterVector>(incd["aggregate"]);
    if (incd.containsElementNamed("influenced_by")) {
      List ib = incd["influenced_by"];
      CharacterVector names = ib.names();
      int n = ib.length();
      List ibb;
      if (out.incd.type == "Type0") {
        for (int i = 0; i < n; ++i) {
          ibb = ib[i];
          out.incd.influenced_by.disease_prvl.push_back(dtSynthPop[as<string>(names[i])]);
          out.incd.influenced_by.lag.push_back(as<int>(ibb["lag"]));
        }
      } else {
        for (int i = 0; i < n; ++i) {
          ibb = ib[i];
          out.incd.influenced_by.disease_prvl.push_back(dtSynthPop[as<string>(names[i])]);
          out.incd.influenced_by.mltp.push_back(dtSynthPop[as<string>(ibb["multiplier"])]);
          out.incd.influenced_by.lag.push_back(as<int>(ibb["lag"]));
        }
      }
    }
    out.incd.flag = false;
    out.incd.can_recur = (incd.containsElementNamed("can_recur")) ? as<bool>(incd["can_recur"]) : false;
  }

  // Diagnosis settings
  if (diseaseFields.containsElementNamed("diagnosis")) {
    dgns = diseaseFields["diagnosis"];
    out.dgns.type = as<string>(dgns["type"]);
    if (!dgns.containsElementNamed("mm_wt") || dgns["mm_wt"] == R_NilValue) {
      string errorMessage = "Missing [meta].[diagnosis].[mm_wt] property for " +
        (haveDiseaseName ? diseaseName : "unknown") +
        ". Add [mm_wt] property for this disease in the YAML config file.\n";
      throw std::runtime_error(errorMessage);
    }
    out.dgns.mm_wt = as<double>(dgns["mm_wt"]);
    if (dgns.containsElementNamed("diagnosed"))
      out.dgns.prvl = dtSynthPop[as<string>(dgns["diagnosed"])];
    if (dgns.containsElementNamed("probability"))
      out.dgns.prbl1 = dtSynthPop[as<string>(dgns["probability"])];
    if (out.dgns.type == "Type0") {
      List ib = dgns["influenced_by"];
      CharacterVector names = ib.names();
      int n = ib.length();
      List ibb;
      for (int i = 0; i < n; ++i) {
        ibb = ib[i];
        out.dgns.influenced_by.disease_prvl.push_back(dtSynthPop[as<string>(names[i])]);
      }
    }
    out.dgns.flag = false;
  }

  // Mortality settings
  if (diseaseFields.containsElementNamed("mortality")) {
    mrtl = diseaseFields["mortality"];
    out.mrtl.type = as<string>(mrtl["type"]);
    if (mrtl.containsElementNamed("probability")) {
      out.mrtl.prbl2 = dtSynthPop[as<string>(mrtl["probability"])];
      out.mrtl1flag = false;
    }
    if (mrtl.containsElementNamed("probability1styear")) {
      out.mrtl.prbl1 = dtSynthPop[as<string>(mrtl["probability1styear"])];
      out.mrtl1flag = true;
    }
    if (mrtl.containsElementNamed("cure"))
      out.mrtl.cure = as<int>(mrtl["cure"]);
    if (mrtl.containsElementNamed("code"))
      out.mrtl.death_code = as<int>(mrtl["code"]);
    if (mrtl.containsElementNamed("influenced_by")) {
      List ib = mrtl["influenced_by"];
      CharacterVector names = ib.names();
      int n = ib.length();
      List ibb;
      for (int i = 0; i < n; ++i) {
        ibb = ib[i];
        out.mrtl.influenced_by.disease_prvl.push_back(dtSynthPop[as<string>(names[i])]);
        out.mrtl.influenced_by.mltp.push_back(dtSynthPop[as<string>(ibb["multiplier"])]);
        out.mrtl.influenced_by.lag.push_back(as<int>(ibb["lag"]));
      }
    }
    out.mrtl.flag = false;
  }
  out.seed = as<int>(diseaseFields["seed"]);
  return out;
}

// -------------------------
// Simulation Helper Functions (Modularized)
// -------------------------

namespace ImpactSim {

  /**
   * @brief Reset disease state for a participant after cure or mortality.
   *
   * If a cure flag is set, resets the incidence prevalence and adjusts diagnosis metrics.
   *
   * @param dmeta Disease metadata for the current disease.
   * @param meta Simulation metadata.
   * @param i Current simulation time index.
   */
  inline void ResetDiseaseState(disease_meta &dmeta, simul_meta &meta, int i) {
    if (dmeta.mrtl.flag) {
      VECT_ELEM(dmeta.incd.prvl, i) = 0;
      if ((dmeta.dgns.type == "Type0" || dmeta.dgns.type == "Type1") &&
          dmeta.dgns.mm_wt > 0.0 && VECT_ELEM(dmeta.dgns.prvl, i - 1) > 0) {
        meta.mm_score[i] -= dmeta.dgns.mm_wt;
        meta.mm_count[i]--;
      }
      dmeta.mrtl.flag = false;
    }
  }

  /**
   * @brief Evaluate disease incidence for Type2 diseases.
   *
   * Updates the prevalence vector for a Type2 incidence mechanism.
   *
   * @param dsmeta Vector of disease metadata.
   * @param j Index of the current disease.
   * @param i Current simulation time index.
   * @param rn1 Random number used for incidence determination.
   * @param meta Simulation metadata.
   */
  inline void DiseaseIncidenceType2(vector<disease_meta> &dsmeta, int j, int i, double rn1, simul_meta &meta) {
    if (dsmeta[j].incd.can_recur) {
      if (VECT_ELEM(dsmeta[j].incd.prvl, i - 1) == 0 &&
          rn1 <= VECT_ELEM(dsmeta[j].incd.prbl1, i))
        VECT_ELEM(dsmeta[j].incd.prvl, i) = 1;
      if (dsmeta[j].mrtl.type == "Type2" || dsmeta[j].mrtl.type == "Type4") {
        if (VECT_ELEM(dsmeta[j].incd.prvl, i - 1) > 0 &&
            VECT_ELEM(dsmeta[j].incd.prvl, i - 1) < dsmeta[j].mrtl.cure)
          VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, i - 1) + 1;
      } else {
        if (VECT_ELEM(dsmeta[j].incd.prvl, i - 1) > 0)
          VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, i - 1) + 1;
      }
    } else {
      if (!dsmeta[j].incd.flag &&
          VECT_ELEM(dsmeta[j].incd.prvl, i - 1) == 0 &&
          rn1 <= VECT_ELEM(dsmeta[j].incd.prbl1, i)) {
        VECT_ELEM(dsmeta[j].incd.prvl, i) = 1;
        dsmeta[j].incd.flag = true;
      }
      if (dsmeta[j].mrtl.type == "Type2" || dsmeta[j].mrtl.type == "Type4") {
        if (VECT_ELEM(dsmeta[j].incd.prvl, i - 1) > 0 &&
            VECT_ELEM(dsmeta[j].incd.prvl, i - 1) < dsmeta[j].mrtl.cure)
          VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, i - 1) + 1;
      } else {
        if (VECT_ELEM(dsmeta[j].incd.prvl, i - 1) > 0)
          VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, i - 1) + 1;
      }
    }
  }

  /**
   * @brief Evaluate disease incidence for Type3 diseases with influenced multipliers.
   *
   * Modifies a multiplier based on influencing diseases and updates prevalence.
   *
   * @param dsmeta Vector of disease metadata.
   * @param i Current simulation time index.
   * @param j Index of the current disease.
   * @param mltp Reference multiplier (modified by influencing factors).
   * @param rn1 Random number used for incidence determination.
   * @param meta Simulation metadata.
   */
  inline void DiseaseIncidenceType3(vector<disease_meta> &dsmeta, int i, int j, double &mltp, double rn1, simul_meta &meta) {
    for (size_t k = 0; k < dsmeta[j].incd.influenced_by.disease_prvl.size(); ++k)
    {
      if (VECT_ELEM(dsmeta[j].incd.influenced_by.disease_prvl[k],
                    i - dsmeta[j].incd.influenced_by.lag[k]) > 0) {
        mltp *= VECT_ELEM(dsmeta[j].incd.influenced_by.mltp[k], i);
      }
    }
    if (dsmeta[j].incd.can_recur) {
      if (VECT_ELEM(dsmeta[j].incd.prvl, i - 1) == 0 &&
          rn1 <= VECT_ELEM(dsmeta[j].incd.prbl1, i) * mltp)
        VECT_ELEM(dsmeta[j].incd.prvl, i) = 1;
      if (dsmeta[j].mrtl.type == "Type2" || dsmeta[j].mrtl.type == "Type4") {
        if (VECT_ELEM(dsmeta[j].incd.prvl, i - 1) > 0 &&
            VECT_ELEM(dsmeta[j].incd.prvl, i - 1) < dsmeta[j].mrtl.cure)
          VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, i - 1) + 1;
      } else {
        if (VECT_ELEM(dsmeta[j].incd.prvl, i - 1) > 0)
          VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, i - 1) + 1;
      }
    } else {
      if (!dsmeta[j].incd.flag &&
          VECT_ELEM(dsmeta[j].incd.prvl, i - 1) == 0 &&
          rn1 <= VECT_ELEM(dsmeta[j].incd.prbl1, i) * mltp) {
        VECT_ELEM(dsmeta[j].incd.prvl, i) = 1;
        dsmeta[j].incd.flag = true;
      }
      if (dsmeta[j].mrtl.type == "Type2" || dsmeta[j].mrtl.type == "Type4") {
        if (VECT_ELEM(dsmeta[j].incd.prvl, i - 1) > 0 &&
            VECT_ELEM(dsmeta[j].incd.prvl, i - 1) < dsmeta[j].mrtl.cure)
          VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, i - 1) + 1;
      } else {
        if (VECT_ELEM(dsmeta[j].incd.prvl, i - 1) > 0)
          VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, i - 1) + 1;
      }
    }
    mltp = 1.0;
  }

  /**
   * @brief Evaluate disease incidence based on type and update prevalence.
   *
   * Determines the incidence mechanism (Type0, Type1, etc.) and applies the appropriate logic.
   *
   * @param dsmeta Vector of disease metadata.
   * @param i Current simulation time index.
   * @param j Index of the current disease.
   * @param rn1 Random number used for incidence determination.
   * @param mltp Multiplier for influenced incidence (may be modified).
   * @param meta Simulation metadata.
   */
  inline void EvalDiseaseIncidence(vector<disease_meta> &dsmeta, int i, int j, double rn1, double &mltp, simul_meta &meta) {
    try {
      if (dsmeta[j].incd.type == "Type0") {
        for (size_t k = 0; k < dsmeta[j].incd.influenced_by.disease_prvl.size(); ++k)
        {
          if (VECT_ELEM(dsmeta[j].incd.influenced_by.disease_prvl[k], i) > VECT_ELEM(dsmeta[j].incd.prvl, i))
            VECT_ELEM(dsmeta[j].incd.prvl, i) =
              VECT_ELEM(dsmeta[j].incd.influenced_by.disease_prvl[k], i);
        }
      } else if (dsmeta[j].incd.type == "Type1") {
        if (dsmeta[j].incd.can_recur) {
          if (VECT_ELEM(dsmeta[j].incd.prbl1, i) == 1.0)
            VECT_ELEM(dsmeta[j].incd.prvl, i) =
              VECT_ELEM(dsmeta[j].incd.prvl, i - 1) + 1;
        } else {
          if (VECT_ELEM(dsmeta[j].incd.prvl, i - 1) == 0 &&
              VECT_ELEM(dsmeta[j].incd.prbl1, i) == 1.0)
            VECT_ELEM(dsmeta[j].incd.prvl, i) = 1;
          if (VECT_ELEM(dsmeta[j].incd.prvl, i - 1) > 0)
            VECT_ELEM(dsmeta[j].incd.prvl, i) =
              VECT_ELEM(dsmeta[j].incd.prvl, i - 1) + 1;
        }
      } else if (dsmeta[j].incd.type == "Type2") {
        DiseaseIncidenceType2(dsmeta, j, i, rn1, meta);
      } else if (dsmeta[j].incd.type == "Type3") {
        DiseaseIncidenceType3(dsmeta, i, j, mltp, rn1, meta);
      } else if (dsmeta[j].incd.type == "Type4") {
        // TODO: Implement Type4 incidence logic if needed.
      } else if (dsmeta[j].incd.type == "Type5") {
        // TODO: Implement Type5 incidence logic if needed.
      }
    } catch (std::exception &e) {
      throw std::runtime_error(string("Error within EvalDiseaseIncidence(): ") + e.what());
    }
  }

  /**
   * @brief Evaluate diagnosis events and update diagnostic metrics.
   *
   * Uses the incidence prevalence to determine diagnosis events and updates mortality scores.
   *
   * @param dsmeta Vector of disease metadata.
   * @param rn1 Random number used for diagnosis determination (updated within).
   * @param j Index of the current disease.
   * @param i Current simulation time index.
   * @param meta Simulation metadata.
   */
  inline void EvalDiagnosis(vector<disease_meta> &dsmeta, double &rn1, int j, int i, simul_meta &meta) {
    try {
      rn1 = runif_impl();
      if (dsmeta[j].incd.type != "Universal" && VECT_ELEM(dsmeta[j].incd.prvl, i) > 0 &&
          dsmeta[j].dgns.type == "Type0") {
        for (size_t k = 0; k < dsmeta[j].dgns.influenced_by.disease_prvl.size(); ++k)
        {
          if (dsmeta[j].dgns.influenced_by.disease_prvl[k](i) > VECT_ELEM(dsmeta[j].dgns.prvl, i))
            VECT_ELEM(dsmeta[j].dgns.prvl, i) = dsmeta[j].dgns.influenced_by.disease_prvl[k](i);
        }
      }
      if (dsmeta[j].incd.type != "Universal" && VECT_ELEM(dsmeta[j].incd.prvl, i) > 0 &&
          dsmeta[j].dgns.type == "Type1") {
        if (VECT_ELEM(dsmeta[j].dgns.prvl, i - 1) == 0 &&
            rn1 <= VECT_ELEM(dsmeta[j].dgns.prbl1, i))
          VECT_ELEM(dsmeta[j].dgns.prvl, i) = 1;
        if (VECT_ELEM(dsmeta[j].dgns.prvl, i - 1) > 0)
          VECT_ELEM(dsmeta[j].dgns.prvl, i) =
            VECT_ELEM(dsmeta[j].dgns.prvl, i - 1) + 1;
      }
      if ((dsmeta[j].dgns.type == "Type0" || dsmeta[j].dgns.type == "Type1") &&
          VECT_ELEM(dsmeta[j].dgns.prvl, i) > 0 &&
          dsmeta[j].dgns.mm_wt > 0.0)
        meta.mm_score[i] += dsmeta[j].dgns.mm_wt;
      if ((dsmeta[j].dgns.type == "Type0" || dsmeta[j].dgns.type == "Type1") &&
          VECT_ELEM(dsmeta[j].dgns.prvl, i) > 0 &&
          dsmeta[j].dgns.mm_wt > 0.0)
        meta.mm_count[i]++;
    } catch (std::exception &e) {
      throw std::runtime_error(string("Error within EvalDiagnosis(): ") + e.what());
    }
  }

  /**
   * @brief Evaluate mortality events and record death codes.
   *
   * Determines if a death event occurs based on disease prevalence, mortality probabilities,
   * and influencing factors. Death codes are accumulated in a temporary vector.
   *
   * @param dsmeta Vector of disease metadata.
   * @param tempdead Vector to collect death codes.
   * @param i Current simulation time index.
   * @param j Index of the current disease.
   * @param rn1 Random number used for mortality determination.
   * @param mltp Multiplier for influenced mortality (may be modified).
   * @param meta Simulation metadata.
   */
  inline void EvalMortality(vector<disease_meta> &dsmeta, vector<int> &tempdead, int i, int j, double &rn1, double &mltp, simul_meta &meta) {
    try {
      rn1 = runif_impl();
      if (dsmeta[j].incd.type == "Universal" || VECT_ELEM(dsmeta[j].incd.prvl, i) > 0) {
        // Type 1 mortality
        if (dsmeta[j].mrtl.type == "Type1") {
          if (dsmeta[j].mrtl1flag) {
            if (VECT_ELEM(dsmeta[j].incd.prvl, i) == 1 &&
                rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl1, i))
              tempdead.push_back(dsmeta[j].mrtl.death_code);
            if (VECT_ELEM(dsmeta[j].incd.prvl, i) > 1 &&
                rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i))
              tempdead.push_back(dsmeta[j].mrtl.death_code);
          } else if (rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i)) {
            tempdead.push_back(dsmeta[j].mrtl.death_code);
          }
        }
        // Type 2 mortality
        else if (dsmeta[j].mrtl.type == "Type2") {
          if (VECT_ELEM(dsmeta[j].incd.prvl, i) < dsmeta[j].mrtl.cure) {
            if (dsmeta[j].mrtl1flag) {
              if (VECT_ELEM(dsmeta[j].incd.prvl, i) == 1 &&
                  rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl1, i))
                tempdead.push_back(dsmeta[j].mrtl.death_code);
              if (VECT_ELEM(dsmeta[j].incd.prvl, i) > 1 &&
                  rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i))
                tempdead.push_back(dsmeta[j].mrtl.death_code);
            } else if (rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i)) {
              tempdead.push_back(dsmeta[j].mrtl.death_code);
            }
          } else {
            dsmeta[j].mrtl.flag = true;
          }
        }
        // Type 3 mortality
        else if (dsmeta[j].mrtl.type == "Type3") {
          for (size_t k = 0; k < dsmeta[j].mrtl.influenced_by.disease_prvl.size(); ++k)
          {
            if (VECT_ELEM(dsmeta[j].mrtl.influenced_by.disease_prvl[k],
                          i - dsmeta[j].mrtl.influenced_by.lag[k]) > 0)
              mltp *= VECT_ELEM(dsmeta[j].mrtl.influenced_by.mltp[k], i);
          }
          if (dsmeta[j].mrtl1flag) {
            if (VECT_ELEM(dsmeta[j].incd.prvl, i) == 1 &&
                rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl1, i))
              tempdead.push_back(dsmeta[j].mrtl.death_code);
            if (VECT_ELEM(dsmeta[j].incd.prvl, i) > 1 &&
                rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i) * mltp)
              tempdead.push_back(dsmeta[j].mrtl.death_code);
          } else {
            if (rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i) * mltp)
              tempdead.push_back(dsmeta[j].mrtl.death_code);
          }
          mltp = 1.0;
        }
        // Type 4 mortality
        else if (dsmeta[j].mrtl.type == "Type4") {
          for (size_t k = 0; k < dsmeta[j].mrtl.influenced_by.disease_prvl.size(); ++k)
          {
            if (VECT_ELEM(dsmeta[j].mrtl.influenced_by.disease_prvl[k],
                          i - dsmeta[j].mrtl.influenced_by.lag[k]) > 0)
              mltp *= VECT_ELEM(dsmeta[j].mrtl.influenced_by.mltp[k], i);
          }
          if (VECT_ELEM(dsmeta[j].incd.prvl, i) < dsmeta[j].mrtl.cure) {
            if (dsmeta[j].mrtl1flag) {
              if (VECT_ELEM(dsmeta[j].incd.prvl, i) == 1 &&
                  rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl1, i))
                tempdead.push_back(dsmeta[j].mrtl.death_code);
              if (VECT_ELEM(dsmeta[j].incd.prvl, i) > 1 &&
                  rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i) * mltp)
                tempdead.push_back(dsmeta[j].mrtl.death_code);
            } else if (rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i) * mltp) {
              tempdead.push_back(dsmeta[j].mrtl.death_code);
            }
          } else {
            dsmeta[j].mrtl.flag = true;
          }
          mltp = 1.0;
        }
      }
    } catch (std::exception &e) {
      throw std::runtime_error(string("Error within EvalMortality(): ") + e.what());
    }
  }

} // End ImpactSim namespace

// -------------------------
// Main Simulation Function
// -------------------------

/**
 * @brief Main simulation routine for the IMPACTncd model.
 *
 * Iterates over the population data, applying incidence, diagnosis, and mortality
 * events for each disease. Mortality is resolved by selecting a death cause if multiple
 * are generated.
 *
 * @param dt DataFrame containing population data.
 * @param l List of simulation parameters and disease configurations.
 * @param mc Monte Carlo simulation identifier.
 */
 // [[Rcpp::export]]
void simcpp(DataFrame dt, const List l, const int mc) {
  try {
    uint64_t seed = rng->operator()();
    rng = dqrng::generator<pcg64>(seed);
    using parm_t = decltype(uniform)::param_type;
    uniform.param(parm_t(0.0, 1.0));

    uint64_t _stream = 0;
    uint64_t _seed = 0;
    const int n = dt.nrow(), SIMULANT_ALIVE = 0;

    simul_meta meta = get_simul_meta(l, dt);
    List diseases = l["diseases"];
    const int dn = diseases.length();
    vector<disease_meta> dsmeta(dn);
    for (int j = 0; j < dn; ++j)
      dsmeta[j] = get_disease_meta(diseases[j], dt, l);

    double mltp = 1.0;
    vector<int> tempdead;
    double rn1, rn2;
    int pid_buffer = meta.pid[0]; // flag for when new pid to reset other flags. Holds the last pid
    bool pid_mrk = true;

    for (int i = 0; i < n; ++i) {
      if ((i == 0 || meta.dead[i - 1] == SIMULANT_ALIVE) &&
          meta.year[i] >= meta.init_year && meta.age[i] >= meta.age_low) {

        if (i > 0 && meta.pid[i] == pid_buffer) // same simulant as the last row?
        {
          pid_mrk = false;
        }
        else {
          pid_mrk = true;
          pid_buffer = meta.pid[i];
        }

        _seed = u32tou64(meta.pid[i], meta.year[i]);

        for (int j = 0; j < dn; ++j) {
          _stream = u32tou64(mc, dsmeta[j].seed);
          rng->seed(_seed, _stream);
          rn1 = runif_impl();

          // reset flags for new simulants
          if (pid_mrk)
          {
            dsmeta[j].incd.flag = false; // denotes that incd occurred
            dsmeta[j].mrtl.flag = false; // denotes cure
          }

          // Reset disease state if cure/mortality flag is set.
          ImpactSim::ResetDiseaseState(dsmeta[j], meta, i);

          ImpactSim::EvalDiseaseIncidence(dsmeta, i, j, rn1, mltp, meta);
          ImpactSim::EvalDiagnosis(dsmeta, rn1, j, i, meta);
          ImpactSim::EvalMortality(dsmeta, tempdead, i, j, rn1, mltp, meta);
        }

        if (tempdead.size() == 1)
          meta.dead[i] = tempdead[0];
        else if (tempdead.size() > 1) {
          _stream = u32tou64(mc, 1L);
          rng->seed(_seed, _stream);
          rn2 = runif_impl();
          int ind = (int)(rn2 * 100000000) % tempdead.size();
          meta.dead[i] = tempdead[ind];
        }
        tempdead.clear();
      }
      if (i > 0 && (meta.dead[i - 1] > SIMULANT_ALIVE || IntegerVector::is_na(meta.dead[i - 1])) &&
          meta.year[i] >= meta.init_year && meta.age[i] >= meta.age_low)
        meta.dead[i] = NA_INTEGER;
    }
  } catch (std::exception &e) {
    throw std::runtime_error(string(__FILE__) + ": " + e.what());
  } catch (...) {
    throw std::runtime_error(string("Exception within ") + __FILE__ + "; consider disabling catch(...)");
  }
}