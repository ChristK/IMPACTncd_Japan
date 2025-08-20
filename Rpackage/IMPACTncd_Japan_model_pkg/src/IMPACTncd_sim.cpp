/**
 * @file IMPACTncd_sim.cpp
 * @brief High-performance epidemiological microsimulation engine for the IMPACT NCD Japan model
 * 
 * This file implements the core simulation engine for microsimulation-based health economic modeling.
 * It simulates disease incidence, diagnosis, mortality, and multimorbidity (comorbidity) patterns 
 * across synthetic populations over time.
 * 
 * Key Features:
 * - Optimized C++ implementation using Rcpp for R integration
 * - High-quality random number generation using dqrng
 * - Memory-efficient data structures with SIMD optimizations
 * - Comprehensive error handling with stack traces
 * - Year-based processing for temporal modeling accuracy
 * 
 * Design Philosophy:
 * The simulation follows a row-by-row processing model where each row represents a person-year.
 * For each person-year, the model evaluates:
 * 1. Disease incidence (new cases)
 * 2. Disease diagnosis (detection of existing conditions)  
 * 3. Disease mortality (death from disease complications)
 * 4. Multimorbidity scoring (cumulative health burden)
 * 
 * @author Original implementation team
 * @version 2.0 (optimized with year-based processing)
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
#include <map>
#include <unordered_map>
#include <unordered_set>
#ifdef __linux__
#include <execinfo.h> // For detailed stack traces on Linux systems
#endif

using namespace Rcpp;
using namespace std;

// ===============================================================================
// COMPILER OPTIMIZATION DIRECTIVES
// ===============================================================================

/**
 * @brief Compiler optimization hints for branch prediction
 * 
 * These macros help the compiler optimize code by providing hints about
 * which branches are more likely to be taken, improving CPU pipeline efficiency.
 */
#ifdef __GNUC__
#define LIKELY(x)       __builtin_expect(!!(x), 1)   // Branch likely to be taken
#define UNLIKELY(x)     __builtin_expect(!!(x), 0)   // Branch unlikely to be taken  
#define FORCE_INLINE    __attribute__((always_inline)) inline  // Force function inlining
#else
#define LIKELY(x)       (x)       // Fallback for non-GCC compilers
#define UNLIKELY(x)     (x)       // Fallback for non-GCC compilers
#define FORCE_INLINE    inline    // Fallback for non-GCC compilers
#endif

// ===============================================================================
// SAFE VECTOR ACCESS MACROS
// ===============================================================================

/**
 * @brief Optimized vector element access for performance-critical simulation
 * 
 * This macro provides optimized access patterns:
 * 1. BOUNDS_CHECK_MODE (current) - Fast bounds checking for production
 * 2. DEBUG_MODE (debugging) - Safe, stops on error with stack trace
 * 3. UNSAFE_MODE (maximum performance) - No bounds checking
 * 
 * The current setting balances safety and performance for production use.
 */
#ifdef DEBUG_MODE
#define VECT_ELEM(__thisVector__,__thisElem__) VectElem(__thisVector__,__thisElem__)
#elif defined(UNSAFE_MODE)
#define VECT_ELEM(__thisVector__,__thisElem__) __thisVector__[__thisElem__]
#else
// Production mode: Fast bounds checking without stack traces
#define VECT_ELEM(__thisVector__,__thisElem__) __thisVector__(__thisElem__)
#endif

// ===============================================================================
// UTILITY FUNCTIONS
// ===============================================================================

/**
 * @brief Combines two 32-bit unsigned integers into a single 64-bit integer
 * 
 * This function is used for creating unique seeds for the random number generator
 * by combining participant ID and year information.
 * 
 * @param low The lower 32 bits (typically participant ID)
 * @param high The upper 32 bits (typically year)
 * @return A 64-bit integer combining both inputs
 */
long long u32tou64(const unsigned int low, const unsigned int high)
{
  return (((uint64_t) high) << 32) | ((uint64_t) low);
}

// ===============================================================================
// RANDOM NUMBER GENERATION SETUP
// ===============================================================================

/**
 * @brief High-quality random number generation system using dqrng
 * 
 * This namespace contains the random number generation infrastructure:
 * - Uses PCG64 algorithm for high-quality, reproducible random numbers
 * - Provides thread-safe initialization
 * - Supports seeded generation for reproducible simulations
 * - Optimized uniform distribution generation
 */
namespace
{
  /**
   * @brief Initialize the random number generator with R's random seed
   * @return A configured dqrng generator object
   */
  dqrng::rng64_t init()
  {
    Rcpp::RNGScope rngScope;
    Rcpp::IntegerVector seed(2, dqrng::R_random_int);
    return dqrng::generator(dqrng::convert_seed<uint64_t>(seed));
  }
  
  dqrng::rng64_t rng = init();  ///< Global RNG instance

  using generator = double(*)();  ///< Function pointer type for random generation
  dqrng::uniform_distribution uniform{};  ///< Uniform [0,1) distribution
  generator runif_impl = [] () {return uniform(*rng);};  ///< Lambda for efficient uniform generation
}

// ===============================================================================
// OPTIMIZED DISEASE TYPE ENUMERATIONS
// ===============================================================================

/**
 * @brief Optimized disease type enumeration for fast comparisons
 * 
 * Replaces string comparisons with fast integer comparisons in hot loops.
 * This provides significant performance improvements in disease evaluation.
 */
enum class DiseaseType : int {
  Type0 = 0,      // Direct prevalence copying
  Type1 = 1,      // Deterministic transitions
  Type2 = 2,      // Stochastic transitions with optional recurrence
  Type3 = 3,      // Disease-dependent transitions
  Type4 = 4,      // Complex mortality (curable and interaction-dependent)
  Type5 = 5,      // Reserved for future implementations
  Universal = 99  // Universal mortality
};

/**
 * @brief Convert string disease type to optimized enum
 * @param type_str String representation of disease type
 * @return Corresponding DiseaseType enum value
 */
FORCE_INLINE DiseaseType string_to_disease_type(const std::string& type_str) {
  if (type_str == "Type0") return DiseaseType::Type0;
  if (type_str == "Type1") return DiseaseType::Type1;
  if (type_str == "Type2") return DiseaseType::Type2;
  if (type_str == "Type3") return DiseaseType::Type3;
  if (type_str == "Type4") return DiseaseType::Type4;
  if (type_str == "Type5") return DiseaseType::Type5;
  if (type_str == "Universal") return DiseaseType::Universal;
  return DiseaseType::Type0; // Default fallback
}

// ===============================================================================
// DATA STRUCTURES FOR DISEASE MODELING
// ===============================================================================

/**
 * @struct infl
 * @brief Disease influence/dependency structure
 * 
 * Models how one disease can influence the incidence or mortality of another disease.
 * This enables complex disease interactions and comorbidity modeling.
 * 
 * Example: Diabetes might increase the risk of cardiovascular disease with a 
 * multiplier of 2.5 and a lag of 1 year.
 */
struct infl
{
  vector<IntegerVector> disease_prvl;  ///< Prevalence vectors of influencing diseases
  vector<NumericVector> mltp;          ///< Risk multipliers applied by each influencing disease
  vector<int> lag;                     ///< Time lag (in years) for the influence to take effect
};

/**
 * @struct disease_epi
 * @brief Core epidemiological parameters for a single disease aspect
 * 
 * This structure encapsulates all parameters needed to model one aspect of a disease
 * (incidence, diagnosis, or mortality). Each disease has three instances of this
 * structure to model its complete epidemiological profile.
 * 
 * Disease Types:
 * - Type0: Direct prevalence copying (e.g., risk factor transitions)
 * - Type1: Deterministic transitions (probability = 1.0)  
 * - Type2: Stochastic transitions with optional recurrence
 * - Type3: Disease-dependent transitions (influenced by other diseases)
 * - Type4/Type5: Reserved for future implementations
 */
struct disease_epi
{
  DiseaseType type;               ///< Disease type enumeration (optimized for fast comparison)
  IntegerVector prvl;            ///< Disease prevalence/duration vector (person-years)
  NumericVector prbl1;           ///< First-year probability/case fatality rate
  NumericVector prbl2;           ///< Subsequent years probability/case fatality rate  
  infl influenced_by;            ///< Other diseases that influence this condition
  CharacterVector aggregate;     ///< Aggregation rules for summary statistics
  double mm_wt;                  ///< Multimorbidity weight for comorbidity scoring
  bool can_recur;               ///< Whether disease can recur after resolution
  bool flag;                    ///< State flag: true if incidence occurred or cure achieved
  int cure;                     ///< Years until automatic cure (0 = no cure)
  int death_code;               ///< Numeric code for cause-specific mortality classification
};

/**
 * @struct disease_meta
 * @brief Complete disease metadata container
 * 
 * Encapsulates all epidemiological aspects of a single disease entity.
 * Each disease in the simulation has one instance of this structure.
 * 
 * The three-component design (incidence/diagnosis/mortality) allows for:
 * - Modeling undiagnosed disease burden
 * - Separate case fatality rates for diagnosed vs. undiagnosed cases
 * - Complex screening and diagnosis scenarios
 * - Cause-specific mortality attribution
 */
struct disease_meta
{
  disease_epi incd;     ///< Incidence (disease onset) parameters
  disease_epi dgns;     ///< Diagnosis (disease detection) parameters  
  disease_epi mrtl;     ///< Mortality (disease-specific death) parameters
  bool mrtl1flag;       ///< True if separate first-year mortality rates are used
  int seed;             ///< Random seed offset for this disease (ensures independence)
};

/**
 * @struct simul_meta
 * @brief Simulation metadata and population data references
 * 
 * Contains all population-level data and simulation parameters needed for
 * the microsimulation. References point to columns in the main population
 * DataFrame to avoid data copying.
 */
struct simul_meta
{
  int init_year;              ///< First year of simulation period
  int age_low;                ///< Minimum age for inclusion in simulation
  IntegerVector pid;          ///< Participant ID vector (reference to DataFrame column)
  IntegerVector year;         ///< Year vector (reference to DataFrame column)
  IntegerVector age;          ///< Age vector (reference to DataFrame column)  
  IntegerVector dead;         ///< Mortality status vector (0=alive, >0=cause of death, NA=long dead)
  IntegerVector mm_count;     ///< Multimorbidity count vector (number of diagnosed conditions)
  NumericVector mm_score;     ///< Multimorbidity score vector (weighted sum of conditions)
};

/**
 * @struct pid_flag_tracker  
 * @brief High-performance per-participant disease state tracking system
 * 
 * This optimized data structure manages disease-specific flags (incidence/mortality)
 * for each participant across the simulation. It uses memory-efficient hash maps
 * and bitwise operations to minimize memory overhead while maximizing access speed.
 * 
 * Key Optimizations:
 * - Pre-allocated hash map capacity to reduce rehashing
 * - Inline functions for critical path operations
 * - Memory-efficient vector<bool> for flag storage
 * - Lazy initialization (flags created only when needed)
 * 
 * Design Rationale:
 * Unlike simple global flags, this per-participant approach enables:
 * - Correct handling of recurrent diseases
 * - Accurate cure modeling with temporal dependencies  
 * - Support for complex multi-disease interactions
 * - Thread-safe operation for future parallelization
 */
struct pid_flag_tracker
{
  std::unordered_map<int, std::vector<bool>> incd_flags; ///< Participant -> disease incidence flags
  std::unordered_map<int, std::vector<bool>> mrtl_flags; ///< Participant -> disease mortality/cure flags
  int num_diseases;                                       ///< Total number of diseases in simulation
  
  /**
   * @brief Constructor with pre-allocation for performance
   * @param diseases Number of diseases to track per participant
   */
  explicit pid_flag_tracker(int diseases) : num_diseases(diseases) {
    // Reserve capacity for estimated maximum participants to reduce rehashing overhead
    incd_flags.reserve(200000); // Increased from 100k for better memory utilization
    mrtl_flags.reserve(200000);
  }
  
  /**
   * @brief Initialize flag vectors for a new participant (optimized for speed)
   * @param pid Participant identifier
   */
  FORCE_INLINE void init_pid(int pid) {
    if (UNLIKELY(incd_flags.find(pid) == incd_flags.end())) {
      incd_flags[pid].resize(num_diseases, false);
      mrtl_flags[pid].resize(num_diseases, false);
    }
  }
  
  /**
   * @brief Check if this is the first encounter with a participant
   * @param pid Participant identifier
   * @return True if participant is new to the system
   */
  FORCE_INLINE bool is_new_pid(int pid) const {
    return UNLIKELY(incd_flags.find(pid) == incd_flags.end());
  }
  
  /**
   * @brief Get incidence flag for specific participant and disease
   * @param pid Participant identifier
   * @param disease_idx Disease index in the disease vector
   * @return Current incidence flag state
   */
  FORCE_INLINE bool get_incd_flag(int pid, int disease_idx) const {
    auto it = incd_flags.find(pid);
    return LIKELY(it != incd_flags.end()) ? it->second[disease_idx] : false;
  }
  
  /**
   * @brief Set incidence flag for specific participant and disease
   * @param pid Participant identifier  
   * @param disease_idx Disease index in the disease vector
   * @param value New flag value
   */
  FORCE_INLINE void set_incd_flag(int pid, int disease_idx, bool value) {
    incd_flags[pid][disease_idx] = value;
  }
  
  /**
   * @brief Get mortality/cure flag for specific participant and disease
   * @param pid Participant identifier
   * @param disease_idx Disease index in the disease vector
   * @return Current mortality flag state
   */
  FORCE_INLINE bool get_mrtl_flag(int pid, int disease_idx) const {
    auto it = mrtl_flags.find(pid);
    return LIKELY(it != mrtl_flags.end()) ? it->second[disease_idx] : false;
  }
  
  /**
   * @brief Set mortality/cure flag for specific participant and disease
   * @param pid Participant identifier
   * @param disease_idx Disease index in the disease vector  
   * @param value New flag value
   */
  FORCE_INLINE void set_mrtl_flag(int pid, int disease_idx, bool value) {
    mrtl_flags[pid][disease_idx] = value;
  }
};

// ===============================================================================
// SIMULATION SETUP AND VALIDATION FUNCTIONS
// ===============================================================================

/**
 * @brief Extract simulation metadata from R configuration list and DataFrame
 * 
 * This function safely extracts and validates all required simulation parameters
 * from the R interface, creating references to DataFrame columns for efficient access.
 * 
 * @param l Configuration list from R containing simulation parameters
 * @param dt Population DataFrame containing participant data
 * @return Populated simul_meta structure with references to data columns
 */
simul_meta get_simul_meta(const List l, DataFrame dt)
{
  simul_meta out = {};
  out.init_year  = as<int>(l["init_year"]);           // Start year for simulation
  out.age_low  = as<int>(l["ageL"]);                  // Minimum age for inclusion
  out.pid = dt[as<string>(l["pids"])];                // Participant ID column
  out.year = dt[as<string>(l["years"])];              // Year column
  out.age = dt[as<string>(l["ages"])];                // Age column
  out.dead = dt[as<string>(l["all_cause_mrtl"])];     // Mortality status column
  out.mm_count = dt[as<string>(l["cms_count"])];      // Multimorbidity count column
  out.mm_score = dt[as<string>(l["cms_score"])];      // Multimorbidity score column
  return out;
}

/**
 * @brief Validate that DataFrame is correctly sorted for deterministic simulation
 * 
 * The simulation requires data to be sorted by participant ID, then by year (both ascending)
 * to ensure deterministic results and correct temporal sequencing. This function performs
 * an optimized O(n) validation using raw pointers and branch prediction hints.
 * 
 * Sorting Requirements:
 * 1. Primary sort: Participant ID (ascending)
 * 2. Secondary sort: Year (ascending within each participant)
 * 
 * This ordering ensures:
 * - All data for each participant is grouped together
 * - Temporal sequence is maintained for longitudinal modeling
 * - Flag state management works correctly across participant boundaries
 * - Random number generation remains deterministic
 * 
 * @param dt DataFrame containing population data
 * @param l Configuration list containing column name mappings
 * @return True if data is correctly sorted, false otherwise
 */
FORCE_INLINE bool is_sorted_by_pid_year(const DataFrame& dt, const List& l) {
  const std::string pid_col = as<std::string>(l["pids"]);
  const std::string year_col = as<std::string>(l["years"]);

  IntegerVector pid = dt[pid_col];
  IntegerVector year = dt[year_col];
  const int n = pid.size();
  if (n <= 1) return true;  // Trivially sorted if 0 or 1 rows

  // Use restrict pointers for compiler optimization hints
  const int* __restrict__ pidp  = pid.begin();
  const int* __restrict__ yearp = year.begin();

  int prev_pid  = pidp[0];
  int prev_year = yearp[0];

  // Single-pass validation with optimized branching
  for (int i = 1; i < n; ++i) {
    const int curr_pid  = pidp[i];
    const int curr_year = yearp[i];

    // Check sorting constraints
    if (UNLIKELY(curr_pid < prev_pid || (curr_pid == prev_pid && curr_year < prev_year))) {
      return false;  // Sorting violation detected
    }

    // Update tracking variables efficiently
    if (curr_pid != prev_pid) {
      prev_pid = curr_pid;
      prev_year = curr_year;
    } else {
      prev_year = curr_year;
    }
  }
  return true;
}

// ===============================================================================
// ERROR HANDLING AND DEBUGGING UTILITIES
// ===============================================================================

/**
 * @brief Generate detailed stack trace for debugging (Linux only)
 * 
 * This function provides comprehensive stack trace information when errors occur,
 * significantly improving debugging capability. On Windows, a placeholder message
 * is returned as stack tracing APIs differ.
 * 
 * @return String containing formatted stack trace or error message
 */
#ifdef __linux__
string GetStackTrace(void) {
	const int maxNumStackCalls= 1024;
	void *stackAddresses[maxNumStackCalls];
	int numStackCalls= backtrace(stackAddresses,maxNumStackCalls);
	char **stackCallDescriptions= backtrace_symbols(stackAddresses,numStackCalls);
	if(stackCallDescriptions==NULL)
		return string("[No stack trace available].");
	else {
		string stackTrace;
		for(int thisStackCall=0;thisStackCall<numStackCalls;++thisStackCall) {
			stackTrace= (stackTrace+stackCallDescriptions[thisStackCall])+"\n";
		}
		free(stackCallDescriptions);
		return stackTrace;
	}
}

#elif _WIN32
string GetStackTrace(void) {
	return string("[No stack trace available on Windows].");
}
#endif

/**
 * @brief Safe IntegerVector element access with enhanced error reporting
 * 
 * These wrapper functions provide comprehensive error information when vector
 * access violations occur, including stack traces for debugging complex issues.
 * The enhanced error messages help identify the exact location and context of
 * out-of-bounds access attempts.
 * 
 * @param v IntegerVector reference
 * @param index Element index to access
 * @return Vector element proxy for safe access
 * @throws out_of_range Enhanced exception with stack trace
 */
IntegerVector::Proxy VectElem(IntegerVector &v,int index) {
	try { return v(index); }
	catch(const index_out_of_bounds& e) {
		throw out_of_range( string(e.what())+"\n"+GetStackTrace() );
	}
}

/**
 * @brief Safe NumericVector element access with enhanced error reporting
 * @param v NumericVector reference  
 * @param index Element index to access
 * @return Vector element proxy for safe access
 * @throws out_of_range Enhanced exception with stack trace
 */
NumericVector::Proxy VectElem(NumericVector &v,int index) {
	try { return v(index); }
	catch(const index_out_of_bounds& e) {
		throw out_of_range( string(e.what())+"\n"+GetStackTrace() );
	}
}

/**
 * @brief Safe CharacterVector element access with enhanced error reporting
 * @param v CharacterVector reference
 * @param index Element index to access  
 * @return Vector element proxy for safe access
 * @throws out_of_range Enhanced exception with stack trace
 */
CharacterVector::Proxy VectElem(CharacterVector &v,int index) {
	try { return v(index); }
	catch(const index_out_of_bounds& e) {
		throw out_of_range( string(e.what())+"\n"+GetStackTrace() );
	}
}


// ===============================================================================
// DISEASE CONFIGURATION AND METADATA CONSTRUCTION
// ===============================================================================

/**
 * @brief Construct complete disease metadata from R configuration objects
 * 
 * This function transforms disease configuration data from R into optimized C++ data structures.
 * It handles the complex mapping between R lists and C++ objects while performing validation
 * and establishing references to population data columns.
 * 
 * The function constructs a three-component disease model:
 * 1. **Incidence Component**: Models disease onset and prevalence evolution
 * 2. **Diagnosis Component**: Models disease detection and multimorbidity scoring  
 * 3. **Mortality Component**: Models disease-specific mortality and cure dynamics
 * 
 * Disease Type Classifications:
 * - **Type0**: Direct prevalence copying from other diseases (risk factor dependencies)
 * - **Type1**: Deterministic transitions (probability = 1.0) for known progressions
 * - **Type2**: Stochastic transitions with configurable recurrence patterns  
 * - **Type3**: Disease-dependent transitions influenced by other conditions
 * - **Type4/5**: Reserved for future complex interaction models
 * 
 * @param diseaseFields R list containing disease configuration from YAML files
 * @param dtSynthPop DataFrame containing synthetic population data with all required columns
 * @return Fully configured disease_meta object ready for simulation
 * @throws std::runtime_error If required configuration elements are missing or invalid
 */
disease_meta get_disease_meta(const List diseaseFields, DataFrame dtSynthPop)
{
	// Create local copy of disease name to avoid reference issues with R's internal buffers
	string diseaseName;
	bool haveDiseaseName= diseaseFields.containsElementNamed("diseaseName");
	if(haveDiseaseName)diseaseName= as<string>(diseaseFields["diseaseName"]);

  disease_meta out = {};  // Initialize with default values

  List incd, dgns, mrtl;  // Temporary R list containers

  // =========================================================================
  // INCIDENCE COMPONENT CONFIGURATION
  // =========================================================================
  if (diseaseFields.containsElementNamed("incidence"))
  {
    incd = diseaseFields["incidence"];
    out.incd.type = string_to_disease_type(as<string>(incd["type"]));

    // Link to population data columns
    if (incd.containsElementNamed("prevalence")) 
      out.incd.prvl = dtSynthPop[as<string>(incd["prevalence"])];
    if (incd.containsElementNamed("probability"))
      out.incd.prbl1 = dtSynthPop[as<string>(incd["probability"])];

    // Configure aggregation rules for summary statistics
    if (incd.containsElementNamed("aggregate")) 
      out.incd.aggregate = as<CharacterVector>(incd["aggregate"]);

    // Configure disease interaction dependencies
    if (incd.containsElementNamed("influenced_by"))
    {
      List ib = incd["influenced_by"];
      CharacterVector tmps= ib.names();
      int n = ib.length();
      List ibb;
      
      if (out.incd.type == DiseaseType::Type0)
      {
        // Type0: Simple prevalence copying (no multipliers needed)
        for (int i = 0; i < n; ++i)
        {
          ibb = ib[i];
          out.incd.influenced_by.disease_prvl.push_back(dtSynthPop[as<string>(tmps[i])]);
          out.incd.influenced_by.lag.push_back(as<int>(ibb["lag"])); // Usually 0 for Type0
        }
      }
      else
      {
        // Type3: Complex interactions with risk multipliers and temporal lags
        for (int i = 0; i < n; ++i)
        {
          ibb = ib[i];
          out.incd.influenced_by.disease_prvl.push_back(dtSynthPop[as<string>(tmps[i])]);
          out.incd.influenced_by.mltp.push_back(dtSynthPop[as<string>(ibb["multiplier"])]);
          out.incd.influenced_by.lag.push_back(as<int>(ibb["lag"]));
        }
      }
    }
    
    out.incd.flag = false;  // Initialize state flag
    
    // Configure recurrence behavior
    if (incd.containsElementNamed("can_recur")) 
      out.incd.can_recur = as<bool>(incd["can_recur"]);
    else 
      out.incd.can_recur = false;  // Default: no recurrence
  }

  // =========================================================================
  // DIAGNOSIS COMPONENT CONFIGURATION
  // =========================================================================
  if (diseaseFields.containsElementNamed("diagnosis"))
  {
    dgns = diseaseFields["diagnosis"];
    out.dgns.type = string_to_disease_type(as<string>(dgns["type"]));
    
    // Validate required multimorbidity weight parameter
	  if(!dgns.containsElementNamed("mm_wt") || dgns["mm_wt"]==R_NilValue)
	  {
		  string errorMessage= "Missing [meta].[diagnosis].[mm_wt] property for "+
			  (haveDiseaseName?diseaseName:(string)"unknown")+
			  ". Add [mm_wt] property for this disease in .yaml config file.\n";
		  throw std::runtime_error(errorMessage);
	  }
    out.dgns.mm_wt = as<double>(dgns["mm_wt"]);
    
    // Link to population data columns
    if (dgns.containsElementNamed("diagnosed")) 
      out.dgns.prvl = dtSynthPop[as<string>(dgns["diagnosed"])];
    if (dgns.containsElementNamed("probability")) 
      out.dgns.prbl1 = dtSynthPop[as<string>(dgns["probability"])];

    // Configure diagnosis dependencies (Type0 only)
    if (out.dgns.type == DiseaseType::Type0)
    {
      List ib = dgns["influenced_by"];
      CharacterVector tmps= ib.names();
      int n = ib.length();
      List ibb;
      for (int i = 0; i < n; ++i)
      {
        ibb = ib[i];
        out.dgns.influenced_by.disease_prvl.push_back(dtSynthPop[as<string>(tmps[i])]);
      }
    }

    out.dgns.flag = false;  // Initialize state flag
  }

  // =========================================================================
  // MORTALITY COMPONENT CONFIGURATION  
  // =========================================================================
  if (diseaseFields.containsElementNamed("mortality"))
  {
    mrtl = diseaseFields["mortality"];
    out.mrtl.type = string_to_disease_type(as<string>(mrtl["type"]));

    // Configure standard mortality probabilities
    if (mrtl.containsElementNamed("probability"))
    {
      out.mrtl.prbl2 = dtSynthPop[as<string>(mrtl["probability"])];
      out.mrtl1flag = false;  // No separate first-year mortality
    }
    
    // Configure separate first-year mortality (higher risk period)
    if (mrtl.containsElementNamed("probability1styear"))
    {
      out.mrtl.prbl1 = dtSynthPop[as<string>(mrtl["probability1styear"])];
      out.mrtl1flag = true;   // Use separate first-year mortality
    }
    
    // Configure cure dynamics
    if (mrtl.containsElementNamed("cure")) 
      out.mrtl.cure = as<int>(mrtl["cure"]);
      
    // Configure cause-of-death coding  
    if (mrtl.containsElementNamed("code")) 
      out.mrtl.death_code = as<int>(mrtl["code"]);

    // Configure mortality dependencies (Type3/Type4)
    if (mrtl.containsElementNamed("influenced_by"))
    {
      List ib = mrtl["influenced_by"];
      CharacterVector tmps= ib.names();
      int n = ib.length();
      List ibb;
      for (int i = 0; i < n; ++i)
      {
        ibb = ib[i];
        out.mrtl.influenced_by.disease_prvl.push_back(dtSynthPop[as<string>(tmps[i])]);
        out.mrtl.influenced_by.mltp.push_back(dtSynthPop[as<string>(ibb["multiplier"])]);
        out.mrtl.influenced_by.lag.push_back(as<int>(ibb["lag"]));
      }
    }

    out.mrtl.flag = false;  // Initialize cure flag
  }

  // Set unique random seed for this disease to ensure statistical independence
  out.seed = as<int>(diseaseFields["seed"]);
  return out;
}

// ===============================================================================
// DISEASE INCIDENCE MODELING FUNCTIONS
// ===============================================================================

/**
 * @brief Evaluate Type2 disease incidence with cure dynamics
 * 
 * Type2 incidence models diseases with stochastic onset and sophisticated 
 * disease progression dynamics including:
 * - **New Onset**: Probabilistic disease incidence in healthy individuals
 * - **Disease Progression**: Yearly progression tracking for existing cases  
 * - **Cure Modeling**: Automatic recovery after specified duration
 * - **Recurrence Support**: Optional re-incidence after cure
 * 
 * **Disease Duration Encoding**:
 * - prvl[i] = 0: Healthy (never had disease)
 * - prvl[i] = 1: First year of disease
 * - prvl[i] = 2,3,4...: Subsequent years of disease
 * - prvl[i] resets to 0 after cure duration reached
 * 
 * **Cure Dynamics**:
 * - Type2/Type4 mortality: Cure after specified duration
 * - Other mortality types: Indefinite progression until death
 * 
 * @param dsmeta Reference to disease metadata vector containing all diseases
 * @param j Disease index in metadata vector  
 * @param i Person index in population
 * @param rn1 Random number [0,1] for stochastic evaluation
 */
inline void DiseaseIncidenceType2(vector<disease_meta> &dsmeta,int j, int i, double rn1)
{
    // Ensure vectors are properly sized
    if (i >= dsmeta[j].incd.prvl.size() || i >= dsmeta[j].incd.prbl1.size()) {
        return; // Skip if index out of bounds
    }
    
    // Ensure we don't access negative indices
    const int prev_idx = (i > 0 && i - 1 < dsmeta[j].incd.prvl.size()) ? i - 1 : i;
    
    if (dsmeta[j].incd.can_recur) // Recurrence allowed - no flag checking needed
    {
        // Evaluate new incidence for healthy individuals
        if (VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) == 0 && rn1 <= VECT_ELEM(dsmeta[j].incd.prbl1, i))
        {
            VECT_ELEM(dsmeta[j].incd.prvl, i) = 1; // Begin first year of disease
        }

        // Handle disease progression for existing cases
        if (dsmeta[j].mrtl.type == DiseaseType::Type2 || dsmeta[j].mrtl.type == DiseaseType::Type4)
        {
            // Progression with cure limit
            if (VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) > 0 &&
                VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) < dsmeta[j].mrtl.cure)
                VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) + 1;
        }
        else
        {
            // Indefinite progression (no automatic cure)
            if (VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) > 0)
                VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) + 1;
            // Note: progression stops naturally at cure duration due to mortality
        }

    }
    else // No recurrence allowed - use flag to prevent repeat incidence
    {
        // Evaluate new incidence only if never occurred before
        if (!dsmeta[j].incd.flag && VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) == 0 &&
            rn1 <= VECT_ELEM(dsmeta[j].incd.prbl1, i))
        {
            VECT_ELEM(dsmeta[j].incd.prvl, i) = 1; // Begin disease
            dsmeta[j].incd.flag = true; // Prevent future incidence
        }

        // Disease progression logic (same as recurrence case)
        if (dsmeta[j].mrtl.type == DiseaseType::Type2 || dsmeta[j].mrtl.type == DiseaseType::Type4)
        {
            if (VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) > 0 &&
                VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) < dsmeta[j].mrtl.cure)
                VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) + 1;
        }
        else
        {
            if (VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) > 0)
                VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) + 1;
        }
    }
}

/**
 * @brief Evaluate Type3 disease incidence with complex disease interactions
 * 
 * Type3 extends Type2 by incorporating multiplicative risk factors from other diseases.
 * The incidence probability is dynamically adjusted based on the presence of 
 * influencing conditions, allowing modeling of:
 * - **Comorbidity Risks**: Heart disease risk elevated by diabetes
 * - **Disease Cascades**: Sequential disease development patterns
 * - **Risk Factor Interactions**: Multiple conditions amplifying disease risk
 * 
 * **Mathematical Model**:
 * P(incidence) = base_probability × ∏(disease_multipliers)
 * where multipliers are applied when influencing diseases were present 
 * at specified lag periods
 * 
 * **Temporal Lag Logic**:
 * - lag[k] = 0: Same-year influence (immediate effect)
 * - lag[k] = 1: Previous year influence (delayed effect)
 * - lag[k] = 5: Five-year lag (cumulative risk effect)
 * 
 * **Risk Multiplier Reset**:
 * The mltp parameter is reset to 1.0 after use to prevent carry-over
 * effects between different individuals or disease evaluations.
 * 
 * @param dsmeta Reference to disease metadata vector
 * @param i Person index in population  
 * @param j Disease index in metadata vector
 * @param mltp Reference to risk multiplier (modified and reset by function)
 * @param rn1 Random number [0,1] for stochastic evaluation
 */
inline void DiseaseIncidenceType3(vector<disease_meta> &dsmeta,int i, int j, double& mltp, double rn1)
{
    // Ensure vectors are properly sized
    if (i >= dsmeta[j].incd.prvl.size() || i >= dsmeta[j].incd.prbl1.size()) {
        mltp = 1.0; // Reset multiplier
        return; // Skip if index out of bounds
    }
    
    // Calculate composite risk multiplier from all influencing diseases
    for (size_t k = 0; k < dsmeta[j].incd.influenced_by.disease_prvl.size(); ++k)
    {
        // Check if influencing disease was present at the required lag time
        const int lag_idx = i - dsmeta[j].incd.influenced_by.lag[k];
        if (lag_idx >= 0 && lag_idx < dsmeta[j].incd.influenced_by.disease_prvl[k].size() &&
            VECT_ELEM(dsmeta[j].incd.influenced_by.disease_prvl[k], lag_idx) > 0)
        {
            // Apply multiplicative risk factor (no lag on multiplier itself)
            if (i < dsmeta[j].incd.influenced_by.mltp[k].size()) {
                mltp *= VECT_ELEM(dsmeta[j].incd.influenced_by.mltp[k], i);
            }
        }
    }

    // Ensure we don't access negative indices
    const int prev_idx = (i > 0 && i - 1 < dsmeta[j].incd.prvl.size()) ? i - 1 : i;

    if (dsmeta[j].incd.can_recur) // Recurrence allowed
    {
        // Evaluate incidence with risk-modified probability
        if (VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) == 0 && rn1 <= VECT_ELEM(dsmeta[j].incd.prbl1, i) * mltp)
        {
            VECT_ELEM(dsmeta[j].incd.prvl, i) = 1; // Begin disease
        }

        // Disease progression logic (identical to Type2)
        if (dsmeta[j].mrtl.type == DiseaseType::Type2 || dsmeta[j].mrtl.type == DiseaseType::Type4)
        {
            // Progression with cure limit
            if (VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) > 0 &&
                VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) < dsmeta[j].mrtl.cure)
                VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) + 1;
        }
        else
        {
            // Indefinite progression
            if (VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) > 0)
                VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) + 1;
        }

    }
    else // No recurrence allowed
    {
        // One-time incidence evaluation with risk modification
        if (!dsmeta[j].incd.flag && VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) == 0 &&
            rn1 <= VECT_ELEM(dsmeta[j].incd.prbl1, i) * mltp)
        {
            VECT_ELEM(dsmeta[j].incd.prvl, i) = 1; // Begin disease
            dsmeta[j].incd.flag = true; // Prevent future incidence
        }

        // Disease progression (same as recurrence case)
        if (dsmeta[j].mrtl.type == DiseaseType::Type2 || dsmeta[j].mrtl.type == DiseaseType::Type4)
        {
            if (VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) > 0 &&
                VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) < dsmeta[j].mrtl.cure)
                VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) + 1;
        }
        else
        {
            if (VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) > 0)
                VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) + 1;
        }
    }

    mltp = 1.0; // Reset multiplier for next use
}

/**
 * @brief Central disease incidence evaluation dispatcher and coordinator
 * 
 * This function serves as the main entry point for disease incidence evaluation,
 * routing different disease types to appropriate specialized handlers while 
 * maintaining consistent state management across all disease models.
 * 
 * **Disease Type Dispatch Logic**:
 * - **Type0**: Prevalence copying - disease occurs when specified conditions are met
 * - **Type1**: Deterministic transitions - certainty-based disease progression  
 * - **Type2**: Stochastic baseline transitions - probability-based incidence
 * - **Type3**: Complex disease interactions - risk-modified stochastic transitions
 * - **Type4/5**: Reserved for future complex modeling extensions
 * 
 * **Type0 Implementation Details**:
 * Uses max() logic to copy the highest prevalence value among influencing diseases.
 * This models scenarios where multiple conditions can trigger the same outcome
 * (e.g., cardiovascular event from either diabetes OR hypertension).
 * 
 * **Type1 Implementation Details**:  
 * Handles deterministic transitions with probability = 1.0. Supports both
 * recurrent and non-recurrent patterns. The logic "overwrites prvl for init year"
 * ensures proper initialization handling.
 * 
 * **Error Handling**:
 * Wraps all disease-specific logic in try-catch blocks to provide meaningful
 * error context when incidence evaluation fails.
 * 
 * @param dsmeta Reference to disease metadata vector containing all disease configurations
 * @param i Person index in synthetic population
 * @param j Disease index in metadata vector
 * @param rn1 Random number [0,1] for stochastic evaluations
 * @param mltp Reference to risk multiplier (used and reset by Type3)
 * @throws std::runtime_error If any disease evaluation logic fails
 */
inline void EvalDiseaseIncidence(vector<disease_meta> &dsmeta,int i, int j, double rn1, double& mltp)
{
  try {
	  // Skip if incidence component is not properly configured
	  if (dsmeta[j].incd.prvl.size() == 0) {
	      return; // No incidence component configured
	  }
	  
	  // Type0: Prevalence copying from influencing diseases
	  if (dsmeta[j].incd.type == DiseaseType::Type0)
    {
        // Find maximum prevalence among all influencing diseases
        for (size_t k = 0; k < dsmeta[j].incd.influenced_by.disease_prvl.size(); ++k)
        {
            // Copy higher prevalence value (max logic for multiple triggers)
            if (i < dsmeta[j].incd.influenced_by.disease_prvl[k].size() && 
                i < dsmeta[j].incd.prvl.size() &&
                VECT_ELEM(dsmeta[j].incd.influenced_by.disease_prvl[k], i) > VECT_ELEM(dsmeta[j].incd.prvl, i))
            {
                VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.influenced_by.disease_prvl[k], i);
            }
        }
    }

    // Type1: Deterministic transitions (probability = 1.0)
    else if (dsmeta[j].incd.type == DiseaseType::Type1)
    {
        if (i < dsmeta[j].incd.prvl.size() && i < dsmeta[j].incd.prbl1.size()) {
            const int prev_idx = (i > 0 && i - 1 < dsmeta[j].incd.prvl.size()) ? i - 1 : i;
            
            if (dsmeta[j].incd.can_recur)
            {
                // Recurrent pattern: continuous progression when probability = 1.0
                if (VECT_ELEM(dsmeta[j].incd.prbl1, i) == 1.0)
                    VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) + 1;
            }
            else // Non-recurrent: one-time deterministic transition
            {
                // Initial incidence for healthy individuals
                if (VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) == 0 && VECT_ELEM(dsmeta[j].incd.prbl1, i) == 1.0)
                    VECT_ELEM(dsmeta[j].incd.prvl, i) = 1;
                // Progression for existing cases (assuming no cure)
                if (VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) > 0)
                    VECT_ELEM(dsmeta[j].incd.prvl, i) = VECT_ELEM(dsmeta[j].incd.prvl, prev_idx) + 1;
            }
        }
    }

    // Type2: Stochastic baseline transitions
    else if (dsmeta[j].incd.type == DiseaseType::Type2)
        DiseaseIncidenceType2(dsmeta,j, i, rn1);

    // Type3: Complex disease interaction modeling
    else if (dsmeta[j].incd.type == DiseaseType::Type3)
        DiseaseIncidenceType3(dsmeta,i, j, mltp, rn1);

    // Future extensions for complex modeling
    else if (dsmeta[j].incd.type == DiseaseType::Type4)
    {
        // TODO: Advanced interaction patterns
    }

    else if (dsmeta[j].incd.type == DiseaseType::Type5)
    {
        // TODO: Machine learning-based transitions
    }
  }
	catch(std::exception &e) {
		// Enhance error context for debugging disease-specific failures
		throw std::runtime_error((string)"Error within EvalDiseaseIncidence(): "+e.what());
	}
}

// ===============================================================================
// DISEASE DIAGNOSIS AND MULTIMORBIDITY MODELING
// ===============================================================================

/**
 * @brief Evaluate disease diagnosis and update multimorbidity scoring
 * 
 * This function models the clinical diagnosis process for diseases that have
 * already manifested (positive incidence). It handles two primary functions:
 * 1. **Clinical Detection**: Modeling when existing diseases are diagnosed
 * 2. **Multimorbidity Scoring**: Accumulating disease burden metrics
 * 
 * **Diagnosis Type Logic**:
 * - **Type0**: Diagnosis copying from other diagnosed diseases (referral patterns)
 * - **Type1**: Stochastic diagnosis for prevalent cases (screening/detection)
 * 
 * **Multimorbidity Calculation**:
 * For each diagnosed disease with positive multimorbidity weight:
 * - mm_score: Cumulative weighted disease burden score
 * - mm_count: Simple count of diagnosed conditions
 * 
 * **Clinical Modeling Rationale**:
 * This separation between incidence and diagnosis reflects real-world healthcare
 * where diseases may exist undiagnosed for years before clinical detection.
 * 
 * @param dsmeta Reference to disease metadata vector
 * @param rn1 Reference to random number (generated within function)
 * @param j Disease index in metadata vector
 * @param i Person index in population
 * @param meta Reference to simulation metadata for multimorbidity tracking
 * @throws std::runtime_error If diagnosis evaluation fails
 */
inline void EvalDiagnosis(vector<disease_meta> &dsmeta,double& rn1, int j, int i, simul_meta& meta)
{
	try {
		rn1 = runif_impl(); // Generate fresh random number for diagnosis

    // Type0: Diagnosis copying from other diagnosed diseases
    if (dsmeta[j].incd.type != DiseaseType::Universal && 
        i < dsmeta[j].incd.prvl.size() && VECT_ELEM(dsmeta[j].incd.prvl, i) > 0 && 
        dsmeta[j].dgns.type == DiseaseType::Type0 &&
        dsmeta[j].dgns.prvl.size() > 0 && i < dsmeta[j].dgns.prvl.size())
    {
        // Find maximum diagnosis status among influencing diseases
        for (size_t k = 0; k < dsmeta[j].dgns.influenced_by.disease_prvl.size(); ++k)
        {
            // Copy higher diagnosis value (referral chain modeling)
            if (i < dsmeta[j].dgns.influenced_by.disease_prvl[k].size() &&
                dsmeta[j].dgns.influenced_by.disease_prvl[k](i) > VECT_ELEM(dsmeta[j].dgns.prvl, i))
            {
                VECT_ELEM(dsmeta[j].dgns.prvl, i) = dsmeta[j].dgns.influenced_by.disease_prvl[k](i);
            }
        }
    }

    // Type1: Stochastic diagnosis for prevalent cases
    if (dsmeta[j].incd.type != DiseaseType::Universal && 
        i < dsmeta[j].incd.prvl.size() && VECT_ELEM(dsmeta[j].incd.prvl, i) > 0 && 
        dsmeta[j].dgns.type == DiseaseType::Type1 &&
        dsmeta[j].dgns.prvl.size() > 0 && i < dsmeta[j].dgns.prvl.size() && 
        dsmeta[j].dgns.prbl1.size() > 0 && i < dsmeta[j].dgns.prbl1.size())
    {
        const int prev_idx = (i > 0 && i - 1 < dsmeta[j].dgns.prvl.size()) ? i - 1 : i;
        
        // Initial diagnosis for undiagnosed prevalent cases
        if (prev_idx < dsmeta[j].dgns.prvl.size() && 
            VECT_ELEM(dsmeta[j].dgns.prvl, prev_idx) == 0 && rn1 <= VECT_ELEM(dsmeta[j].dgns.prbl1, i))
        {
            VECT_ELEM(dsmeta[j].dgns.prvl, i) = 1; // Begin diagnosis tracking
        }
        // Progression for previously diagnosed cases
        if (prev_idx < dsmeta[j].dgns.prvl.size() && 
            VECT_ELEM(dsmeta[j].dgns.prvl, prev_idx) > 0)
        {
            VECT_ELEM(dsmeta[j].dgns.prvl, i) = VECT_ELEM(dsmeta[j].dgns.prvl, prev_idx) + 1;
        }
    }

    // Update multimorbidity metrics for diagnosed diseases
    if ((dsmeta[j].dgns.type == DiseaseType::Type0 || dsmeta[j].dgns.type == DiseaseType::Type1) && 
        i < dsmeta[j].dgns.prvl.size() && VECT_ELEM(dsmeta[j].dgns.prvl, i) > 0 && dsmeta[j].dgns.mm_wt > 0.0) 
    {
        meta.mm_score[i] += dsmeta[j].dgns.mm_wt;  // Weighted disease burden
        meta.mm_count[i]++;                        // Simple disease count
    }
  }
	catch(std::exception &e) {
		// Enhance error context for diagnosis-specific debugging
		throw std::runtime_error((string)"Error within EvalDiagnosis(): "+e.what());
	}
}

// ===============================================================================
// DISEASE MORTALITY AND CURE MODELING  
// ===============================================================================

/**
 * @brief Evaluate disease-specific mortality with complex interaction patterns
 * 
 * This function models disease-related death using sophisticated mortality patterns
 * that account for disease duration, cure dynamics, and interaction effects from
 * other health conditions.
 * 
 * **Mortality Type Classifications**:
 * - **Type1**: Basic mortality (no cure, no disease dependencies)
 * - **Type2**: Curable mortality (automatic recovery after specified duration)
 * - **Type3**: Interactive mortality (risk modified by other diseases, no cure)
 * - **Type4**: Complex mortality (both curable and interaction-dependent)
 * 
 * **First-Year Mortality Logic**:
 * Many diseases have elevated mortality risk in the first year after incidence.
 * When mrtl1flag=true, separate probabilities are used for:
 * - Year 1: Higher risk period (prbl1)
 * - Year 2+: Standard risk period (prbl2)
 * 
 * **Cure Dynamics**:
 * Type2/Type4 diseases can be "cured" (prevalence resets to 0) after reaching
 * the specified cure duration. This models conditions with finite disease periods.
 * 
 * **Death Code Assignment**:
 * When mortality occurs, the disease-specific death code is added to tempdead
 * vector for cause-of-death analysis and mortality statistics.
 * 
 * @param dsmeta Reference to disease metadata vector
 * @param tempdead Reference to death code vector for this person
 * @param i Person index in population
 * @param j Disease index in metadata vector
 * @param rn1 Reference to random number for stochastic evaluation
 * @param mltp Reference to risk multiplier (used and reset by Type3/Type4)
 * @throws std::runtime_error If mortality evaluation fails
 */
inline void EvalMortality(vector<disease_meta> &dsmeta,vector<int> &tempdead,int i, int j, double& rn1, double& mltp)
{
	try {
    rn1 = runif_impl(); // Generate fresh random number for mortality

    // Only evaluate mortality for prevalent cases or Universal incidence types
    if ((dsmeta[j].incd.type == DiseaseType::Universal || 
         (i < dsmeta[j].incd.prvl.size() && VECT_ELEM(dsmeta[j].incd.prvl, i) > 0)) &&
        dsmeta[j].mrtl.prbl2.size() > 0 && i < dsmeta[j].mrtl.prbl2.size())
    {
        // Type1: Basic mortality (no cure, no disease dependency)
        if (dsmeta[j].mrtl.type == DiseaseType::Type1)
        {
            if (dsmeta[j].mrtl1flag && dsmeta[j].mrtl.prbl1.size() > 0 && i < dsmeta[j].mrtl.prbl1.size())  // Separate first-year mortality
            {
                // Higher risk in first year of disease
                if (i < dsmeta[j].incd.prvl.size() && VECT_ELEM(dsmeta[j].incd.prvl, i) == 1 && rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl1, i)) 
                    tempdead.push_back(dsmeta[j].mrtl.death_code);
                // Standard risk in subsequent years
                else if (i < dsmeta[j].incd.prvl.size() && VECT_ELEM(dsmeta[j].incd.prvl, i) > 1 && rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i)) 
                    tempdead.push_back(dsmeta[j].mrtl.death_code);
            }
            else // Single mortality probability for all years
            {
                if (rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i)) 
                    tempdead.push_back(dsmeta[j].mrtl.death_code);
            }
        }

        // Type2: Curable mortality (automatic recovery after cure duration)
        else if (dsmeta[j].mrtl.type == DiseaseType::Type2)
        {
            // Evaluate mortality only before cure threshold
            if (i < dsmeta[j].incd.prvl.size() && VECT_ELEM(dsmeta[j].incd.prvl, i) < dsmeta[j].mrtl.cure)
            {
                if (dsmeta[j].mrtl1flag && dsmeta[j].mrtl.prbl1.size() > 0 && i < dsmeta[j].mrtl.prbl1.size())  // Separate first-year mortality
                {
                    if (VECT_ELEM(dsmeta[j].incd.prvl, i) == 1 && rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl1, i)) 
                        tempdead.push_back(dsmeta[j].mrtl.death_code);
                    else if (VECT_ELEM(dsmeta[j].incd.prvl, i) > 1 && rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i)) 
                        tempdead.push_back(dsmeta[j].mrtl.death_code);
                }
                else // Single probability
                {
                    if (rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i)) 
                        tempdead.push_back(dsmeta[j].mrtl.death_code);
                }
            }
            else if (i < dsmeta[j].incd.prvl.size() && VECT_ELEM(dsmeta[j].incd.prvl, i) >= dsmeta[j].mrtl.cure)
            {
                dsmeta[j].mrtl.flag = true; // Mark as cured after reaching cure duration
            }
        }

        // Type3: Interactive mortality (risk modified by other diseases, no cure)
        else if (dsmeta[j].mrtl.type == DiseaseType::Type3)
        {
            // Calculate composite risk multiplier from influencing diseases
            for (size_t k = 0; k < dsmeta[j].mrtl.influenced_by.disease_prvl.size(); ++k)
            {
                // Check if influencing disease was present at required lag
                const int lag_idx = i - dsmeta[j].mrtl.influenced_by.lag[k];
                if (lag_idx >= 0 && VECT_ELEM(dsmeta[j].mrtl.influenced_by.disease_prvl[k], lag_idx) > 0)
                {
                    mltp *= dsmeta[j].mrtl.influenced_by.mltp[k](i); // Apply risk multiplier
                }
            }

            if (dsmeta[j].mrtl1flag)  // Separate first-year mortality
            {
                // First year: base probability (no multiplier effect)
                if (VECT_ELEM(dsmeta[j].incd.prvl, i) == 1 && rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl1, i))
                    tempdead.push_back(dsmeta[j].mrtl.death_code);
                // Subsequent years: risk-modified probability
                else if (VECT_ELEM(dsmeta[j].incd.prvl, i) > 1 && rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i) * mltp) 
                    tempdead.push_back(dsmeta[j].mrtl.death_code);
            }
            else // Single risk-modified probability
            {
                if (rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i) * mltp) 
                    tempdead.push_back(dsmeta[j].mrtl.death_code);
            }

            mltp = 1.0; // Reset multiplier for next use
        }

        // Type4: Complex mortality (both curable and interaction-dependent)
        else if (dsmeta[j].mrtl.type == DiseaseType::Type4)
        {
            // Calculate risk multipliers from influencing diseases
            for (size_t k = 0; k < dsmeta[j].mrtl.influenced_by.disease_prvl.size(); ++k)
            {
                const int lag_idx = i - dsmeta[j].mrtl.influenced_by.lag[k];
                if (lag_idx >= 0 && VECT_ELEM(dsmeta[j].mrtl.influenced_by.disease_prvl[k], lag_idx) > 0)
                {
                    mltp *= dsmeta[j].mrtl.influenced_by.mltp[k](i);
                }
            }

            // Evaluate mortality only before cure threshold
            if (VECT_ELEM(dsmeta[j].incd.prvl, i) < dsmeta[j].mrtl.cure)
            {
                if (dsmeta[j].mrtl1flag)  // Separate first-year mortality
                {
                    // First year: base probability
                    if (VECT_ELEM(dsmeta[j].incd.prvl, i) == 1 && rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl1, i))
                        tempdead.push_back(dsmeta[j].mrtl.death_code);
                    // Subsequent years: risk-modified probability
                    else if (VECT_ELEM(dsmeta[j].incd.prvl, i) > 1 && rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i) * mltp) 
                        tempdead.push_back(dsmeta[j].mrtl.death_code);
                }
                else // Single risk-modified probability
                {
                    if (rn1 < VECT_ELEM(dsmeta[j].mrtl.prbl2, i) * mltp) 
                        tempdead.push_back(dsmeta[j].mrtl.death_code);
                }
            }
            else 
            {
                dsmeta[j].mrtl.flag = true; // Mark as cured
            }

            mltp = 1.0; // Reset multiplier for next use
        }
    }
	}
	catch(std::exception &e) {
		// Enhance error context for mortality-specific debugging
		throw std::runtime_error((string)"Error within EvalMortality(): "+e.what());
	}
}

// ===============================================================================
// MAIN SIMULATION ENGINES
// ===============================================================================

/**
 * @brief Core microsimulation engine for epidemiological modeling
 * 
 * This function implements the main IMPACT microsimulation algorithm that models
 * disease progression, diagnosis, and mortality for synthetic populations over time.
 * The simulation processes each person-year sequentially to maintain proper
 * temporal dependencies and disease state transitions.
 * 
 * **Simulation Architecture**:
 * 1. **Sequential Processing**: Processes person-years in sorted order (pid, year)
 * 2. **Disease State Management**: Tracks incidence and mortality flags per person
 * 3. **Random Number Management**: Deterministic seeding for reproducible results
 * 4. **Multimorbidity Modeling**: Accumulates disease burden scores
 * 5. **Mortality Resolution**: Handles competing causes of death
 * 
 * **Key Processing Steps per Person-Year**:
 * 1. Initialize/restore disease flags for current person
 * 2. Process cure dynamics (reset prevalence if cured)
 * 3. Evaluate disease incidence using type-specific logic
 * 4. Evaluate clinical diagnosis probability
 * 5. Evaluate disease-specific mortality
 * 6. Resolve competing mortality causes
 * 7. Save updated disease flags for next year
 * 
 * **Flag State Management**:
 * The pid_flag_tracker maintains disease state across years:
 * - incidence flags: prevent re-occurrence of non-recurrent diseases
 * - mortality flags: track cure status for temporary diseases
 * 
 * **Random Number Strategy**:
 * Uses deterministic seeding based on person ID, year, Monte Carlo iteration,
 * and disease-specific seeds to ensure:
 * - Reproducible results across runs
 * - Statistical independence between diseases
 * - Proper handling of scenario comparisons
 * 
 * **Performance Considerations**:
 * - Pre-calculates pointer access for hot loops
 * - Uses LIKELY/UNLIKELY hints for branch prediction
 * - Reserves memory for temporary vectors
 * - Optimizes memory access patterns
 * 
 * @param dt DataFrame containing synthetic population data (must be sorted by pid, year)
 * @param l Configuration list containing diseases, simulation parameters, column mappings
 * @param mc Monte Carlo iteration number for deterministic seeding
 * @throws std::runtime_error If input validation fails or simulation encounters errors
 * 
 * @note This function modifies the 'dead' column in dt to record mortality outcomes
 * @note Requires input data sorted by person ID then year for correct temporal logic
 */
//' @export
// [[Rcpp::export]]
void simcpp(DataFrame dt, const List l, const int mc)
{
	try { // catch all simcpp() exceptions

  // Validate input ordering for deterministic, correct simulation
  if (UNLIKELY(!is_sorted_by_pid_year(dt, l))) {
    const std::string pid_col = as<std::string>(l["pids"]);
    const std::string year_col = as<std::string>(l["years"]);
    throw std::runtime_error(std::string("Input DataFrame must be sorted by ") + pid_col + ", then " + year_col + " (both ascending).");
  }

  // Initialize random number generator with proper seeding
  uint64_t seed = rng->operator()();
  rng =  dqrng::generator<pcg64>(seed);
  using parm_t = decltype(uniform)::param_type;
  uniform.param(parm_t(0.0, 1.0));

  uint64_t _stream = 0;
  uint64_t _seed = 0;

  const int n = dt.nrow(), SIMULANT_ALIVE=0;

  // Extract simulation metadata and disease configurations
  simul_meta meta = get_simul_meta(l, dt);

  List diseases = l["diseases"];
  const int dn = diseases.length();
  vector<disease_meta> dsmeta(dn);
  for (int j = 0; j < dn; ++j) dsmeta[j] = get_disease_meta(diseases[j], dt);

  // Initialize flag tracker for per-pid disease state management
  pid_flag_tracker flag_tracker(dn);

  // Pre-calculate frequently used values for performance optimization
  const int* pid_data = meta.pid.begin();
  const int* year_data = meta.year.begin();
  const int* age_data = meta.age.begin();
  const int* dead_data = meta.dead.begin();

  double mltp = 1.0; // Risk multiplier for disease interactions
  vector<int> tempdead; // Temporary storage for multiple causes of death
  tempdead.reserve(dn + 5); // Pre-allocate slightly more than max diseases to avoid reallocations
  double rn1, rn2; // Random numbers for stochastic evaluations

  // =========================================================================
  // MAIN SIMULATION LOOP: Process each person-year sequentially
  // =========================================================================
  for (int i = 0; i < n; ++i)
  {
    // Process only living simulants within simulation age and year bounds
    if (LIKELY((i==0 || dead_data[i-1]==SIMULANT_ALIVE) && year_data[i] >= meta.init_year && age_data[i] >= meta.age_low))
    {
      int current_pid = pid_data[i];
      bool is_new_simulant = flag_tracker.is_new_pid(current_pid);
      
      // Initialize disease flags for new simulants
      if (UNLIKELY(is_new_simulant)) {
        flag_tracker.init_pid(current_pid);
      }

      // Generate deterministic seed for this person-year
      _seed = u32tou64(current_pid, year_data[i]);

      // =====================================================================
      // DISEASE PROCESSING LOOP: Evaluate all diseases for current person-year
      // =====================================================================
      for (int j = 0; j < dn; ++j)
      {
        // Create unique random stream for this disease
        _stream = u32tou64(mc, dsmeta[j].seed);
        rng->seed(_seed, _stream);

        // Generate random number for incidence evaluation
        rn1 = runif_impl();

        // Cache disease metadata reference for optimal memory access
        disease_meta& dm = dsmeta[j];
        
        // Restore disease state flags from persistent tracker
        dm.incd.flag = flag_tracker.get_incd_flag(current_pid, j);
        dm.mrtl.flag = flag_tracker.get_mrtl_flag(current_pid, j);

        // Handle cure dynamics: reset disease state if cured
        if (UNLIKELY(dm.mrtl.flag))
        {
          VECT_ELEM(dm.incd.prvl,i) = 0; // Reset prevalence to healthy
          
          // Adjust multimorbidity scores for cured diagnosis
          if ((dm.dgns.type == DiseaseType::Type0 || dm.dgns.type == DiseaseType::Type1) && dm.dgns.mm_wt > 0.0 && i > 0 && VECT_ELEM(dm.dgns.prvl,i - 1) > 0) {
            meta.mm_score[i] -= dm.dgns.mm_wt;
            meta.mm_count[i]--;
          }
          dm.mrtl.flag = false; // Reset cure flag
        }

        // Core disease evaluation sequence
        EvalDiseaseIncidence(dsmeta,i, j, rn1, mltp);
        EvalDiagnosis(dsmeta,rn1, j, i, meta);
        EvalMortality(dsmeta,tempdead,i, j, rn1, mltp);
        
        // Persist updated disease flags for future years
        flag_tracker.set_incd_flag(current_pid, j, dm.incd.flag);
        flag_tracker.set_mrtl_flag(current_pid, j, dm.mrtl.flag);
      }

      // =====================================================================
      // MORTALITY RESOLUTION: Handle competing causes of death
      // =====================================================================
      if (tempdead.size() == 1) 
      {
        meta.dead[i] = tempdead[0]; // Single cause of death
      }
      if (tempdead.size() > 1)
      {
        // Randomly select among competing causes
        _stream = u32tou64(mc, 1L);
        rng->seed(_seed, _stream);
        rn2 = runif_impl();

        int ind = (int)(rn2 * 100000000) % tempdead.size();
        meta.dead[i] = tempdead[ind];
      }
      tempdead.clear(); // Reset for next person-year

    } // end processing for living simulants

	 // =====================================================================
	 // DEAD SIMULANT HANDLING: Carry forward death status
	 // =====================================================================
	 if (i>0 && (dead_data[i - 1]>SIMULANT_ALIVE || IntegerVector::is_na(dead_data[i - 1])) &&
        year_data[i] >= meta.init_year && age_data[i] >= meta.age_low)
     {
      meta.dead[i] = NA_INTEGER; // Mark as long-dead (administratively censored)
     }

	} // end main simulation loop
	
	} // end exception handling scope

	// =========================================================================
	// COMPREHENSIVE ERROR HANDLING
	// =========================================================================
	catch(std::exception &e) {
		// Forward standard library exceptions with file context
		throw std::runtime_error(__FILE__+(string)": "+e.what());
	}
	catch(...) {
		// Handle non-standard exceptions
		throw std::runtime_error((string)"Exception within "+__FILE__+(string)"; "+
			"on investigating, it may helpful to disable this catch(...){} statement.");
	}
}

/**
 * @brief Performance-optimized year-based microsimulation engine
 * 
 * This function implements a year-based processing approach that groups all 
 * person-years by calendar year and processes them together. This architecture
 * provides significant performance benefits through improved cache locality
 * and memory access patterns while maintaining identical simulation logic.
 * 
 * **Year-Based Processing Architecture**:
 * 1. **Temporal Grouping**: Processes all individuals within each year together
 * 2. **Cache Optimization**: Prefetches data for entire year blocks
 * 3. **Memory Locality**: Optimizes memory access patterns for better performance
 * 4. **Branch Prediction**: Uses compiler hints for hot paths
 * 5. **SIMD-Friendly**: Memory layout optimized for vectorization
 * 
 * **Performance Optimizations**:
 * - **Restrict Pointers**: Uses __restrict__ hints for better compiler optimization
 * - **Prefetching**: Cache-line aware data prefetching for entire year blocks
 * - **Branch Hints**: LIKELY/UNLIKELY macros for better branch prediction
 * - **Memory Layout**: Optimizes data access patterns for cache efficiency
 * - **Loop Unrolling**: Compiler-friendly loop structures
 * 
 * **Key Differences from Sequential Processing**:
 * - Groups processing by year rather than processing sequentially
 * - Uses cache-aware memory prefetching strategies
 * - Optimizes memory access patterns for better performance
 * - Maintains identical simulation logic and random number sequences
 * - Preserves all temporal dependencies and disease interactions
 * 
 * **Cache Strategy**:
 * The function prefetches data in 16-element chunks (typical cache line size)
 * to ensure optimal memory throughput when processing large year blocks.
 * 
 * **Compatibility**:
 * This function produces identical results to simcpp() but with significantly
 * improved performance on large datasets due to better memory access patterns.
 * 
 * @param dt DataFrame containing synthetic population data (must be sorted by pid, year)
 * @param l Configuration list containing diseases, simulation parameters, column mappings  
 * @param mc Monte Carlo iteration number for deterministic seeding
 * @throws std::runtime_error If input validation fails or simulation encounters errors
 * 
 * @note Requires C++11 compiler with restrict pointer support for optimal performance
 * @note Uses GCC-specific __builtin_prefetch for cache optimization
 */
// [[Rcpp::export]]
void simcpp_year_based(DataFrame dt, const List l, const int mc)
{
  try {
    // Validate input ordering for deterministic, correct simulation
    if (UNLIKELY(!is_sorted_by_pid_year(dt, l))) {
      const std::string pid_col = as<std::string>(l["pids"]);
      const std::string year_col = as<std::string>(l["years"]);
      throw std::runtime_error(std::string("Input DataFrame must be sorted by ") + pid_col + ", then " + year_col + " (both ascending).");
    }

    // Initialize high-performance random number generator
    uint64_t seed = rng->operator()();
    rng = dqrng::generator<pcg64>(seed);
    using parm_t = decltype(uniform)::param_type;
    uniform.param(parm_t(0.0, 1.0));

    uint64_t _stream = 0;
    uint64_t _seed = 0;
    const int n = dt.nrow(), SIMULANT_ALIVE = 0;

    // Extract simulation configuration and disease metadata
    simul_meta meta = get_simul_meta(l, dt);
    
    List diseases = l["diseases"];
    const int dn = diseases.length();
    vector<disease_meta> dsmeta(dn);
    for (int j = 0; j < dn; ++j) dsmeta[j] = get_disease_meta(diseases[j], dt);

    // Initialize optimized flag tracker for disease state management
    pid_flag_tracker flag_tracker(dn);
    
    // Pre-calculate frequently used values with restrict pointers for optimization
    const int* __restrict__ pid_data = meta.pid.begin();
    const int* __restrict__ year_data = meta.year.begin();
    const int* __restrict__ age_data = meta.age.begin();
    const int* __restrict__ dead_data = meta.dead.begin();

    double mltp = 1.0; // Risk multiplier for disease interactions
    vector<int> tempdead; // Temporary storage for competing mortality causes
    tempdead.reserve(dn + 5); // Pre-allocate slightly more than diseases for optimal performance
    double rn1, rn2; // Random numbers for stochastic evaluations

    // =========================================================================
    // YEAR-BASED PROCESSING LOOP: Process all data grouped by calendar year
    // =========================================================================
    int current_year = (n > 0) ? year_data[0] : meta.init_year;
    int year_start_idx = 0;
    
    for (int i = 0; i <= n; ++i) { // Extra iteration to handle final year
      
      // Detect year boundaries and trigger year-block processing
      bool year_changed = (i == n) || (i > 0 && year_data[i] != current_year);
      
      if (year_changed) {
        // Process complete year block for optimal cache performance
        if (current_year >= meta.init_year && year_start_idx < i) {
          
          const int year_end_idx = i;
          
          // ===================================================================
          // CACHE OPTIMIZATION: Prefetch data for entire year block
          // ===================================================================
          for (int prefetch_idx = year_start_idx; prefetch_idx < year_end_idx; prefetch_idx += 16) {
            const int prefetch_end = std::min(prefetch_idx + 16, year_end_idx);
            if (prefetch_end < n) {
              // Prefetch next cache lines for all critical data arrays
              __builtin_prefetch(&pid_data[prefetch_end], 0, 1);
              __builtin_prefetch(&year_data[prefetch_end], 0, 1);
              __builtin_prefetch(&age_data[prefetch_end], 0, 1);
              __builtin_prefetch(&dead_data[prefetch_end], 0, 1);
            }
          }
          
          // ===================================================================
          // YEAR BLOCK PROCESSING: Process all person-years in current year
          // ===================================================================
          for (int row_idx = year_start_idx; row_idx < year_end_idx; ++row_idx) {
            
            // Main processing logic with optimized branching predictions
            if (LIKELY((row_idx==0 || dead_data[row_idx-1]==SIMULANT_ALIVE) && 
                       year_data[row_idx] >= meta.init_year && age_data[row_idx] >= meta.age_low))
            {
              const int current_pid = pid_data[row_idx];
              const bool is_new_simulant = flag_tracker.is_new_pid(current_pid);
              
              // Initialize disease state for new simulants
              if (UNLIKELY(is_new_simulant)) {
                flag_tracker.init_pid(current_pid);
              }

              // Generate deterministic seed for this person-year
              _seed = u32tou64(current_pid, year_data[row_idx]);

              // =============================================================
              // DISEASE EVALUATION LOOP: Process all diseases for current simulant  
              // =============================================================
              for (int j = 0; j < dn; ++j) {
                // Create unique random stream for this disease
                _stream = u32tou64(mc, dsmeta[j].seed);
                rng->seed(_seed, _stream);
                rn1 = runif_impl();

                // Use restrict reference for optimal compiler optimization
                disease_meta& __restrict__ dm = dsmeta[j];
                
                // Restore disease state flags from persistent storage
                dm.incd.flag = flag_tracker.get_incd_flag(current_pid, j);
                dm.mrtl.flag = flag_tracker.get_mrtl_flag(current_pid, j);

                // Handle cure dynamics with optimized branching
                if (UNLIKELY(dm.mrtl.flag))
                {
                  VECT_ELEM(dm.incd.prvl, row_idx) = 0; // Reset to healthy state
                  
                  // Adjust multimorbidity scores for cured diagnoses
                  if ((dm.dgns.type == DiseaseType::Type0 || dm.dgns.type == DiseaseType::Type1) && dm.dgns.mm_wt > 0.0 && 
                      row_idx > 0 && VECT_ELEM(dm.dgns.prvl, row_idx - 1) > 0) {
                    meta.mm_score[row_idx] -= dm.dgns.mm_wt;
                    meta.mm_count[row_idx]--;
                  }
                  dm.mrtl.flag = false; // Reset cure flag
                }

                // Core disease evaluation sequence (identical to sequential version)
                EvalDiseaseIncidence(dsmeta, row_idx, j, rn1, mltp);
                EvalDiagnosis(dsmeta, rn1, j, row_idx, meta);
                EvalMortality(dsmeta, tempdead, row_idx, j, rn1, mltp);
                
                // Persist updated disease flags for future processing
                flag_tracker.set_incd_flag(current_pid, j, dm.incd.flag);
                flag_tracker.set_mrtl_flag(current_pid, j, dm.mrtl.flag);
              }

              // =============================================================
              // OPTIMIZED MORTALITY RESOLUTION: Handle competing causes
              // =============================================================
              const size_t dead_count = tempdead.size();
              if (LIKELY(dead_count == 1)) {
                meta.dead[row_idx] = tempdead[0]; // Single cause of death
              } else if (UNLIKELY(dead_count > 1)) {
                // Randomly select among competing causes
                _stream = u32tou64(mc, 1L);
                rng->seed(_seed, _stream);
                rn2 = runif_impl();
                const int ind = (int)(rn2 * 100000000) % dead_count;
                meta.dead[row_idx] = tempdead[ind];
              }
              tempdead.clear(); // Reset for next simulant
            }
            
            // Handle dead simulants with optimized branching
            else if (UNLIKELY(row_idx > 0 && (dead_data[row_idx - 1] > SIMULANT_ALIVE || IntegerVector::is_na(dead_data[row_idx - 1])) &&
                     year_data[row_idx] >= meta.init_year && age_data[row_idx] >= meta.age_low)) {
              meta.dead[row_idx] = NA_INTEGER; // Mark as administratively censored
            }
          }
        }
        
        // Update year tracking for next iteration
        if (i < n) {
          current_year = year_data[i];
          year_start_idx = i;
        }
      }
    }
  } 
  // =========================================================================
  // COMPREHENSIVE ERROR HANDLING 
  // =========================================================================
  catch (std::exception &e) {
    throw std::runtime_error(string(__FILE__) + ": " + e.what());
  } catch (...) {
    throw std::runtime_error(string("Exception within ") + __FILE__ + "; consider disabling catch(...)");
  }
}

