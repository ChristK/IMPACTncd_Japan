# Graph Report - .  (2026-05-28)

## Corpus Check
- 68 files · ~135,928 words
- Verdict: corpus is large enough that graph structure adds value.

## Summary
- 299 nodes · 404 edges · 33 communities (24 shown, 9 thin omitted)
- Extraction: 90% EXTRACTED · 10% INFERRED · 0% AMBIGUOUS · INFERRED: 39 edges (avg confidence: 0.81)
- Token cost: 0 input · 0 output

## Community Hubs (Navigation)
- [[_COMMUNITY_Disease Cost & Calibration Data|Disease Cost & Calibration Data]]
- [[_COMMUNITY_C++ Sim Engine (current)|C++ Sim Engine (current)]]
- [[_COMMUNITY_R6 Simulation Architecture|R6 Simulation Architecture]]
- [[_COMMUNITY_Docker Image Build|Docker Image Build]]
- [[_COMMUNITY_C++ Sim Engine (reference)|C++ Sim Engine (reference)]]
- [[_COMMUNITY_Beta Distribution Fitter|Beta Distribution Fitter]]
- [[_COMMUNITY_Simulation Config & Scenarios|Simulation Config & Scenarios]]
- [[_COMMUNITY_disease_epi struct (reference)|disease_epi struct (reference)]]
- [[_COMMUNITY_disease_epi struct (current)|disease_epi struct (current)]]
- [[_COMMUNITY_simul_meta struct (reference)|simul_meta struct (reference)]]
- [[_COMMUNITY_simul_meta struct (current)|simul_meta struct (current)]]
- [[_COMMUNITY_disbayes Prevalence Pipeline|disbayes Prevalence Pipeline]]
- [[_COMMUNITY_CICD Docker Push|CI/CD Docker Push]]
- [[_COMMUNITY_APT Packages Updater (Bash)|APT Packages Updater (Bash)]]
- [[_COMMUNITY_Memory Profiler|Memory Profiler]]
- [[_COMMUNITY_C++ Engine Test Harness|C++ Engine Test Harness]]
- [[_COMMUNITY_Disease Influence Modifiers|Disease Influence Modifiers]]
- [[_COMMUNITY_Docker BuildPush (Bash)|Docker Build/Push (Bash)]]
- [[_COMMUNITY_User Docker Env Setup (Bash)|User Docker Env Setup (Bash)]]
- [[_COMMUNITY_Container Entrypoint|Container Entrypoint]]
- [[_COMMUNITY_R Package Installer|R Package Installer]]
- [[_COMMUNITY_Dev Docker Env Setup (Bash)|Dev Docker Env Setup (Bash)]]
- [[_COMMUNITY_Population Data READMEs|Population Data READMEs]]
- [[_COMMUNITY_undef_fortify Header Note|undef_fortify Header Note]]
- [[_COMMUNITY_Output Comparison Helper|Output Comparison Helper]]

## God Nodes (most connected - your core abstractions)
1. `simcpp()` - 16 edges
2. `simcpp_year_based()` - 16 edges
3. `disease_epi` - 12 edges
4. `disease_epi` - 12 edges
5. `pid_flag_tracker` - 11 edges
6. `Simulation R6 class` - 11 edges
7. `simul_meta` - 9 edges
8. `simul_meta` - 9 edges
9. `simcpp_ref()` - 8 edges
10. `Docker Setup README` - 8 edges

## Surprising Connections (you probably didn't know these)
- `GitHub Actions: Test simulation.R workflow` --references--> `global.R bootstrap / library setup`  [EXTRACTED]
  .github/workflows/test_sim.yml → global.R
- `export_summaries() pipeline` --semantically_similar_to--> `auxil/process_out.R post-processing`  [INFERRED] [semantically similar]
  Rpackage/IMPACTncd_Japan_model_pkg/R/Simulation_class.R → simulate.R
- `IMPACTncd_sim_first_paper.cpp (simcpp_ref benchmark)` --semantically_similar_to--> `IMPACTncd_sim.cpp (simcpp under test)`  [INFERRED] [semantically similar]
  auxil/cpp_testing/scripts/IMPACTncd_sim_first_paper.cpp → Rpackage/IMPACTncd_Japan_model_pkg/src/IMPACTncd_sim.cpp
- `Diseases: obesity, htn, t2dm, chd, stroke, cvd, nonmodelled` --shares_data_with--> `2_disbayes.R MCMC fit`  [INFERRED]
  inputs/sim_design.yaml → inputs/disease_burden/disbayes_corrected_prvl/2_disbayes.R
- `Docker-based user workflow` --references--> `sim_design YAML configuration`  [EXTRACTED]
  README.md → Rpackage/IMPACTncd_Japan_model_pkg/R/Design_class.R

## Hyperedges (group relationships)
- **Simulation initialisation flow** — simulation_class_r6, design_class_r6, exposure_class_r6, disease_class_r6, concept_causality_structure [EXTRACTED 0.90]
- **Top-level entry-script pipeline (run/export/validate)** — simulate_r_script, calibrate_r_script, validate_r_script, global_r_bootstrap, simulation_class_r6 [EXTRACTED 0.85]
- **Docker CI build pipeline (prerequisite -> main image)** — build_push_prerequisite_workflow, build_push_impactncdjpn_workflow, concept_docker_prerequisite_image, concept_docker_main_image [EXTRACTED 0.90]
- **HTN treatment initiation scenarios (10/20/30/40/50pcnt) plus base case** — htn_treatment_initiation_scenario, sc0_baseline_scenario, simulate_httrt_initiation_script, table_sbp_fst [EXTRACTED 0.95]
- **C++ engine regression testing harness** — quick_simcpp_test_script, impactncd_sim_cpp, impactncd_sim_first_paper_cpp, cpp_testing_readme [EXTRACTED 0.95]
- **Docker provisioning toolchain (build, install pkgs, dev/user envs)** — docker_build_push_ps1, install_packages_sh, setup_dev_docker_env_sh, setup_dev_docker_env_ps1, setup_user_docker_env_ps1, dockerfile_prerequisite [EXTRACTED 0.90]
- **Docker user-run workflow** — setup_user_docker_env_script, docker_entrypoint_script, sim_design_yaml [EXTRACTED 0.95]
- **disbayes corrected-prevalence pipeline** — disbayes_1_set, disbayes_2_run, disbayes_5_fstfiles [EXTRACTED 0.95]
- **sim_design YAML family** — sim_design_yaml, sim_design_clbr_yaml, sim_design_testing_yaml [INFERRED 0.85]

## Communities (33 total, 9 thin omitted)

### Community 0 - "Disease Cost & Calibration Data"
Cohesion: 0.07
Nodes (26): absorb_dt() lookup join, arrow::open_dataset, log-log calibration of incidence, memedian(), inputs/disease_burden/chd_ftlt.fst, inputs/disease_burden/chd_incd.fst, inputs/disease_burden/chd_prvl.fst, Design R6 class (+18 more)

### Community 1 - "C++ Sim Engine (current)"
Cohesion: 0.12
Nodes (23): disease_meta, dgns, incd, mrtl, mrtl1flag, seed, DiseaseIncidenceType2(), DiseaseIncidenceType3() (+15 more)

### Community 2 - "R6 Simulation Architecture"
Cohesion: 0.12
Nodes (30): calibrate.R (entry script), private$calc_costs() DuckDB cost view, calibrate_incd_ftlt() calibration method, Disease causality graph (igraph DAG), CKutils dependency package, export_summaries() pipeline, primary prevention scenario hook, auxil/process_out.R post-processing (+22 more)

### Community 3 - "Docker Image Build"
Cohesion: 0.09
Nodes (11): CRAN snapshot 2025-09-30, chriskypri/impactncdjpn Docker image, Dockerfile.IMPACTncdJPN, Dockerfile.prerequisite.IMPACTncdJPN, gosu non-root user switching, Posit Package Manager 2025-07-20 snapshot, rocker/r-ver:4.5.1 base image, Docker volume vs bind-mount modes (+3 more)

### Community 4 - "C++ Sim Engine (reference)"
Cohesion: 0.13
Nodes (21): disease_meta, dgns, incd, mrtl, mrtl1flag, seed, DiseaseIncidenceType2(), DiseaseIncidenceType3() (+13 more)

### Community 5 - "Beta Distribution Fitter"
Cohesion: 0.22
Nodes (15): betacf(), check_quantile_monotonicity(), fit_beta_cpp(), fit_beta_pure_cpp(), fit_beta_vec_cpp(), lbeta(), nelder_mead_2d(), objective() (+7 more)

### Community 6 - "Simulation Config & Scenarios"
Cohesion: 0.18
Nodes (11): carry_forward_incr helper, Custom column suffix convention (_prvl/_contd/_costs), Diseases: obesity, htn, t2dm, chd, stroke, cvd, nonmodelled, Exposures (Fruit_vege, Smoking, BMI, HbA1c, LDLc, SBP, PA_days, Med_HT/HL/DM), GAMLSS exposure models, global.R bootstrap, NHNS (Japan National Health and Nutrition Survey), Simulation R6 class entrypoint (+3 more)

### Community 7 - "disease_epi struct (reference)"
Cohesion: 0.17
Nodes (12): disease_epi, aggregate, can_recur, cure, death_code, flag, influenced_by, mm_wt (+4 more)

### Community 8 - "disease_epi struct (current)"
Cohesion: 0.17
Nodes (12): disease_epi, aggregate, can_recur, cure, death_code, flag, influenced_by, mm_wt (+4 more)

### Community 9 - "simul_meta struct (reference)"
Cohesion: 0.22
Nodes (9): simul_meta, age, age_low, dead, init_year, mm_count, mm_score, pid (+1 more)

### Community 10 - "simul_meta struct (current)"
Cohesion: 0.22
Nodes (9): simul_meta, age, age_low, dead, init_year, mm_count, mm_score, pid (+1 more)

### Community 11 - "disbayes Prevalence Pipeline"
Cohesion: 0.25
Nodes (5): disbayes R package (Bayesian disease prevalence), Global Burden of Disease (GBD), 1_set.R disbayes setup, 2_disbayes.R MCMC fit, 6_vsgbd.R disbayes vs GBD comparison

### Community 12 - "CI/CD Docker Push"
Cohesion: 0.29
Nodes (8): GitHub Actions: Build Push IMPACTncdJPN, GitHub Actions: Build Push prerequisite.IMPACTncdJPN, impactncdjpn Docker image, prerequisite.impactncdjpn Docker image, sim_design YAML configuration, Developer Docker setup workflow, Docker-based user workflow, IMPACT-NCD Japan microsimulation (README)

### Community 13 - "APT Packages Updater (Bash)"
Cohesion: 0.53
Nodes (4): main(), update_apt_packages_file(), usage(), update-apt-packages.sh script

### Community 15 - "C++ Engine Test Harness"
Cohesion: 0.50
Nodes (4): cpp_testing README, IMPACTncd_sim.cpp (simcpp under test), IMPACTncd_sim_first_paper.cpp (simcpp_ref benchmark), simcpp() C++ engine

### Community 18 - "Disease Influence Modifiers"
Cohesion: 0.50
Nodes (4): infl, disease_prvl, lag, mltp

## Knowledge Gaps
- **94 isolated node(s):** `disease_prvl`, `mltp`, `lag`, `type`, `prvl` (+89 more)
  These have ≤1 connection - possible missing edges or undocumented components.
- **9 thin communities (<3 nodes) omitted from report** — run `graphify query` to explore isolated nodes.

## Suggested Questions
_Questions this graph is uniquely positioned to answer:_

- **Why does `export_summaries() pipeline` connect `R6 Simulation Architecture` to `Simulation Config & Scenarios`?**
  _High betweenness centrality (0.053) - this node is a cross-community bridge._
- **What connects `disease_prvl`, `mltp`, `lag` to the rest of the system?**
  _94 weakly-connected nodes found - possible documentation gaps or missing edges._
- **Should `Disease Cost & Calibration Data` be split into smaller, more focused modules?**
  _Cohesion score 0.06868686868686869 - nodes in this community are weakly interconnected._
- **Should `C++ Sim Engine (current)` be split into smaller, more focused modules?**
  _Cohesion score 0.125 - nodes in this community are weakly interconnected._
- **Should `R6 Simulation Architecture` be split into smaller, more focused modules?**
  _Cohesion score 0.11724137931034483 - nodes in this community are weakly interconnected._
- **Should `Docker Image Build` be split into smaller, more focused modules?**
  _Cohesion score 0.09420289855072464 - nodes in this community are weakly interconnected._
- **Should `C++ Sim Engine (reference)` be split into smaller, more focused modules?**
  _Cohesion score 0.12648221343873517 - nodes in this community are weakly interconnected._