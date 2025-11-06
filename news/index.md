# Changelog

## IMPACTncdJapan 0.0.1

### New Features

- Initial release of the IMPACTncd-Japan microsimulation model
- Core simulation framework with R6 classes for Design, Simulation,
  SynthPop, Disease, and Exposure
- Docker-based deployment for reproducible analysis across platforms
- Comprehensive documentation and vignettes for users and developers

### Simulation Capabilities

- Lifecourse microsimulation for Japanese adult population
- Disease modeling for CHD, stroke, diabetes, COPD, and lung cancer
- Risk factor modeling for BMI, smoking, blood pressure, cholesterol,
  and physical activity
- Scenario analysis framework for policy evaluation
- Parallel processing support for large-scale simulations

### Infrastructure

- C++17 backend for high-performance computation
- Docker containers for consistent execution environment
- GitHub Actions for automated builds and testing
- Comprehensive logging and error handling
- Flexible YAML-based configuration system

### Documentation

- Quick start guide for new users
- Detailed installation instructions
- Docker setup guide
- Scenario analysis tutorial
- Technical documentation for developers

### Known Issues

- Memory usage scales linearly with population size (~12GB per core)
- Large simulations require substantial storage space for outputs
- Some visualization features still in development

### Contributors

- Chris Kypridemos (University of Liverpool) - Lead developer
- Peter Crowther (Melandra Ltd) - Core infrastructure
- Maria Guzman-Castillo - Disease modeling
- Amandine Robert - Validation and testing
- Max Birkett - Documentation
- Piotr Bandosz - Model design
- Soshiro Ogata - Japan-specific calibration
