# IMPC Mouse Phenotype Analysis Dashboard

## Project Overview
This repository contains code for a comprehensive data analysis pipeline and visualization dashboard for mouse phenotype data from the International Mouse Phenotyping Consortium (IMPC). The system enables exploration of gene knockout effects across multiple biological systems through statistical analysis, database integration, and interactive visualization.

The IMPC is a global initiative aimed at understanding the function of every gene in the mouse genome by systematically knocking out individual genes and measuring the resulting phenotypes. This tool helps researchers identify and visualize significant phenotypic changes across various biological domains.

## Features

### Data Processing Pipeline
- Automated quality control of phenotype analysis data
- Standardized cleaning and normalization of p-values and effect sizes
- Comprehensive biological categorization of phenotypic parameters
- Missing value detection and handling
- Integration of phenotype analysis with metadata

### MySQL Database
- Normalized database schema organizing:
  - Knockout mouse genotype information
  - Phenotype parameters and measurements
  - Statistical significance scores
  - Parameter groupings by biological systems
  - Human disease associations (via Phenodigm scores)
- Efficient query capabilities for cross-referencing genotypes, phenotypes, and diseases
- Parameter groupings for weight, images, brain parameters, and other biological systems

### Interactive Dashboard
The RShiny application provides three primary visualization capabilities:

1. **Gene-Specific Analysis**
   - Select any knockout gene to visualize its phenotypic profile
   - Highlight statistically significant phenotypes
   - Filter by biological category
   - Generate detailed tables of significant effects

2. **Phenotype-Specific Analysis**
   - Examine how a selected phenotype varies across all knockout genes
   - Identify genes with significant effects on specific phenotypes
   - Sort and filter by significance level

3. **Gene Clustering Analysis**
   - Interactive heatmap showing gene-gene relationships
   - UMAP dimensionality reduction for visualizing gene clusters
   - Customizable significance thresholds and clustering parameters
   - Color-coded visualization of biological systems
   - Dendrograms showing hierarchical relationships between genes

## Repository Structure
- `data/`: Contains phenotype analysis CSV files
- `metadata/`: Contains experimental metadata and reference files
  - `IMPC_SOP.csv`: Standard operating procedures
  - `IMPC_parameter_description.txt`: Parameter descriptions
  - `IMPC_procedure.txt`: Procedure information
  - `Disease_information.txt`: PhenoDigm disease association scores
  - `query_genes.csv`: Example genotypes for querying
- `output/`: Contains processed data files
- `scripts/`: Contains all processing and analysis scripts

## Scripts
1. `perform_qc.R/sh`: Quality control processing of phenotype data
2. `merge_data.R/sh`: Merges QC-passed files into a unified dataset
3. `collate_metadata.R`: Processes metadata files into standardized formats
4. `missing_values.R`: Detects and handles missing/inconsistent values
5. `data_cleaning.R`: Standardizes fields in the merged phenotype data
6. `database_script.sql`: Creates and populates the MySQL database structure
7. `app.R`: RShiny application for interactive data visualization

## Database Schema
The MySQL database organizes data across several tables:
- `genotypes`: Information about knockout genes
- `parameters`: Phenotype parameters and their metadata
- `parameter_groups`: Biological groupings of parameters
- `procedures`: Experimental procedures used for phenotyping
- `results`: Statistical analysis results (p-values, effect sizes)
- `disease_associations`: Human disease-to-mouse phenotype associations

## Visualization Features
The dashboard provides multiple ways to explore the IMPC data:
- **Filtering**: By biological category, significance level, and gene sets
- **Heatmaps**: Gene-gene correlation matrices with hierarchical clustering
- **UMAP Plots**: Dimensionality reduction showing gene clusters
- **Significance Visualization**: Color-coding by statistical significance
- **Interactive Elements**: Hover details, custom gene selection, p-value thresholds

## Usage
1. Process data through the pipeline scripts
2. Import processed data into MySQL using the database script
3. Launch the RShiny app to explore the data interactively

## Dependencies
- R libraries: shiny, shinydashboard, data.table, dplyr, ggplot2, plotly, umap, ComplexHeatmap, circlize, RColorBrewer, viridis, dbscan, reshape2
- MySQL server for database functionality

## Data Sources
All data is derived from the International Mouse Phenotyping Consortium (IMPC) phenotyping pipeline, which performs standardized phenotypic assays on knockout mouse lines.

---

**Note**: This tool is designed to help researchers identify genes with significant phenotypic effects and potential relationships to human disease, facilitating the discovery of gene function and potential therapeutic targets.
