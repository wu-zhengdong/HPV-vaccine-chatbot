# A vaccine chatbot intervention for parents to improve HPV vaccination uptake among middle school girls: A cluster randomized trial

## Project Overview
This repository contains the code and data for a cluster randomized trial (cRCT) evaluating the effectiveness of a chatbot intervention designed for parents to improve HPV vaccination uptake among middle school girls in China.

## Study Design
- **Design**: Cluster randomized trial
- **Participants**: Parents of middle school girls aged 12-15 years in China
- **Outcomes**:
  - **Primary Outcome**:
    - HPV vaccine receipt or scheduled appointment among female students
  - **Secondary Outcomes**:
    - HPV vaccination-specific consultation
    - Parental willingness to vaccinate their daughter
    - HPV vaccine confidence
    - HPV-related literacy

## Repository Structure
- **Statistical Analysis.do**: Stata code for primary statistical analyses
- **Fig2.R**: R code for generating Figure 2 (forest plot of HPV vaccine uptake)
- **Extended Data Fig1.R**: R code for generating Extended Data Figure 1 (health professional consultation)
- **Extended Data Fig2.R**: R code for generating Extended Data Figure 2 (health literacy)
- **ICC_calculation.R**: R code for intraclass correlation coefficient calculations
- **Table3.R**: R code for generating Table 3 results
- **data/**: Directory containing the datasets used in analyses
  - `Fig2_hpv_vaccine_uptake_forestplot_data.xlsx`: Data for HPV vaccine uptake forest plot
  - `ED_fig1_healthcare_consultation_forestplot_data.xlsx`: Data for healthcare consultation forest plot
  - `ED_fig2_health_literacy_forestplot_data.xlsx`: Data for health literacy forest plot
  - `icc_cal_post_data.xlsx`: Data for ICC calculations
- **Figures/**: Directory containing generated figures in PDF and PNG formats

## Requirements
### R Dependencies
```
forestplot
forestploter
grid
dplyr
readxl
ggplot2
```

The analyses were performed using R version 4.4.1.

### Stata Dependencies
The statistical analyses were performed using Stata version 15.1.

## Contact
**Corresponding authors:**
- Dr. Leesa Lin: leesa.lin@lshtm.ac.uk
- Dr. Zhiyuan Hou: zyhou@fudan.edu.cn

Please feel free to contact the authors with any questions about the study, methodology, or code implementation.
