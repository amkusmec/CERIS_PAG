[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

# Data, Code and Workflows Guideline

1. __CERIS_EP.r__ and __Sub_functions_bio.r__ are two R scripts for conducting Critical Environmental Regressor through Informed Search (CERIS) and Environic Prediction (EP).
2. __1Sorghum__: The demo data for a sorghum multi-environment trial, which represents the scenerio of the number of tested indivdiuals is large.
3. __2Idaho__: The demo data for a wheat multi-environment tria, which represents the scenerio of the number of environments is large. 
4. __README__: Follow the demo example to format the necessary input files and run the __CERIS_EP.r__. The output file will be stored under __1Sorhgum__ or __2Idaho__. The provided __output__ folder under each demo contains the expected output from analysess.

## Installation

- __Running environment__: 
    - The workflow was constructed based on the __Windows 10__, and should be supported in __iOS__ and __Linux__ as long as R is supported.

- __Required software and versions__: 
    - [R 4.4.1](https://cran.r-project.org/) or [Rstudio](https://www.rstudio.com/)
        - [rrBLUP](https://cran.r-project.org/web/packages/rrBLUP/index.html) and [colorspace](https://cran.r-project.org/web/packages/colorspace/index.html)

## Input Data

- Traits record: `1Sorghum/Traits_record.txt` or `2Idaho/Traits_record.txt`
- Cultural information: `1Sorghum/Env_meta_table.txt` or `2Idaho/Env_meta_table.txt`
- daily weather profile for each environment under `1Sorghum/dailyEnv/` or `2Idaho/dailyEnv/`
- SNP file: `1Sorghum/Genotype.txt/`

## Folder structure (1Sorghum as example)
---CWR
   |
   1Sorghum
   |  |
   |--dailyEnv
   |   |---IA13_daily.txt
   |   |---IA14_daily.txt
   |--FTgdd_output
   |   |--oupput_1.txt
   |   |--oupput_2.txt
   |   |--oupput_1.png
   |--Env_meta_table.txt
   |--Genotype.txt
   |--Traits_record.txt
   2Idaho
   

## Major steps

The __CERIS_EP.r__ has 6 blocks of codes, modify and run each block according to the protocol. 

## License
It is a free and open source software, licensed under []() (choose a license from the suggested list:  [GPLv3](https://github.com/github/choosealicense.com/blob/gh-pages/_licenses/gpl-3.0.txt), [MIT](https://github.com/github/choosealicense.com/blob/gh-pages/LICENSE.md), or [CC BY 4.0](https://github.com/github/choosealicense.com/blob/gh-pages/_licenses/cc-by-4.0.txt)).
