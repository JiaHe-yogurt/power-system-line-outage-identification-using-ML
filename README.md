# ML application on power system line outage identification
This is a R implementation of supervised machine learning algorithms to address power system line outage identification. For detailed techniques, please see our paper,

[Jia He, Maggie Cheng, Yixin Fang, Mariesa L. Crow, A Machine Learning Approach for Line Outage Identification in Power Systems] (https://www.researchgate.net/publication/331080783_A_Machine_Learning_Approach_for_Line_Outage_Identification_in_Power_Systems_4th_International_Conference_LOD_2018_Volterra_Italy_September_13-16_2018_Revised_Selected_Papers)

## Requirements
- Matlab toolbox PSAT
- R studio

## Data
To generate data, run Time Domain Simulation in PSAT for different power networks. The file--data simulation, contains matlab codes to simultate 39 and 118 power system single line outage data, with dynmic loads during simulation. Multiple line outage simulation can be achieved with a minor change of codes.

## Model
We used Logistic Regression and Random Forest for the task of line outage identification, which outperforms other supervised algorithms.

## Cite
Please cite our paper if you use this code in your own work:

@inproceedings{he2018machine,
title={A Machine Learning Approach for Line Outage Identification in Power Systems},
author={He, Jia and Cheng, Maggie X and Fang, Yixin and Crow, Mariesa L},
booktitle={International Conference on Machine Learning, Optimization, and Data Science},
pages={482--493},
year={2018},
organization={Springer}
}
