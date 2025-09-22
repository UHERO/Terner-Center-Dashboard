# Housing Development Model Dashboard for Oahu

## Project Overview
This project implements a housing development feasibility and policy impact model specifically for Oahu, inspired by the Terner Center Housing Dashboard. The pro forma modeling serves as the backbone of the dashboard by calculating the financial feasibility and expected value of development projects, which then feed into the interactive visualizations and policy simulations.

## Currently Frame of the Project
The pro forma modeling (financial/expeceted project value) is the current focus of the R project. Then, feasibility analysis and scenario testing can soon follow, allowing us to change the model to be geared more towards user-inputs for the housing dashboard part.

## Project Structure
Housing.Rproj
- data # Raw and processed data files
- src # R scripts for data processing & modeling

## Next Steps
- Update/find the global variables specific to Oahu  
- Undo the hardcoded constants (e.g., affordable units, for sale/for rent)  
- Work on the financial modeling for completed project value in the pro forma part  
- Turn the model into a user input interactive dashboard  
- Think of statistical or rule-based models to estimate probabilities of development 

## Resources
- Path to RData on the NAS: `Z:\work\research\housing\zoning\cch-zoning\RData`  
- [Global variables spreadsheet](https://docs.google.com/spreadsheets/d/1_sSlavakP_3b8Ssv547RV-63rOuHqrqrIfim9JED3XU/edit?usp=sharing)  
- [Honolulu Inclusionary Housing Flowchart](https://miro.com/app/board/uXjVJaPZAJQ=/)  

## Notes 
- Filepathing is unique to Sakura, change when needed 

## Usage 
- Main branch: fully working/developed code
- Develop: different workspace for code still in progress
- Features: branches in develop for specific tasks
