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
- [UHERO Google Drive Folder](https://drive.google.com/drive/folders/1hgTBZ4-xmxrQN5XRX8QaS8DMSH_GULah?usp=sharing)
- [Pro Forma Variables Spreadsheet](https://docs.google.com/spreadsheets/d/1_sSlavakP_3b8Ssv547RV-63rOuHqrqrIfim9JED3XU/edit?usp=sharing)  
- [Honolulu Inclusionary Housing Flowchart](https://miro.com/app/board/uXjVJaPZAJQ=/)
- [IPDT Flowchart](https://miro.com/app/live-embed/uXjVJDi57jk=/?embedMode=view_only_without_ui&moveToViewport=-2298%2C-1870%2C3822%2C2086&embedId=247174178665)
- [Parcels & Zoning Dashboard](https://www.honolulugis.org/apps/cchnl::parcels-zoning-information-app-/explore?path=)

## Notes 
- Filepathing is unique to Sakura, change when needed 

## Usage 
- Main branch: fully working/developed code
- Develop: different workspace for code still in progress
- Features: branches in develop for specific tasks 

## Data 
- TOD_Interim_Boundary: TOD plan boundary shp files for the proposed TOD zones not implemented currently (ex. Kalihi, Ala moana)
- [TOD_Adopted_Boundary](https://honolulu-cchnl.opendata.arcgis.com/datasets/cchnl::tod-special-district-and-zone-change/explore?layer=3&location=21.359764%2C-157.982790%2C12.99): Already adopted TOD zones (ex. Aloha Stadium, Waipahu)  shp files (includes: height & zone changes) 
- [ASMTGIS_Table](https://honolulu-cchnl.opendata.arcgis.com/datasets/11a667cc8ab0434c9c40c8566dc0d375_9/explore?filters=eyJ0bWsiOlsiOTQxMTUwMzgiXX0%3D): BFS Real Property Assesment Table for all islands
- All_Zoning: All zoning codes for Oahu (ex. includes B1-3, BMX1-3), for later impementation if we want to include other residential/apartment units into the pro forma
- [Special_Districts_Zoning](https://honolulu-cchnl.opendata.arcgis.com/datasets/6e1bbf5e381847ad93fd76dbeecfdd50_1/explore?filters=eyJzcGVjaWFsX2Rpc3RyaWN0IjpbIldhaWtpa2kgU3BlY2lhbCBEaXN0cmljdCBhbmQgRGlhbW9uZCBIZWFkIFNwZWNpYWwgRGlzdHJpY3QiLCJXYWlraWtpIFNwZWNpYWwgRGlzdHJpY3QiLCJQdW5jaGJvd2wgU3BlY2lhbCBEaXN0cmljdCIsIktpbmcgS2FtZWhhbWVoYSBJSUkgYXQgVGhvbWFzIFNxdWFyZS9Ib25vbHVsdSBNdXNldW0gb2YgQXJ0IFNwZWNpYWwgRGlzdHJpY3QiLCJLYWthYWtvIFNwZWNpYWwgRGVzaWduIERpc3RyaWN0IiwiSGFsZWl3YSBTcGVjaWFsIERpc3RyaWN0IiwiRGlhbW9uZCBIZWFkIFNwZWNpYWwgRGlzdHJpY3QiLCJDaGluYXRvd24gU3BlY2lhbCBEaXN0cmljdCIsIkhhd2FpaSBDYXBpdGFsIFNwZWNpYWwgRGlzdHJpY3QiLCJUcmFuc2l0LU9yaWVudGVkIERldmVsb3BtZW50IFNwZWNpYWwgRGlzdHJpY3QiXX0%3D&location=21.288384%2C-157.836843%2C13.57): Special district zones for zoning (ex. Waikiki Special District) (Also includes already implemented TOD zones (ex. Waipahu), however is not an accurate shape compared to TOD_Adopted_Boundary)
