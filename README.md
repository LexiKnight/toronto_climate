# 🌍 Climate Change Perspectives


## ✏️ Overview
This project explores the perspectives of Toronto residents on climate change and the likelihood of individuals taking climate-friendly actions. The analysis primarily relies on survey data from 2018, supplemented by summary data from 2021 to provide a more current overview of the public's engagement with climate-related behaviors.

The focus of this project is to understand the extent to which different demographic factors—specifically age and education attainment impact individuals' likelihood to engage in climate-friendly actions. These actions include reducing energy usage at home, minimizing car use, using electric or hybrid vehicles, adopting meat alternatives, reducing waste, purchasing green products, walking or cycling short distances, and sorting waste properly.

By comparing the 2018 and 2021 datasets, the goal is to identify trends and changes in climate action engagement over time, particularly as it relates to demographics. A decision tree model is used to examine the relationship between age, education level, and the likelihood of taking climate-friendly actions, with the hypothesis that younger and more highly educated individuals are more likely to take these actions.


## 🤔 Key Insights
-   **Climate-Friendly Actions**: The project identifies which climate-friendly actions Toronto residents are most likely to engage in, ranging from home improvements to waste sorting and reducing car use.
-    **Demographic Influence**: The analysis investigates how age and education influence the likelihood of individuals taking climate-friendly actions, with an expected outcome that younger and more educated individuals are more likely to participate.
-    **Comparison of 2018 and 2021 Data**:  By comparing 2018 survey data with 2021 summary data, the project offers a more updated picture of climate action trends, accounting for any shifts in public perspectives or behaviors over the three-year period.
-    **Communication Strategies**: The project also explores the most effective methods for communicating climate change information to the public, including examining the self-reported extent to which individuals feel informed about climate change and climate action.


## 📁 File Structure

The repo is structured as:

-   `data/00-simulated_data`: Contains the simulated data based on climate change perspectives data obtained from OpenDataToronto.
-   `data/01-raw_data`: Contains the original datasets from OpenDataToronto for 2018 and 2021.
-   `data/02-analysis_data`: Contains the cleaned and processed datasets used for analysis.
-   `data/03-figures_data`: Contains the summary figures for 2018 and 2021.
-   `models`: Contains model figures and summary table for 2018.
-   `other`: Contains relevant literature, details about interactions with LLMs (Large Language Models), and sketches.
-   `paper`: Contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts`: Contains the R scripts used to install packages as well as simulate, download, clean data, test, explore and model the data. 


## 🖊️ Statement on LLM
Aspects of the code and some debugging were written with the help of ChatGpt 4o. Entire chat history is available in `other/llms/usage.txt`.

## 🔍 How to Use This Repo
1. Install the required packages: Ensure you have the necessary R packages installed and loaded. See `scripts/07-install_packages.R` for list of all packages used and the reasoning. 
2. Load the data: The datasets are available in `data/01-raw_data` and `data/02-analysis_data`. You can use the R scripts in `scripts` to simulate, download, clean, test and model the data. 
3. Run the model: The `model` folder contains the decision tree model used to examine the relationship between age, education and climate-friendly actions. 

## ⭐ Future Work and Improvements
-    **Expanding Data**: Additional years of survey data or other sources of demographic data could further enrich the analysis and provide more detailed insights into climate action trends over time.

-    **Exploring Additional Variables**: Beyond age and education, it would be valuable to explore other factors, such as income or geographic location, to better understand climate action behaviors across different segments of the population.



