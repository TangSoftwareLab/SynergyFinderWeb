---
title: "USER GUIDE"
date: 2021.4.7
output: html_document
css: "../styles/userGuideStyle.css"
---

# UPLOAD DATA

## 1 Data Format

SynergyFinder accepts two data formats: **Table** and **Matrix**.

### 1.1 Table format

In the Table format, the input dose-response matrix is represented as a table where each row contains the information about the one cell in the dose-response matrix (Fig.1).

The input table must contain the following columns:

1. **block_id** Index of drug-pair to be analyzed
2. **drug1** Name of the first tested drug
3. **drug2** Name of the second tested drug
4. **conc1** Concentration of drug1
5. **conc2** Concentration of drug2
6. **response** Measurement of the cell line response to the drug treatment. It could be \%inhibition or \%viability
7. **conc\_unit** Unit of concentration for drugs. Alternatively, separated columns: **conc\_unit1**, **conc\_unit2**, ..., **conc\_unit[n]** for each tested drugs. It is helpful while different unit was used for measuring the concentrations.
8. **drug[n]** (Option) Name of the n_th_ tested drug. For example, "drug3" for the third tested drug.
9. **conc[n]** (Option) Concentration of n_th_ tested drug. For example, "conc3" for the third tested drug.
10. **cell\_line\_name** (Option) Name of the cell line. The cell line names will be used for "data annotation". It will not influence the drug treatment effect analysis.


The new SynergyFinder accepts the column naming style used in the old SynergyFinder or [DrugComb](https://drugcomb.org/). Following lists the available column naming methods:


|Recommend.Column.Names |Alternative.Column.Names         |
|:----------------------|:--------------------------------|
|block_id               |PairIndex, BlockId               |
|drug1                  |Drug1, drug_row, DrugRow         |
|drug2                  |Drug2, drug_col, DrugCol         |
|conc1                  |Conc1, conc_row, ConcRow         |
|conc2                  |Conc2, conc_col, ConcCol         |
|response               |Response, inhibition, Inhibition |
|conc_unit              |ConcUnit                         |
|conc_unit1             |conc_r_unit                      |
|conc_unit2             |conc_c_unit                      |

| Recommend Column Names | Alternative Column names         |
| ---------------------- | -------------------------------- |
| block\_id              | PairIndex, BlockId               |
| drug1                  | Drug1, drug\_row, DrugRow        |
| drug2                  | Drug2, drug\_col, DrugCol        |
| conc1                  | Conc1, conc\_row, ConcRow        |
| conc2                  | Conc2, conc\_col, ConcCol        |
| response               | Response, inhibition, Inhibition |
| conc\_unit             | ConcUnit                         |
| conc\_unit1            | conc\_r\_unit                    |
| conc\_unit2            | conc\_c\_unit                    |


The orders of rows and columns are arbitrary. The number of drug combinations (`block_id`) provided in the input
file is unlimited. The only restriction is imposed on the size of a dose-response matrix, which should comprise **at least
three rows and columns**, so that sensible synergy scores can be calculated.

![Fig.1 Input file in Table format.](../www/images/exampleTab.png)

<p style="text-align: center;">Fig.1 Input file in Table format.</p>

### 1.2 Matrix format

In the Matrix format, the dose-response matrix is represented in a matrix form with drug concentrations shown along the top and left side of the matrix (Fig.2).

The three rows should precede each dose-response matrix:

1. **Drug1** name of the first drug
2. **Drug2** name of the second drug
3. **ConcUnit** unit of concentration

The drug concentrations should be located at the top and left side of the matrix, where left side concentrations correspond to Drug1 and concentrations shown at the top correspond to the Drug2. The number of drug combinations provided in the input file is unlimited. The only restriction is imposed on the dose-response matrix size, which should comprise at least three rows and columns, so that sensible synergy scores can be calculated.

![Fig.2 An input file in Matrix format. Two dose-response matrices (for AT-406 & Navitoclax and NVP-LGK974 &
Alpelisib drug combinations) are provided as an example.](../www/images/exampleMat.png)

<p style="text-align: center;">Fig.2 An input file in Matrix format. Two dose-response matrices are provided as an example.</p>

## 2 Load file

Using the "Browse" button, users could select a file from local directory for uploading. The expected file formats are:

* EXCLE file with extention ".xlsx"
* comma-delimited CSV file, with extention ".csv"
* comma-delimited TXT file, with extention ".txt"

Example data is available from <a href="ExampleData.zip" download>here</a>.

## 3 Choose readout

After uploading the input file the user needs to specify the type of **phenotypic response**. Available options in SynergyFinder are:
* **inhibition** % inhibition to cell growth or other signals. Normalized by negative control. 
* **viabillity** % cell viability after treatment. Normalized by negative control. In case of %viability response type, the provided %viability values will be converted to %inhibition by the formula: $\%inhibition = 100 - \%viability$.

After specifying the type of phenotypic response the user may firstly obtain an overview of the provided data in a searchable table to check for the problematic data points.

By switching the "Visualize dose response data" toggle button, users will be directed to nex tab "Dose Response Map" for dose-response dat visualization.

# Dose Response Map

This tab will show an overview of the full dose-response matrix, as well as the dose response (%inhibition) curve for the single drugs (i.e. the first column and first row in the dose-response matrix), fitted by four-parameter logistic curves. The tabs named by combination of drug names are design for navigating to maps for different combinations. (Fig.3)

![Fig.3 Visualization of input dose-response data](../www/images/visDos.png)

<p style="text-align: center;">Fig.3 Visualization of input dose-response data</p>

The control panel at the left side of the page enables user to change the height and width of the plot.

SynergyFinder allows for missing values in the dose-response matrix. They will be colored in gray. (Fig.4A) They will be imputed by taking the average of the nearest responses of both drugs. For example, the missing value from the response matrix in Fig.4A will be estimated as: $( 73.04 + 74.8 + 52.1 + 45.25) / 4 = 61.3$

User could also specify the outliers (Fig.4C) by using the ‘Estimate outliers’ widget. In this case the cell specified by users will be imputed by using the same method as that for missing value.

![Fig.4 A. Dose-response matrix with a missing value; B. Dose-response matrix with an outlier; C. SynergyFinder imputation of outliers.](../www/images/imputation.png)

<p style="text-align: center;">Fig.4 A. Dose-response matrix with a missing value; B. Dose-response matrix with an outlier;<br/>C. SynergyFinder imputation of outliers.</p>

After adjusting the dose response matrix, user could switch the "Calculate synergy" toggle button to navigate to next tab "Synergy Map".

# Synergy Map

User could set two parameters for synergy score calculation:

1. **Correct baseline**: SynergyFinder could adjust the base line of response matrix to make it closer to 0. There are 3 options available:
 
 * _non_ do notcorrect base line
 * _part_ correct base line but only adjust negative responsevalues in matrix
 * _all_ correct base line with adjusting all values in matrix.
 
2. **Method**: Select the reference model by means of which the expected synergy score will be calculated. 

The synergistic effect can be determined as the excess of observed effect over expected effect calculated with
reference models (synergy scoring models). All of the models make different assumptions regarding the expected effect. Currently, 4 reference models are available in SynergyFinder. 

* **Highest Single Agent (HSA)** [Berenbaum, 1989] states that the expected combination effect equals to the higher effect of individual drugs:

$$y_{HSA} = max(y_1, y_2)$$

* **Loewe** additivity model [Loewe, 1953] defines the expected effect $y_{LOEWE}$ as if a drug was combined with itself. Unlike the HSA and the Bliss independence models giving a point estimate using different assumptions, the Loewe additivity model considers the dose-response curves of individual drugs. The expected effect $y_{LOEWE}$ must satisfy:

$$
		\frac
		{x_1}{\chi_{LOEWE}^1}
		+
		\frac{x_2}{\chi_{LOEWE}^2} 
		= 1
$$

, where $x_{1,2}$ are drug doses and $\chi_{LOEWE}^1,\ \chi_{LOEWE}^2$ are the doses of drug 1 and 2 alone that produce $y_{LOEWE}$.
Using 4-parameter log-logistic (4PL) curves to describe dose-response curves the following parametric form of previous equation is derived:

$$
		\frac
		{x_1}{m_1(\frac{y_{LOEWE}-E_{min}^1}{E_{max}^1 - y_{LOEWE}})^{\frac{1}{\lambda_1}}}
		+
		\frac{x_2}{m_2(\frac{y_{LOEWE}-E_{min}^2}{E_{max}^2 - y_{LOEWE}})^{\frac{1}{\lambda_2}}}
		= 1
$$
, where $E_{min}, E_{max}\in[0,1]$ are minimal and maximal effects of the drug, $m_{1,2}$ are the doses of drugs that produce the midpoint effect of $E_{min} + E_{max}$, also known as relative $EC_{50}$ or $IC_{50}$, and $\lambda_{1,2}(\lambda>0)$ are the shape parameters indicating the sigmoidicity or slope of dose-response curves. A numerical nonlinear solver can be then used to determine $y_{LOEWE}$ for ($x_1$, $x_2$).

* **Bliss** model [Bliss, 1939] assumes a stochastic process in which two drugs elicit their effects independently, and the expected combination effect can be calculated based on the probability of independent events as:

$$
		y_{BLISS} = y_1 + y_2 - y_1 \cdot y_2
$$

* **Zero Interaction Potency (ZIP)** [Yadav et al., 2015] calculates the expected effect of two drugs under the assumption that they do not potentiate each other:

$$
y_{ZIP} = \frac{(\frac{x_1}{m_1}) ^ {\lambda_1}}{(1 + \frac{x_1}{m_1})^{\lambda_1}}+\frac{(\frac{x_2}{m_2})^{\lambda_2}}{(1 + \frac{x_2}{m_2})^{\lambda_2}} - \frac{(\frac{x_1}{m_1})^{\lambda_1}}{(1 + \frac{x_1}{m_1})^{\lambda_1}} \cdot \frac{(\frac{x_2}{m_2})^{\lambda_2}}{(1 + \frac{x_2}{m_2})^{\lambda_2}}
$$

After these parameters have been specified the calculation of synergy scores and visualization of synergy maps is
done by switching on the "Visualize synergy score" toggle button.

The synergy score for a drug combination is averaged over all the dose combination cells. The 2D and 3D synergy maps highlight synergistic and antagonistic dose regions in red and green colors, respectively (Fig. 5).

SynergyFinder also helps to find the most synergistic area in the synergy interaction map. In a 3D synergy map users are provided with possibilities to (i) rotate, (ii) zoom, (iii) hover over to show synergy scores for any part of the synergy map and (iv) save the resulting image as HTML, PNG or SVG. In a 2D synergy map it also allows zooming in specific regions by brushing and double-clicking. After zooming the synergy score will be recalculated for the chosen area.

![Fig.5 Visualization of synergy maps.](../www/images/visSyn.png)
<p style="text-align: center;">Fig.5 Visualization of synergy maps.</p>

# Download Report

SynergyFinder allows saving all the results in one PDF file. There are 4 types of reports are available (Fig. 6): 

* **static report** For a static report a user must specify which types of plots and which drug combinations should be included in the report (by default, all plots for all drug pair are included). In order to exclude a certain drug pair, click on it from the ‘Choose drug pairs’ box and then press DEL button.
* **dynamic report** A dynamic PDF report allows the rotation of 3D synergy maps, in order to find the best view angle for printing. 
* **short report** A short report is the concise version, where only dose-response matrices and 2D synergy maps are included. A user may also specify whether the most synergistic areas should be shown on the synergy maps.

In addition **the synergy scores** for all the drug combinations could be downloaded in a separate table for further analyses.

![Fig.6 Report Generation](../www/images/report.png)
<p style="text-align: center;">Fig.6 Report Generation</p>

# Bibliography

[Berenbaum, 1989] Berenbaum, M. C. (1989). What is synergy? Pharmacol. Rev., 41(2):93–141.

[Bliss, 1939] Bliss, C. I. (1939). The toxicity of poisons applied jointly1. Annals of Applied Biology, 26(3):585–615.

[Loewe, 1953] Loewe, S. (1953). The problem of synergism and antagonism of combined drugs. Arzneimit- telforschung, 3(6):285–290.

[Yadav et al., 2015] Yadav, B., Wennerberg, K., Aittokallio, T., and Tang, J. (2015). Searching for Drug Synergy in Complex Dose-Response Landscapes Using an Interaction Potency Model. Comput Struct Biotechnol J, 13:504– 513.
