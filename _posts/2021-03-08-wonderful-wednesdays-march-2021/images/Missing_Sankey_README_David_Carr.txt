Sankey plots of the missing data

David Carr

These plots are based on un updated version of the SAS Sankey macros created by Shane Rosanbalm 
of Rho, Inc. 2015, by Yichen Zhong (to incorporate missing data).

-  This macro creates a stacked bar chart with sankey-like links between the stacked bars. 
   It is intended to display the change over time in subject endpoint values.
   These changes are depicted by bands flowing from left to right between the stacked bars. 
   The thickness of each band corresponds to the number of subjects moving from the left to 
   the right.

Note: A two weekly interval was chosen as the version with all 10 post-baseline visits was 
a little cluttered

Reference:
Yichen Zhong (2016) 
Paper LS-142: "Sankey Diagram with Incomplete Data – From a Medical Research Perspective"
SESUG 2016.

ABSTRACT
Sankey diagram is widely used in energy industry but relatively rare in medical research. 
Sankey diagram is an innovative tool in visualizing patient flow in longitudinal data. 
A SAS® Macro for generating Sankey Diagram with Bar Charts was published in 2015. 
However, it did not provide an intuitive solution for missing data or terminated data, 
which is common in medical research. This paper presents a modification of this macro that 
allows subjects with incomplete data to appear on the Sankey diagram. In addition, examples 
of using Sankey diagram in medical research are also provided.

