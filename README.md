# CIO_Brief

The CIOBrief is generated from 3 files: preProcess.R -> weekly_CIO_Brief.R -> CIOBrief.pptx.

1. The **preProcess.R** reads the latest data from SharePoint and SNOW data at
*//r01slchsm01.r01.med.va.gov/ISL_Workgroups/Operations Triage Center/Production/Data Repository/OTG SP Data Extracts/* and pre-processes the data.

2. The **weekly_CIO_Brief.R** _sources_ preProcess.R and creates a set of figures and tables that are then appended to a PowerPoint blank template.

3. The **CIOBrief.pptx** file includes tables and figures detailing previous week (called current week in the slides) events broken out by day of week, business line, cupability, and other variables of interest compared to other weeks over time. Slide 2 usually denotes conclusions and major findings for the week. 

CIO Brief slides are presented at the Daily IT Operational Staff Briefing every Friday morning at 8 AM EST, and meetings before then dictate the exact content of these slides.
