## Research project: Bibliographic Databases and Fake News

Research project examining the history of fake news and related topics, such as
misinformation, disinformation, and propaganda as indexed in a variety of
bibliographic databases.

### Data Files

The ``data/data.csv`` is the main data file, which was exported as a CSV file
from ``data/maindata.xlsx``. The data is extracted from ten bibliographic
databases covering a variety of disciplines and topic areas, such as
psychology, business, library and information science, sociology, and more.
Each database was searched using the same search strategy. 

The data file (``data.csv``) contains the following variables:

- Term (string): The controlled terms in the bibliographic databases 
- Freq (integer): The frequency the terms appear in the bibliographic database
- StartYear (integer): A marker indicating the start of the decade the terms appear
- EndYear (integer): A marker indicating the end of the decade the terms appear
- Database (factor): The name of the bibliographic database
- Platform (factor): The name of the bibliographic database provider
- TermType (factor): The type of controlled vocaculary. Using the name provided
  by the Platform

Databases/Platforms include:

| Database                                     | Platform  |
|----------------------------------------------|-----------|
| Business Source Premier (BSP)                | EBSCOHost |
| CINAHL                                       | EBSCOHost |
| Communication and Mass MEDIA Complete (CMMC) | EBSCOHost |
| EconLit                                      | ProQuest  |
| Education Full Text (EFT)                    | EBSCOHost |
| Education Index Retrospective (EIR)          | EBSCOHost |
| Gender Watch (GW)                            | ProQuest  |
| Library, Info Sci and Tech Abstracts (LISTA) | EBSCOHost |
| Library Lit and Information Science (LL)     | EBSCOHost |
| PsychInfo (PI)                               | EBSCOHost |

### Scripts

- ``1-data-prep.R`` is a script that cleans and preps the data for analysis
- ``2-basic-analysis.R`` is a script for basic exploratory analysis
- ``3-term-analysis.R`` is a script that looks more closely at the terms on
  a database and decade level
