## Research project comparing database results

- ``data/data.csv`` is main data

### Notes

#### Files

- ``archived-data/misinformation_compare_10_30jb.xlsx`` is the original data
  file created by @bossjen and was copied and pasted into a single sheet named
  ``data.ods``. 
- When exporting the ``archived-data/data.ods`` data to CSV
  (``archived-data/data.csv``), I needed to strip the non-printable characters
  from the file (from Microsoft Excel???). Did that in Vim because for some
  reason, I couldn't get it to work in ``sed``. In Vim, after saving
  ``archived-data/data.ods`` as a CSV file:

```Vim
%s/[^[:print:]]//
```
#### Scripts

- ``analysis.R`` is the script to examine database frequencies by decades
- ``term-analysis.R`` is the script to examine terms by database by decades

#### Other data

Not using in this repo, but keeping here for @bossjen.

1. ``data-archived/scopus.xlsxi`` is dat from *Scopus*
2. ``data-archived/data-wos.csv`` is data from *Web of Science*
