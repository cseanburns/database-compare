Research project comparing database results.

1. data.csv is main data
2. data-wos.csv is data from WOS. Kept separate since WOS is more general than the other databases in data.csv

Later:

1. Merge data-wos.csv and Scopus.xlsx into a single file called data-general.csv

Notes:

When exporting the data-dates.ods data to CSV, need to strip the non-printable
characters from the file. Did that in Vim (for some reason, couldn't get it to
work in ``sed``.

```Vim
%s/[^[:print:]]//
```
