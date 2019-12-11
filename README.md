Research project comparing database results.

Notes:

When exporting the data-dates.ods data to CSV, need to strip the non-printable
characters from the file. Did that in Vim (for some reason, couldn't get it to
work in ``sed``.

```Vim
%s/[^[:print:]]//
```
