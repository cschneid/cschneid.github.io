---
layout: post
title: "Checking EE Bonds with Google Sheets"
date: 2019-06-23 20:40:25 +0000
author: "Chris Schneider"
comments: true
categories:
  - Hacks
---

I had a stack of old and untracked old United States EE Treasury bonds. It was
finally time to check them.

After typing in the Serial Number, Issue Date, and Face Value into Google
Sheets, I wanted to fetch the rest of the interesting data from the official
[Treasury site](https://www.treasurydirect.gov/BC/SBCPrice).

It took me a bit to figure out the right spreadsheet formula, so I figured I'd
write it here if anybody else was looking.

Manually fill out the data:

* A column: Serial Number
* B column: Date Issued
* C column: Face Value


```
=QUERY(ImportHtml("https://www.treasurydirect.gov/BC/SBCPrice?RedemptionDate=06%2F2019&Series=EE&Denomination=" & ENCODEURL(C2) & "&SerialNumber=" & A2 & "&IssueDate=" & ENCODEURL(B2) & "&btnAdd.x=CALCULATE&OldRedemptionDate=782&ViewPos=1","table",4),"offset 1",0)
```

I put this in column E, and the values will be:

* E: Serial Number again
* F: Type ("EE")
* G: Face Value
* H: Issue Date
* I: Next Accrual
* J: Final Maturity
* K: Purchase Price
* L: Total Interest
* M: Interest Rate
* N: Total Current Value
* O: Note Codes ("MA" == Matured)

Then to fix up the total current value column, I had one more I put in R:

```
=value(regexreplace(N2,"(\$|\*)",""))
```

This converts the string value with the asterisk and dollar sign over to be a
proper number that Google Sheets can do math on.
