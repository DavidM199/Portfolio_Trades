use "$data/RFQ/inquiry_proccessed.dta" , clear

keep cusip
duplicates drop
rename cusip COMPLETE_CUSIP

preserve


merge 1:1 COMPLETE_CUSIP using "$data/MergentFisd/FISD_bond issues.dta"
drop if _merge != 3
drop _merge
rename COMPLETE_CUSIP cusip
save "$data/MergentFisd/bondFacts.dta" , replace


restore


merge 1:m COMPLETE_CUSIP using "$data/MergentFisd/FISD_bond rating.dta"
drop if _merge != 3

keep COMPLETE_CUSIP RATING_TYPE RATING_DATE RATING


foreach var of varlist * {
  rename `var' `=strlower("`var'")'
}
rename complete_cusip cusip


duplicates drop
duplicates tag cusip rating_type rating_date, generate(duplicate)
drop if duplicate == 1
drop duplicate

reshape wide rating, i(cusip rating_date) j(rating_type) string

save "$data/MergentFisd/bondRating.dta" , replace





