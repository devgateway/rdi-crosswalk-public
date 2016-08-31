* This is editing the classified Ghana, Sri Lanka, and Tanzania data

* First we paste the data in from Excel, then we run the following commands
replace year = 2000 if year<2000
replace year = 2016 if year>2016
drop name
sort cluster subcluster keyword donor year
by cluster subcluster keyword donor year: egen keyvalue = total(all_tot)
by cluster subcluster keyword donor year: generate dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
drop all_tot
rename keyvalue all_tot
sort cluster subcluster keyword
by cluster subcluster keyword: egen keyvalue = total(all_tot)
replace donor = keyword + " by " + donor
by cluster subcluster keyword: generate indi = _n
egen grouper = group(cluster subcluster keyword)
sort group indi
generate o_key = "filler"
order cluster subcluster keyword keyvalue donor all_tot grouper indi o_key sector year
