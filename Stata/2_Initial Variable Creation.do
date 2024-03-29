use "$data/RFQ/inquiry_proccessed.dta" , replace


gen 	trd_bst_response = .
replace trd_bst_response = tradeprice 		if product_cd == "USHY" & !mi(tradeprice)
replace trd_bst_response = tradespread 		if product_cd == "USHG" & !mi(tradespread)
replace trd_bst_response = best_response 	if product_cd == "USHY" &  mi(tradeprice)
replace trd_bst_response = best_response 	if product_cd == "USHG" &  mi(tradespread)

gen cpp_offer_prc_sprd = .
replace cpp_offer_prc_sprd = cpp_offer_price 	if product_cd == "USHY"
replace cpp_offer_prc_sprd = cpp_offer_spread 	if product_cd == "USHG"

gen cpp_bid_prc_sprd = .
replace cpp_bid_prc_sprd = cpp_bid_price 	if product_cd == "USHY"
replace cpp_bid_prc_sprd = cpp_bid_spread 	if product_cd == "USHG"

gen cpp_mid_price_spread = 0.5*(cpp_bid_prc_sprd + cpp_offer_prc_sprd)

gen byte buy_or_sell = .
replace buy_or_sell =  1 if legside == "OWIC"
replace buy_or_sell = -1 if legside == "BWIC"

gen byte price_or_spread = .
replace price_or_spread =  1 if trading_protocol == "Price"
replace price_or_spread = -1 if trading_protocol != "Price"


gen spread_dollar 	= buy_or_sell * price_or_spread *(trd_bst_response - cpp_mid_price_spread)
gen spread_cpp    	= 0.5 * price_or_spread * (cpp_offer_prc_sprd - cpp_bid_prc_sprd)
gen spread 			= spread_dollar / cpp_mid_price_spread

gen req_dollar_value = .
replace req_dollar_value = cpp_offer_prc_sprd	* req_quantity if legside == "OWIC"
replace req_dollar_value = cpp_bid_prc_sprd		* req_quantity if legside == "BWIC"

gen best_dollar_value = trd_bst_response * req_quantity


bysort listid: egen number_assets = nvals(cusip)
replace number_assets = 1 if mi(listid)

gen request_type = ""
replace request_type = "PT"   if inquiry_type == "PT"
replace request_type = "List" if inquiry_type != "PT" & !mi(listid)
replace request_type = "SRFQ" if inquiry_type != "PT" & number_assets == 1 

gen long req_id = .
replace req_id = listid 	if request_type != "SRFQ"
replace req_id = inquiryid 	if request_type == "SRFQ"

bysort req_id: gen First = 1 if _n == 1

gen fill_rate = trade_quantity/req_quantity
replace fill_rate = 0 if inquiry_outcome == "DNT"


gen Amgmt = 0
replace Amgmt = 1 if p_type =="Asset Manager"

gen BD = 0
replace BD = 1 if p_type =="Broker-Dealer"

gen anyresp = 0
replace anyresp = 1 if all_resps > 0


label define binary_label 0 "N" 1 "Y"
foreach var of varlist asap revealdealers OT_invited {
	
	gen byte `var'_n = 1 if `var' == "Y"
	replace  `var'_n = 0 if `var' == "N"
	
	drop `var'
	
	rename `var'_n `var'
	
	label values `var' binary_label
}

save "$data/RFQ/inquiry_proccessed.dta" , replace

local i  1
foreach var of varlist _all {
	
	if `i' > 11  & substr("`: type `var''", 1 , 3)!="str" {
		local number_vars `number_vars' `var'
	}
	
	local i = `i' + 1

}

foreach grade in "USHG" "USHY" {
	
	foreach req_type in "SRFQ" "List" "PT" {
		
		qui estpost summarize `number_vars' if product_cd == "`grade'" & request_type == "`req_type'"
		estimates store `req_type'_`grade', title("`req_type'_`grade'")
		
		local model_names `model_names' `req_type'_`grade'
	}
	
}


esttab `model_names',  cells(mean(fmt(%12.3g))) mtitles(`model_names') modelwidth(13)  varwidth(30) drop(req_id First)
esttab `model_names' using IVC_all.tex, replace   cells(mean(fmt(%12.3g))) mtitles(`model_names') modelwidth(13) plain  varwidth(30) drop(req_id First)
esttab `model_names' using IVC_all.csv, replace   cells(mean(fmt(%12.3g))) mtitles(`model_names') modelwidth(13) plain  varwidth(30) drop(req_id First)


/*
foreach grade in "USHG" "USHY" {
	
	foreach req_type in "SRFQ" "List" "PT"   {
		
		qui estpost summarize number_assets if First == 1 & product_cd == "`grade'" & request_type == "`req_type'"
		estimates store `req_type'_`grade', title("`req_type'_`grade'")
		
		local model_names `model_names' `req_type'_`grade'
	}
	
}


esttab `model_names',  cells("mean") mtitles(`model_names') modelwidth(13)  varwidth(30)

esttab `model_names' using "$tables/IVC_assets.csv", replace  cells("mean") mtitles(`model_names') modelwidth(13) plain  varwidth(30)*/