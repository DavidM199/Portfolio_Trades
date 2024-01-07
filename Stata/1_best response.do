* reading raw inquiry data and keep it in a frame
frames reset

use "$data/RFQ/inquiry.dta" , clear
frame rename default inquiry

* reading raw response data and keep it in a frame
frame create responses
frame rename responses resp
frame change resp
use "$data/RFQ/response.dta" , clear

* link resp frame with inquiry frame (get variables for this task)
frlink m:1 inquiryid , frame(inquiry)
frget trading_protocol legside req_quantity, from(inquiry)

*unlink
drop inquiry

* inquiries with no response have no best response
drop if response_via_OT == "No Response"


* status of response_quantity regarding the req_quantity
gen qnt_match = 1 if req_quantity == response_quantity
gen qnt_range = 1 if 0.5*req_quantity < response_quantity  & response_quantity < 1.5*req_quantity


* status of response_quantity of all responses regarding the req_quantity
sort inquiryid
by inquiryid: egen qnt_match_exist = max(qnt_match)
by inquiryid: egen qnt_range_exist = max(qnt_range)

* remove responses that didn't fullfil the req_quantity (if it doesn't remove all responses)
drop if qnt_match_exist == 1 & mi(qnt_match)
drop if qnt_range_exist == 1 & mi(qnt_range)

* generate best_response based on the rules
gen best_response = .

by inquiryid: ereplace best_response = max(response_level) if  trading_protocol == "Price" & legside == "BWIC"
by inquiryid: ereplace best_response = min(response_level) if  trading_protocol != "Price" & legside == "BWIC"

by inquiryid: ereplace best_response = min(response_level) if  trading_protocol == "Price" & legside == "OWIC"
by inquiryid: ereplace best_response = max(response_level) if  trading_protocol != "Price" & legside == "OWIC"

* keep best response
keep inquiryid best_response
duplicates drop

* change frame to inquiry and get the respective best_response for each inquiry
frame change inquiry
frlink 1:1 inquiryid , frame(resp)
frget best_response  , from(resp)
drop resp

* save data
save "$data/RFQ/inquiry_proccessed.dta" , replace

clear frames
