
global data 	"../Data/"
global tables 	"../Outputs_Hamed/Tables"
global figures 	"../Outputs_Hamed/Figures"

* Extract the best response from responseData and attach it to inquiryData
do "1_best response.do"

* Creating initial variables
do "2_Initial Variable Creation"

* Extract bond characteristics and ratings (from mergent-fisd) for our dataset
do "3_BondRating.do"

