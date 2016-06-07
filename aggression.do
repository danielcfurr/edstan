* Export the verbal aggression data from Stata

use aggression, clear
generate poly = y
generate dich = y > 0
generate do = do_want == 1
generate other = other_self == 1
generate scold = express == -1
generate shout = blame == -1

drop y i1-i24 do_want-express
order person anger gender item description do-shout poly dich

export delimited using "aggression.csv", replace
