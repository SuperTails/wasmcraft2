# %udiv_ult_lhs reg ->
# %udiv_ult_rhs reg ->
# -> %udiv_ult_is_less reg

scoreboard players set %udiv_ult_is_less reg 0
execute if score %udiv_ult_lhs reg matches 0.. if score %udiv_ult_rhs reg matches ..-1 run scoreboard players set %udiv_ult_is_less reg 1
execute if score %udiv_ult_lhs reg matches ..-1 if score %udiv_ult_rhs reg matches ..-1 if score %udiv_ult_lhs reg < %udiv_ult_rhs reg run scoreboard players set %udiv_ult_is_less reg 1
execute if score %udiv_ult_lhs reg matches 0.. if score %udiv_ult_rhs reg matches 0.. if score %udiv_ult_lhs reg < %udiv_ult_rhs reg run scoreboard players set %udiv_ult_is_less reg 1