scoreboard players operation %sdiv_lhs_lo reg *= %%-1 reg
scoreboard players remove %sdiv_lhs_lo reg 1

scoreboard players operation %sdiv_lhs_hi reg *= %%-1 reg
scoreboard players remove %sdiv_lhs_hi reg 1

execute if score %sdiv_lhs_lo reg matches -1 run scoreboard players add %sdiv_lhs_hi reg 1
scoreboard players add %sdiv_lhs_lo reg 1

scoreboard players operation %sdiv_sign reg *= %%-1 reg