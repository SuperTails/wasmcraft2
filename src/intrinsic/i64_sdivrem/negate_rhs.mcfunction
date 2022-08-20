scoreboard players operation %sdiv_rhs_lo reg *= %%-1 reg
scoreboard players remove %sdiv_rhs_lo reg 1

scoreboard players operation %sdiv_rhs_hi reg *= %%-1 reg
scoreboard players remove %sdiv_rhs_hi reg 1

execute if score %sdiv_rhs_lo reg matches -1 run scoreboard players add %sdiv_rhs_hi reg 1
scoreboard players add %sdiv_rhs_lo reg 1

scoreboard players operation %sdiv_sign reg *= %%-1 reg