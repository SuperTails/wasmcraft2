scoreboard players operation %sdiv_q_lo reg *= %%-1 reg
scoreboard players remove %sdiv_q_lo reg 1

scoreboard players operation %sdiv_q_hi reg *= %%-1 reg
scoreboard players remove %sdiv_q_hi reg 1

execute if score %sdiv_q_lo reg matches -1 run scoreboard players add %sdiv_q_hi reg 1
scoreboard players add %sdiv_q_lo reg 1