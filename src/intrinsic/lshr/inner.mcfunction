execute if score %param1%0 reg matches 2.. run scoreboard players operation %%templshr_sign reg /= %%2 reg
scoreboard players operation %param0%0 reg /= %%2 reg

scoreboard players remove %param1%0 reg 1

execute if score %param1%0 reg matches 1.. run function intrinsic:lshr/inner