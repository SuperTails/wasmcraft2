

execute store success score %%temp_sign reg if score %param0%1 reg matches ..-1
scoreboard players operation %%temp_sign reg *= %%-2147483648 reg

execute if score %param1%0 reg matches 1.. run function intrinsic:lshr_i64/shift_once
execute if score %param1%0 reg matches 1.. run scoreboard players operation %param0%1 reg += %%temp_sign reg

scoreboard players remove %param1%0 reg 1
execute if score %param1%0 reg matches 1.. run function intrinsic:ashr_i64