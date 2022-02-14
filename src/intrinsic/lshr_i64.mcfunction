

execute if score %param1%0 reg matches 64.. run scoreboard players set %param0%0 reg 0
execute if score %param1%0 reg matches 64.. run scoreboard players set %param1%0 reg 0

execute if score %param1%0 reg matches 1..63 run function intrinsic:lshr_i64/shift_once
scoreboard players remove %param1%0 reg 1

execute if score %param1%0 reg matches 1..63 run function intrinsic:lshr_i64