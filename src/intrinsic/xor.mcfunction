# first argument - %param0%0 reg 
# second argument - %param1%0 reg
# return value - %return%0 reg

execute if score %param1%0 reg matches 0 run scoreboard players operation %return%0 reg = %param0%0 reg

execute if score %param1%0 reg matches -1 run scoreboard players operation %return%0 reg = %param0%0 reg
execute if score %param1%0 reg matches -1 run scoreboard players operation %return%0 reg *= %%-1 reg
execute if score %param1%0 reg matches -1 run scoreboard players remove %return%0 reg 1

execute unless score %param1%0 reg matches -1..0 run function intrinsic:xor_normal