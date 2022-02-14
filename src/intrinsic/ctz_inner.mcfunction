scoreboard players operation %temp%0 reg = %param0%0 reg
scoreboard players operation %temp%0 reg %= %%2 reg

scoreboard players operation %param0%0 reg /= %%2 reg

scoreboard players add %return%0 reg 1

execute if score %return%0 reg matches ..31 if score %temp%0 reg matches 0..0 run function intrinsic:ctz_inner