# Arguments:
# %param0%0 - The value to be shifted (also the output)
# %param1%0 - The amount to shift by, is clobbered

execute if score %param1%0 reg matches 16.. run scoreboard players operation %param0%0 reg *= %%65536 reg
execute if score %param1%0 reg matches 16.. run scoreboard players remove %param1%0 reg 16

execute if score %param1%0 reg matches 8.. run scoreboard players operation %param0%0 reg *= %%256 reg
execute if score %param1%0 reg matches 8.. run scoreboard players remove %param1%0 reg 8

execute if score %param1%0 reg matches 4.. run scoreboard players operation %param0%0 reg *= %%16 reg
execute if score %param1%0 reg matches 4.. run scoreboard players remove %param1%0 reg 4

execute if score %param1%0 reg matches 2.. run scoreboard players operation %param0%0 reg *= %%4 reg
execute if score %param1%0 reg matches 2.. run scoreboard players remove %param1%0 reg 2

execute if score %param1%0 reg matches 1.. run scoreboard players operation %param0%0 reg *= %%2 reg

# execute if score %param1%0 reg matches 1.. run scoreboard players operation %param0%0 reg *= %%2 reg
# scoreboard players remove %param1%0 reg 1
# execute if score %param1%0 reg matches 0.. run function intrinsic:shl