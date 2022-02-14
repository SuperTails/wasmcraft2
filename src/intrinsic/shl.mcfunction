# Arguments:
# %param0%0 - The value to be shifted (also the output)
# %param1%0 - The amount to shift by, is clobbered

execute if score %param1%0 reg matches 1.. run scoreboard players operation %param0%0 reg *= %%2 reg
scoreboard players remove %param1%0 reg 1
execute if score %param1%0 reg matches 0.. run function intrinsic:shl