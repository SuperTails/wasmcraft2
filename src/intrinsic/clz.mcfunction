# %param0%0 - input
# %return%0 - output

scoreboard players set %return%0 reg 0

execute if score %param0%0 reg matches 0.. run function intrinsic:clz_inner