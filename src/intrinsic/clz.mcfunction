# %param0%0 - input
# %return%0 - output

# start:
#	return0 = 0
# 	if param0 >= 0 { goto inner }
#		
# inner:
#	++return0
#	param0 <<= 1
#	if return0 <= 31 && param0 >= 0 { goto inner }

scoreboard players set %return%0 reg 0

execute if score %param0%0 reg matches 0..65535 run scoreboard players add %return%0 reg 16
execute if score %param0%0 reg matches 0..65535 run scoreboard players operation %param0%0 reg *= %%65536 reg

execute if score %param0%0 reg matches 0..255 run scoreboard players add %return%0 reg 8
execute if score %param0%0 reg matches 0..255 run scoreboard players operation %param0%0 reg *= %%256 reg

# TODO: Do these make it faster?

# execute if score %param0%0 reg matches 0..15 run scoreboard players add %return%0 reg 4
# execute if score %param0%0 reg matches 0..15 run scoreboard players operation %param0%0 reg *= %%16 reg

# execute if score %param0%0 reg matches 0..3 run scoreboard players add %return%0 reg 2
# execute if score %param0%0 reg matches 0..3 run scoreboard players operation %param0%0 reg *= %%4 reg

execute if score %param0%0 reg matches 0.. run function intrinsic:clz_inner