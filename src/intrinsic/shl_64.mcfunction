# Arguments:
# %param0%0 
# %param0%1 - The value to be shifted (also the output)

# %param1%0 - The amount to shift by, is clobbered

# temp_carry = ((param0_lo) & (1 << 31)) != 0
execute if score %param1%0 reg matches 1.. run execute store success score %temp_carry reg if score %param0%0 reg matches ..-1
# param0_lo <<= 1
execute if score %param1%0 reg matches 1.. run scoreboard players operation %param0%0 reg *= %%2 reg
# param0_hi <<= 1
execute if score %param1%0 reg matches 1.. run scoreboard players operation %param0%1 reg *= %%2 reg
# param0_lo += carry
execute if score %param1%0 reg matches 1.. run scoreboard players operation %param0%1 reg += %temp_carry reg

scoreboard players remove %param1%0 reg 1
execute if score %param1%0 reg matches 0.. run function intrinsic:shl_64