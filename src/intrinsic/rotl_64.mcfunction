# Arguments:
# %param0%0 
# %param0%1 - The value to be shifted (also the output)

# %param1%0 - The amount to shift by

execute if score %param1%0 reg matches 1.. run function intrinsic:rotl_64_once

scoreboard players remove %param1%0 reg 1
execute if score %param1%0 reg matches 1.. run function intrinsic:rotl_64
