# Arguments
# %ptr      - Address to store at
# %param0%0 - Word to be stored

scoreboard players operation %tempsave_swu reg = %param0%0 reg
scoreboard players operation %param0%0 reg %= %%65536 reg

scoreboard players operation %%temp0_swu reg = %param0%0 reg

# FIXME: This may not actually work like an `and`
scoreboard players operation %param2%0 reg = %%temp0_swu reg
scoreboard players operation %param2%0 reg %= %%256 reg
function intrinsic:setptr
function intrinsic:store_byte
scoreboard players add %ptr reg 1

scoreboard players operation %param0%0 reg = %%temp0_swu reg
scoreboard players set %param1%0 reg 8
function intrinsic:lshr
scoreboard players operation %param2%0 reg = %param0%0 reg
scoreboard players operation %param2%0 reg %= %%256 reg
function intrinsic:setptr
function intrinsic:store_byte

scoreboard players operation %param0%0 reg = %tempsave_swu reg