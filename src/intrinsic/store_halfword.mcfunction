# arguments:
# %ptr - The location to write to
# %param2%0 - The halfword to write

scoreboard players operation %tempsave_store_word reg = %param2%0 reg
scoreboard players operation %param2%0 reg %= %%65536 reg

scoreboard players operation %%temp0_store_byte reg = %ptr reg
scoreboard players operation %%temp0_store_byte reg %= %%2 reg
execute unless score %%temp0_store_byte reg matches 0 run function intrinsic:store_halfword_unaligned
execute if score %%temp0_store_byte reg matches 0 run function intrinsic:store_halfword_aligned