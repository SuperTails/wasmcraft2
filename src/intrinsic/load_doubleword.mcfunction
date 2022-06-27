# %ptr reg - Address to load
# %return%0%lo - Low word of value value
# %return%0%hi - High word of return value

scoreboard players operation %%ldw_align reg = %ptr reg
scoreboard players operation %%ldw_align reg %= %%8 reg

execute if score %%ldw_align reg matches 0 run function intrinsic:doubleword/load_aligned

execute if score %%ldw_align reg matches 1..7 run function intrinsic:doubleword/load_unaligned