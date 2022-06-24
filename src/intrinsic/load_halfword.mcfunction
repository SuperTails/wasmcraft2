# %ptr - The location to read from
# %return%0 - The return value
# Clobbers %param1%0

scoreboard players operation %%load_halfword_align reg = %ptr reg
scoreboard players operation %%load_halfword_align reg %= %%2 reg

execute if score %%load_halfword_align reg matches 0 run function intrinsic:load_halfword_aligned
execute if score %%load_halfword_align reg matches 1 run function intrinsic:load_halfword_unaligned