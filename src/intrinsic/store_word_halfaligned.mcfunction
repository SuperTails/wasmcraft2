
scoreboard players operation %swha_savedptr reg = %ptr reg

scoreboard players set %tempsave_store_word reg 0

scoreboard players operation %param2%0 reg = %param0%0 reg 
scoreboard players operation %param2%0 reg %= %%65536 reg
function intrinsic:store_halfword_aligned

scoreboard players add %ptr reg 2
scoreboard players operation %param2%0 reg = %param0%0 reg
scoreboard players operation %param2%0 reg /= %%65536 reg
scoreboard players operation %param2%0 reg %= %%65536 reg
function intrinsic:store_halfword_aligned

scoreboard players operation %ptr reg = %swha_savedptr reg