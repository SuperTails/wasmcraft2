scoreboard players add %return%0 reg 1

scoreboard players operation %param0%0 reg += %param0%0 reg

execute if score %return%0 reg matches ..31 if score %param0%0 reg matches 0.. run function intrinsic:clz_inner