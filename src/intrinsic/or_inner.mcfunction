# return <<= 1
scoreboard players operation %return%0 reg += %return%0 reg

# if param0 < 0 { c += 1 }
execute if score %param0%0 reg matches ..-1 run scoreboard players add %return%0 reg 1

# else if param0 < 0 { c += 1 }
execute if score %param1%0 reg matches ..-1 if score %param0%0 reg matches 0.. run scoreboard players add %return%0 reg 1

scoreboard players operation %param0%0 reg += %param0%0 reg
scoreboard players operation %param1%0 reg += %param1%0 reg