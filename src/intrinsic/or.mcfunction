scoreboard players operation %tempp1lo reg = %param0%0 reg
scoreboard players operation %tempp1lo reg %= %%65536 reg

execute unless score %param1%0 reg matches 0..65535 run scoreboard players set %temp1lo reg 1

execute if score %tempp1lo reg matches 0 run scoreboard players operation %return%0 reg = %param0%0 reg
execute if score %tempp1lo reg matches 0 run scoreboard players operation %return%0 reg += %param1%0 reg

execute unless score %tempp1lo reg matches 0 run function intrinsic:or_normal