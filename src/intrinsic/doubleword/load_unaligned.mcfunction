
scoreboard players operation %%ldw_ptr reg = %ptr reg

execute if score %%ldw_align reg matches 4 run function intrinsic:setptr
function intrinsic:load_word
scoreboard players operation %return%0%lo reg = %return%0 reg

scoreboard players operation %ptr reg = %%ldw_ptr reg
scoreboard players add %ptr reg 4
execute if score %%ldw_align reg matches 4 run function intrinsic:setptr
function intrinsic:load_word
scoreboard players operation %return%0%hi reg = %return%0 reg
