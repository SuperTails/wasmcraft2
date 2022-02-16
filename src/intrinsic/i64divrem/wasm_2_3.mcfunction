scoreboard players set %work%2%47%lo%temp reg 0
scoreboard players set %work%2%47%hi%temp reg 0
execute store result score %work%2%48%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%2%48%hi%temp reg run data get storage wasm:localstack stack.data[7]
scoreboard players operation %temp%11%lo reg = %work%2%48%lo%temp reg
scoreboard players operation %temp%11%hi reg = %work%2%48%hi%temp reg
scoreboard players operation %temp%11%lo reg *= %%-1 reg
scoreboard players remove %temp%11%lo reg 1
scoreboard players operation %temp%11%hi reg *= %%-1 reg
scoreboard players remove %temp%11%hi reg 1
execute if score %temp%11%lo reg matches -1 run scoreboard players add %temp%11%hi reg 1
scoreboard players add %temp%11%lo reg 1
scoreboard players operation %work%2%49%lo%temp reg = %work%2%47%lo%temp reg
scoreboard players operation %work%2%49%hi%temp reg = %work%2%47%hi%temp reg
scoreboard players operation %work%2%49%lo%temp reg += %temp%11%lo reg
scoreboard players operation %work%2%49%hi%temp reg += %temp%11%hi reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%2%47%lo%temp reg matches ..-1 if score %temp%11%lo reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%2%47%lo%temp reg matches ..-1 if score %temp%11%lo reg matches 0.. if score %work%2%49%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%2%47%lo%temp reg matches 0.. if score %temp%11%lo reg matches ..-1 if score %work%2%49%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%2%49%hi%temp reg += %temp%10%lo reg
execute store result score %work%2%50%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%2%50%hi%temp reg run data get storage wasm:localstack stack.data[7]
execute store result score %work%2%51%lo%temp reg run data get storage wasm:localstack stack.data[0]
execute store result score %work%2%51%hi%temp reg run data get storage wasm:localstack stack.data[1]
scoreboard players set %work%2%52%lo%temp reg -1
scoreboard players set %work%2%52%hi%temp reg -1
scoreboard players set %work%2%53%lo%temp reg 0
execute if score %work%2%52%lo%temp reg matches ..-1 if score %work%2%51%lo%temp reg matches 0.. run scoreboard players set %work%2%53%lo%temp reg 0
execute if score %work%2%52%lo%temp reg matches 0.. if score %work%2%51%lo%temp reg matches ..-1 run scoreboard players set %work%2%53%lo%temp reg 1
execute if score %work%2%52%lo%temp reg matches ..-1 if score %work%2%51%lo%temp reg matches ..-1 if score %work%2%52%lo%temp reg < %work%2%51%lo%temp reg run scoreboard players set %work%2%53%lo%temp reg 1
execute if score %work%2%52%lo%temp reg matches 0.. if score %work%2%51%lo%temp reg matches 0.. if score %work%2%52%lo%temp reg < %work%2%51%lo%temp reg run scoreboard players set %work%2%53%lo%temp reg 1
execute if score %work%2%52%hi%temp reg < %work%2%51%hi%temp reg run scoreboard players set %work%2%53%lo%temp reg 1
execute if score %work%2%52%hi%temp reg > %work%2%51%hi%temp reg run scoreboard players set %work%2%53%lo%temp reg 0
execute store result score %work%2%54%lo%temp reg run data get storage wasm:localstack stack.data[0]
execute store result score %work%2%54%hi%temp reg run data get storage wasm:localstack stack.data[1]
scoreboard players set %work%2%55%lo%temp reg 63
scoreboard players set %work%2%55%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%2%54%lo%temp reg
scoreboard players operation %param0%1 reg = %work%2%54%hi%temp reg
scoreboard players operation %param1%0 reg = %work%2%55%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:lshr_i64
scoreboard players operation %work%2%56%lo%temp reg = %param0%0 reg
scoreboard players operation %work%2%56%hi%temp reg = %param0%1 reg
scoreboard players operation %work%2%57%lo%temp reg = %work%2%56%lo%temp reg
execute store result score %work%2%58%lo%temp reg run data get storage wasm:localstack stack.data[2]
execute store result score %work%2%58%hi%temp reg run data get storage wasm:localstack stack.data[3]
scoreboard players set %work%2%59%lo%temp reg 0
scoreboard players set %work%2%59%hi%temp reg 0
scoreboard players set %work%2%60%lo%temp reg 0
execute if score %work%2%58%lo%temp reg matches ..-1 if score %work%2%59%lo%temp reg matches 0.. run scoreboard players set %work%2%60%lo%temp reg 0
execute if score %work%2%58%lo%temp reg matches 0.. if score %work%2%59%lo%temp reg matches ..-1 run scoreboard players set %work%2%60%lo%temp reg 1
execute if score %work%2%58%lo%temp reg matches ..-1 if score %work%2%59%lo%temp reg matches ..-1 if score %work%2%58%lo%temp reg < %work%2%59%lo%temp reg run scoreboard players set %work%2%60%lo%temp reg 1
execute if score %work%2%58%lo%temp reg matches 0.. if score %work%2%59%lo%temp reg matches 0.. if score %work%2%58%lo%temp reg < %work%2%59%lo%temp reg run scoreboard players set %work%2%60%lo%temp reg 1
execute if score %work%2%58%hi%temp reg < %work%2%59%hi%temp reg run scoreboard players set %work%2%60%lo%temp reg 1
execute if score %work%2%58%hi%temp reg > %work%2%59%hi%temp reg run scoreboard players set %work%2%60%lo%temp reg 0
scoreboard players operation %work%2%61%lo%temp reg = %work%2%57%lo%temp reg
execute unless score %work%2%60%lo%temp reg matches 0 run scoreboard players operation %work%2%61%lo%temp reg = %work%2%53%lo%temp reg
scoreboard players operation %work%2%62%lo%temp reg = %work%2%50%lo%temp reg
execute unless score %work%2%61%lo%temp reg matches 0 run scoreboard players operation %work%2%62%lo%temp reg = %work%2%49%lo%temp reg
scoreboard players operation %work%2%62%hi%temp reg = %work%2%50%hi%temp reg
execute unless score %work%2%61%lo%temp reg matches 0 run scoreboard players operation %work%2%62%hi%temp reg = %work%2%49%hi%temp reg
scoreboard players operation %temp%0%lo reg = %work%2%62%lo%temp reg
scoreboard players operation %temp%0%hi reg = %work%2%62%hi%temp reg
scoreboard players operation %work%2%63%lo%temp reg = %temp%0%lo reg
scoreboard players operation %work%2%63%hi%temp reg = %temp%0%hi reg
function intrinsic:i64divrem/wasm_2_1
scoreboard players set %condtaken reg 1