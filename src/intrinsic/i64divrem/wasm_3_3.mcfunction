execute store result score %work%3%47%lo%temp reg run data get storage wasm:localstack stack.data[0]
execute store result score %work%3%47%hi%temp reg run data get storage wasm:localstack stack.data[1]
scoreboard players set %work%3%48%lo%temp reg 0
scoreboard players set %work%3%48%hi%temp reg 0
execute store result score %work%3%49%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%3%49%hi%temp reg run data get storage wasm:localstack stack.data[7]
scoreboard players operation %temp%11%lo reg = %work%3%49%lo%temp reg
scoreboard players operation %temp%11%hi reg = %work%3%49%hi%temp reg
scoreboard players operation %temp%11%lo reg *= %%-1 reg
scoreboard players remove %temp%11%lo reg 1
scoreboard players operation %temp%11%hi reg *= %%-1 reg
scoreboard players remove %temp%11%hi reg 1
execute if score %temp%11%lo reg matches -1 run scoreboard players add %temp%11%hi reg 1
scoreboard players add %temp%11%lo reg 1
scoreboard players operation %work%3%50%lo%temp reg = %work%3%48%lo%temp reg
scoreboard players operation %work%3%50%hi%temp reg = %work%3%48%hi%temp reg
scoreboard players operation %work%3%50%lo%temp reg += %temp%11%lo reg
scoreboard players operation %work%3%50%hi%temp reg += %temp%11%hi reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%3%48%lo%temp reg matches ..-1 if score %temp%11%lo reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%3%48%lo%temp reg matches ..-1 if score %temp%11%lo reg matches 0.. if score %work%3%50%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%3%48%lo%temp reg matches 0.. if score %temp%11%lo reg matches ..-1 if score %work%3%50%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%3%50%hi%temp reg += %temp%10%lo reg
execute store result score %work%3%51%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%3%51%hi%temp reg run data get storage wasm:localstack stack.data[7]
execute store result score %work%3%52%lo%temp reg run data get storage wasm:localstack stack.data[0]
execute store result score %work%3%52%hi%temp reg run data get storage wasm:localstack stack.data[1]
scoreboard players set %work%3%53%lo%temp reg -1
scoreboard players set %work%3%53%hi%temp reg -1
scoreboard players set %work%3%54%lo%temp reg 0
execute if score %work%3%53%lo%temp reg matches ..-1 if score %work%3%52%lo%temp reg matches 0.. run scoreboard players set %work%3%54%lo%temp reg 0
execute if score %work%3%53%lo%temp reg matches 0.. if score %work%3%52%lo%temp reg matches ..-1 run scoreboard players set %work%3%54%lo%temp reg 1
execute if score %work%3%53%lo%temp reg matches ..-1 if score %work%3%52%lo%temp reg matches ..-1 if score %work%3%53%lo%temp reg < %work%3%52%lo%temp reg run scoreboard players set %work%3%54%lo%temp reg 1
execute if score %work%3%53%lo%temp reg matches 0.. if score %work%3%52%lo%temp reg matches 0.. if score %work%3%53%lo%temp reg < %work%3%52%lo%temp reg run scoreboard players set %work%3%54%lo%temp reg 1
execute if score %work%3%53%hi%temp reg < %work%3%52%hi%temp reg run scoreboard players set %work%3%54%lo%temp reg 1
execute if score %work%3%53%hi%temp reg > %work%3%52%hi%temp reg run scoreboard players set %work%3%54%lo%temp reg 0
execute store result score %work%3%55%lo%temp reg run data get storage wasm:localstack stack.data[0]
execute store result score %work%3%55%hi%temp reg run data get storage wasm:localstack stack.data[1]
scoreboard players set %work%3%56%lo%temp reg 63
scoreboard players set %work%3%56%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%3%55%lo%temp reg
scoreboard players operation %param0%1 reg = %work%3%55%hi%temp reg
scoreboard players operation %param1%0 reg = %work%3%56%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:lshr_i64
scoreboard players operation %work%3%57%lo%temp reg = %param0%0 reg
scoreboard players operation %work%3%57%hi%temp reg = %param0%1 reg
scoreboard players operation %work%3%58%lo%temp reg = %work%3%57%lo%temp reg
execute store result score %work%3%59%lo%temp reg run data get storage wasm:localstack stack.data[2]
execute store result score %work%3%59%hi%temp reg run data get storage wasm:localstack stack.data[3]
scoreboard players set %work%3%60%lo%temp reg 0
scoreboard players set %work%3%60%hi%temp reg 0
scoreboard players set %work%3%61%lo%temp reg 0
execute if score %work%3%59%lo%temp reg matches ..-1 if score %work%3%60%lo%temp reg matches 0.. run scoreboard players set %work%3%61%lo%temp reg 0
execute if score %work%3%59%lo%temp reg matches 0.. if score %work%3%60%lo%temp reg matches ..-1 run scoreboard players set %work%3%61%lo%temp reg 1
execute if score %work%3%59%lo%temp reg matches ..-1 if score %work%3%60%lo%temp reg matches ..-1 if score %work%3%59%lo%temp reg < %work%3%60%lo%temp reg run scoreboard players set %work%3%61%lo%temp reg 1
execute if score %work%3%59%lo%temp reg matches 0.. if score %work%3%60%lo%temp reg matches 0.. if score %work%3%59%lo%temp reg < %work%3%60%lo%temp reg run scoreboard players set %work%3%61%lo%temp reg 1
execute if score %work%3%59%hi%temp reg < %work%3%60%hi%temp reg run scoreboard players set %work%3%61%lo%temp reg 1
execute if score %work%3%59%hi%temp reg > %work%3%60%hi%temp reg run scoreboard players set %work%3%61%lo%temp reg 0
scoreboard players operation %work%3%62%lo%temp reg = %work%3%58%lo%temp reg
execute unless score %work%3%61%lo%temp reg matches 0 run scoreboard players operation %work%3%62%lo%temp reg = %work%3%54%lo%temp reg
scoreboard players operation %work%3%63%lo%temp reg = %work%3%51%lo%temp reg
execute unless score %work%3%62%lo%temp reg matches 0 run scoreboard players operation %work%3%63%lo%temp reg = %work%3%50%lo%temp reg
scoreboard players operation %work%3%63%hi%temp reg = %work%3%51%hi%temp reg
execute unless score %work%3%62%lo%temp reg matches 0 run scoreboard players operation %work%3%63%hi%temp reg = %work%3%50%hi%temp reg
execute store result score %work%3%64%lo%temp reg run data get storage wasm:localstack stack.data[2]
execute store result score %work%3%64%hi%temp reg run data get storage wasm:localstack stack.data[3]
scoreboard players operation %param0%0 reg = %work%3%63%lo%temp reg
scoreboard players operation %param1%0 reg = %work%3%64%lo%temp reg
function intrinsic:mul_32_to_64
scoreboard players operation %work%3%65%lo%temp reg = %return%0 reg
scoreboard players operation %work%3%65%hi%temp reg = %return%1 reg
scoreboard players operation %temp%1000%lo reg = %work%3%63%lo%temp reg
scoreboard players operation %temp%1000%lo reg *= %work%3%64%hi%temp reg
scoreboard players operation %work%3%65%hi%temp reg += %temp%1000%lo reg
scoreboard players operation %temp%1000%lo reg = %work%3%63%hi%temp reg
scoreboard players operation %temp%1000%lo reg *= %work%3%64%lo%temp reg
scoreboard players operation %work%3%65%hi%temp reg += %temp%1000%lo reg
scoreboard players operation %temp%11%lo reg = %work%3%65%lo%temp reg
scoreboard players operation %temp%11%hi reg = %work%3%65%hi%temp reg
scoreboard players operation %temp%11%lo reg *= %%-1 reg
scoreboard players remove %temp%11%lo reg 1
scoreboard players operation %temp%11%hi reg *= %%-1 reg
scoreboard players remove %temp%11%hi reg 1
execute if score %temp%11%lo reg matches -1 run scoreboard players add %temp%11%hi reg 1
scoreboard players add %temp%11%lo reg 1
scoreboard players operation %work%3%66%lo%temp reg = %work%3%47%lo%temp reg
scoreboard players operation %work%3%66%hi%temp reg = %work%3%47%hi%temp reg
scoreboard players operation %work%3%66%lo%temp reg += %temp%11%lo reg
scoreboard players operation %work%3%66%hi%temp reg += %temp%11%hi reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%3%47%lo%temp reg matches ..-1 if score %temp%11%lo reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%3%47%lo%temp reg matches ..-1 if score %temp%11%lo reg matches 0.. if score %work%3%66%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%3%47%lo%temp reg matches 0.. if score %temp%11%lo reg matches ..-1 if score %work%3%66%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%3%66%hi%temp reg += %temp%10%lo reg
scoreboard players operation %temp%0%lo reg = %work%3%66%lo%temp reg
scoreboard players operation %temp%0%hi reg = %work%3%66%hi%temp reg
scoreboard players operation %work%3%67%lo%temp reg = %temp%0%lo reg
scoreboard players operation %work%3%67%hi%temp reg = %temp%0%hi reg
function intrinsic:i64divrem/wasm_3_1
scoreboard players set %condtaken reg 1