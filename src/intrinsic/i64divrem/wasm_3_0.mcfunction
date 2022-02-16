data modify storage wasm:scratch stack.data set value [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
data modify storage wasm:scratch stack.tail set from storage wasm:localstack stack
data modify storage wasm:localstack stack set from storage wasm:scratch stack
execute store result storage wasm:localstack stack.data[0] int 1 run scoreboard players get %param%0%lo reg
execute store result storage wasm:localstack stack.data[1] int 1 run scoreboard players get %param%0%hi reg
execute store result storage wasm:localstack stack.data[2] int 1 run scoreboard players get %param%1%lo reg
execute store result storage wasm:localstack stack.data[3] int 1 run scoreboard players get %param%1%hi reg
execute store result storage wasm:localstack stack.data[4] int 1 run scoreboard players get %const%0 reg
execute store result storage wasm:localstack stack.data[5] int 1 run scoreboard players get %const%0 reg
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %const%0 reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %const%0 reg
execute store result storage wasm:localstack stack.data[8] int 1 run scoreboard players get %const%0 reg
execute store result storage wasm:localstack stack.data[9] int 1 run scoreboard players get %const%0 reg
execute store result storage wasm:localstack stack.data[10] int 1 run scoreboard players get %const%0 reg
execute store result storage wasm:localstack stack.data[11] int 1 run scoreboard players get %const%0 reg
execute store result storage wasm:localstack stack.data[12] int 1 run scoreboard players get %const%0 reg
execute store result storage wasm:localstack stack.data[13] int 1 run scoreboard players get %const%0 reg
execute store result storage wasm:localstack stack.data[14] int 1 run scoreboard players get %const%0 reg
execute store result storage wasm:localstack stack.data[15] int 1 run scoreboard players get %const%0 reg
execute store result storage wasm:localstack stack.data[16] int 1 run scoreboard players get %const%0 reg
scoreboard players set %work%3%0%lo%temp reg 63
scoreboard players set %work%3%0%hi%temp reg 0
execute store result storage wasm:localstack stack.data[4] int 1 run scoreboard players get %work%3%0%lo%temp reg
execute store result storage wasm:localstack stack.data[5] int 1 run scoreboard players get %work%3%0%hi%temp reg
execute store result score %work%3%1%lo%temp reg run data get storage wasm:localstack stack.data[2]
execute store result score %work%3%1%hi%temp reg run data get storage wasm:localstack stack.data[3]
execute store result score %work%3%2%lo%temp reg run data get storage wasm:localstack stack.data[2]
execute store result score %work%3%2%hi%temp reg run data get storage wasm:localstack stack.data[3]
scoreboard players set %work%3%3%lo%temp reg 63
scoreboard players set %work%3%3%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%3%2%lo%temp reg
scoreboard players operation %param0%1 reg = %work%3%2%hi%temp reg
scoreboard players operation %param1%0 reg = %work%3%3%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:ashr_i64
scoreboard players operation %work%3%4%lo%temp reg = %param0%0 reg
scoreboard players operation %work%3%4%hi%temp reg = %param0%1 reg
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%3%4%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%3%4%hi%temp reg
scoreboard players operation %work%3%5%lo%temp reg = %work%3%1%lo%temp reg
scoreboard players operation %work%3%5%hi%temp reg = %work%3%1%hi%temp reg
scoreboard players operation %work%3%5%lo%temp reg += %work%3%4%lo%temp reg
scoreboard players operation %work%3%5%hi%temp reg += %work%3%4%hi%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%3%1%lo%temp reg matches ..-1 if score %work%3%4%lo%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%3%1%lo%temp reg matches ..-1 if score %work%3%4%lo%temp reg matches 0.. if score %work%3%5%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%3%1%lo%temp reg matches 0.. if score %work%3%4%lo%temp reg matches ..-1 if score %work%3%5%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%3%5%hi%temp reg += %temp%10%lo reg
execute store result score %work%3%6%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%3%6%hi%temp reg run data get storage wasm:localstack stack.data[7]
scoreboard players operation %param0%0 reg = %work%3%5%lo%temp reg
scoreboard players operation %param1%0 reg = %work%3%6%lo%temp reg
function intrinsic:xor
scoreboard players operation %work%3%7%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%3%5%hi%temp reg
scoreboard players operation %param1%0 reg = %work%3%6%hi%temp reg
function intrinsic:xor
scoreboard players operation %work%3%7%hi%temp reg = %return%0 reg
execute store result storage wasm:localstack stack.data[8] int 1 run scoreboard players get %work%3%7%lo%temp reg
execute store result storage wasm:localstack stack.data[9] int 1 run scoreboard players get %work%3%7%hi%temp reg
execute store result score %work%3%8%lo%temp reg run data get storage wasm:localstack stack.data[0]
execute store result score %work%3%8%hi%temp reg run data get storage wasm:localstack stack.data[1]
execute store result score %work%3%9%lo%temp reg run data get storage wasm:localstack stack.data[0]
execute store result score %work%3%9%hi%temp reg run data get storage wasm:localstack stack.data[1]
scoreboard players set %work%3%10%lo%temp reg 63
scoreboard players set %work%3%10%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%3%9%lo%temp reg
scoreboard players operation %param0%1 reg = %work%3%9%hi%temp reg
scoreboard players operation %param1%0 reg = %work%3%10%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:ashr_i64
scoreboard players operation %work%3%11%lo%temp reg = %param0%0 reg
scoreboard players operation %work%3%11%hi%temp reg = %param0%1 reg
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%3%11%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%3%11%hi%temp reg
scoreboard players operation %work%3%12%lo%temp reg = %work%3%8%lo%temp reg
scoreboard players operation %work%3%12%hi%temp reg = %work%3%8%hi%temp reg
scoreboard players operation %work%3%12%lo%temp reg += %work%3%11%lo%temp reg
scoreboard players operation %work%3%12%hi%temp reg += %work%3%11%hi%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%3%8%lo%temp reg matches ..-1 if score %work%3%11%lo%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%3%8%lo%temp reg matches ..-1 if score %work%3%11%lo%temp reg matches 0.. if score %work%3%12%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%3%8%lo%temp reg matches 0.. if score %work%3%11%lo%temp reg matches ..-1 if score %work%3%12%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%3%12%hi%temp reg += %temp%10%lo reg
execute store result score %work%3%13%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%3%13%hi%temp reg run data get storage wasm:localstack stack.data[7]
scoreboard players operation %param0%0 reg = %work%3%12%lo%temp reg
scoreboard players operation %param1%0 reg = %work%3%13%lo%temp reg
function intrinsic:xor
scoreboard players operation %work%3%14%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%3%12%hi%temp reg
scoreboard players operation %param1%0 reg = %work%3%13%hi%temp reg
function intrinsic:xor
scoreboard players operation %work%3%14%hi%temp reg = %return%0 reg
execute store result storage wasm:localstack stack.data[10] int 1 run scoreboard players get %work%3%14%lo%temp reg
execute store result storage wasm:localstack stack.data[11] int 1 run scoreboard players get %work%3%14%hi%temp reg
scoreboard players set %work%3%15%lo%temp reg 0
scoreboard players set %work%3%15%hi%temp reg 0
execute store result storage wasm:localstack stack.data[12] int 1 run scoreboard players get %work%3%15%lo%temp reg
execute store result storage wasm:localstack stack.data[13] int 1 run scoreboard players get %work%3%15%hi%temp reg
scoreboard players set %work%3%16%lo%temp reg 0
scoreboard players set %work%3%16%hi%temp reg 0
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%3%16%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%3%16%hi%temp reg
function intrinsic:i64divrem/wasm_3_2
scoreboard players set %condtaken reg 1