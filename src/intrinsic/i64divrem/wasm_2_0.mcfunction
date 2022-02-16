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
scoreboard players set %work%2%0%lo%temp reg 63
scoreboard players set %work%2%0%hi%temp reg 0
execute store result storage wasm:localstack stack.data[4] int 1 run scoreboard players get %work%2%0%lo%temp reg
execute store result storage wasm:localstack stack.data[5] int 1 run scoreboard players get %work%2%0%hi%temp reg
execute store result score %work%2%1%lo%temp reg run data get storage wasm:localstack stack.data[2]
execute store result score %work%2%1%hi%temp reg run data get storage wasm:localstack stack.data[3]
execute store result score %work%2%2%lo%temp reg run data get storage wasm:localstack stack.data[2]
execute store result score %work%2%2%hi%temp reg run data get storage wasm:localstack stack.data[3]
scoreboard players set %work%2%3%lo%temp reg 63
scoreboard players set %work%2%3%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%2%2%lo%temp reg
scoreboard players operation %param0%1 reg = %work%2%2%hi%temp reg
scoreboard players operation %param1%0 reg = %work%2%3%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:ashr_i64
scoreboard players operation %work%2%4%lo%temp reg = %param0%0 reg
scoreboard players operation %work%2%4%hi%temp reg = %param0%1 reg
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%2%4%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%2%4%hi%temp reg
scoreboard players operation %work%2%5%lo%temp reg = %work%2%1%lo%temp reg
scoreboard players operation %work%2%5%hi%temp reg = %work%2%1%hi%temp reg
scoreboard players operation %work%2%5%lo%temp reg += %work%2%4%lo%temp reg
scoreboard players operation %work%2%5%hi%temp reg += %work%2%4%hi%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%2%1%lo%temp reg matches ..-1 if score %work%2%4%lo%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%2%1%lo%temp reg matches ..-1 if score %work%2%4%lo%temp reg matches 0.. if score %work%2%5%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%2%1%lo%temp reg matches 0.. if score %work%2%4%lo%temp reg matches ..-1 if score %work%2%5%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%2%5%hi%temp reg += %temp%10%lo reg
execute store result score %work%2%6%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%2%6%hi%temp reg run data get storage wasm:localstack stack.data[7]
scoreboard players operation %param0%0 reg = %work%2%5%lo%temp reg
scoreboard players operation %param1%0 reg = %work%2%6%lo%temp reg
function intrinsic:xor
scoreboard players operation %work%2%7%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%2%5%hi%temp reg
scoreboard players operation %param1%0 reg = %work%2%6%hi%temp reg
function intrinsic:xor
scoreboard players operation %work%2%7%hi%temp reg = %return%0 reg
execute store result storage wasm:localstack stack.data[8] int 1 run scoreboard players get %work%2%7%lo%temp reg
execute store result storage wasm:localstack stack.data[9] int 1 run scoreboard players get %work%2%7%hi%temp reg
execute store result score %work%2%8%lo%temp reg run data get storage wasm:localstack stack.data[0]
execute store result score %work%2%8%hi%temp reg run data get storage wasm:localstack stack.data[1]
execute store result score %work%2%9%lo%temp reg run data get storage wasm:localstack stack.data[0]
execute store result score %work%2%9%hi%temp reg run data get storage wasm:localstack stack.data[1]
scoreboard players set %work%2%10%lo%temp reg 63
scoreboard players set %work%2%10%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%2%9%lo%temp reg
scoreboard players operation %param0%1 reg = %work%2%9%hi%temp reg
scoreboard players operation %param1%0 reg = %work%2%10%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:ashr_i64
scoreboard players operation %work%2%11%lo%temp reg = %param0%0 reg
scoreboard players operation %work%2%11%hi%temp reg = %param0%1 reg
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%2%11%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%2%11%hi%temp reg
scoreboard players operation %work%2%12%lo%temp reg = %work%2%8%lo%temp reg
scoreboard players operation %work%2%12%hi%temp reg = %work%2%8%hi%temp reg
scoreboard players operation %work%2%12%lo%temp reg += %work%2%11%lo%temp reg
scoreboard players operation %work%2%12%hi%temp reg += %work%2%11%hi%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%2%8%lo%temp reg matches ..-1 if score %work%2%11%lo%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%2%8%lo%temp reg matches ..-1 if score %work%2%11%lo%temp reg matches 0.. if score %work%2%12%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%2%8%lo%temp reg matches 0.. if score %work%2%11%lo%temp reg matches ..-1 if score %work%2%12%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%2%12%hi%temp reg += %temp%10%lo reg
execute store result score %work%2%13%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%2%13%hi%temp reg run data get storage wasm:localstack stack.data[7]
scoreboard players operation %param0%0 reg = %work%2%12%lo%temp reg
scoreboard players operation %param1%0 reg = %work%2%13%lo%temp reg
function intrinsic:xor
scoreboard players operation %work%2%14%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%2%12%hi%temp reg
scoreboard players operation %param1%0 reg = %work%2%13%hi%temp reg
function intrinsic:xor
scoreboard players operation %work%2%14%hi%temp reg = %return%0 reg
execute store result storage wasm:localstack stack.data[10] int 1 run scoreboard players get %work%2%14%lo%temp reg
execute store result storage wasm:localstack stack.data[11] int 1 run scoreboard players get %work%2%14%hi%temp reg
scoreboard players set %work%2%15%lo%temp reg 0
scoreboard players set %work%2%15%hi%temp reg 0
execute store result storage wasm:localstack stack.data[12] int 1 run scoreboard players get %work%2%15%lo%temp reg
execute store result storage wasm:localstack stack.data[13] int 1 run scoreboard players get %work%2%15%hi%temp reg
scoreboard players set %work%2%16%lo%temp reg 0
scoreboard players set %work%2%16%hi%temp reg 0
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%2%16%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%2%16%hi%temp reg
function intrinsic:i64divrem/wasm_2_2
scoreboard players set %condtaken reg 1