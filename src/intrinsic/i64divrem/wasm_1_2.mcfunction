execute store result score %work%1%2%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%1%2%hi%temp reg run data get storage wasm:localstack stack.data[7]
scoreboard players set %work%1%3%lo%temp reg 1
scoreboard players set %work%1%3%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%1%2%lo%temp reg
scoreboard players operation %param0%1 reg = %work%1%2%hi%temp reg
scoreboard players operation %param1%0 reg = %work%1%3%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:shl_64
scoreboard players operation %work%1%4%lo%temp reg = %param0%0 reg
scoreboard players operation %work%1%4%hi%temp reg = %param0%1 reg
execute store result score %work%1%5%lo%temp reg run data get storage wasm:localstack stack.data[0]
execute store result score %work%1%5%hi%temp reg run data get storage wasm:localstack stack.data[1]
execute store result score %work%1%6%lo%temp reg run data get storage wasm:localstack stack.data[4]
execute store result score %work%1%6%hi%temp reg run data get storage wasm:localstack stack.data[5]
scoreboard players operation %param0%0 reg = %work%1%5%lo%temp reg
scoreboard players operation %param0%1 reg = %work%1%5%hi%temp reg
scoreboard players operation %param1%0 reg = %work%1%6%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:lshr_i64
scoreboard players operation %work%1%7%lo%temp reg = %param0%0 reg
scoreboard players operation %work%1%7%hi%temp reg = %param0%1 reg
scoreboard players set %work%1%8%lo%temp reg 1
scoreboard players set %work%1%8%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%1%7%lo%temp reg
scoreboard players operation %param1%0 reg = %work%1%8%lo%temp reg
function intrinsic:and
scoreboard players operation %work%1%9%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%1%7%hi%temp reg
scoreboard players operation %param1%0 reg = %work%1%8%hi%temp reg
function intrinsic:and
scoreboard players operation %work%1%9%hi%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%1%4%lo%temp reg
scoreboard players operation %param1%0 reg = %work%1%9%lo%temp reg
function intrinsic:or
scoreboard players operation %work%1%10%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%1%4%hi%temp reg
scoreboard players operation %param1%0 reg = %work%1%9%hi%temp reg
function intrinsic:or
scoreboard players operation %work%1%10%hi%temp reg = %return%0 reg
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%1%10%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%1%10%hi%temp reg
scoreboard players set %work%1%11%lo%temp reg 0
scoreboard players set %work%1%11%hi%temp reg 0
execute store result score %work%1%12%lo%temp reg run data get storage wasm:localstack stack.data[2]
execute store result score %work%1%12%hi%temp reg run data get storage wasm:localstack stack.data[3]
execute store result score %work%1%13%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%1%13%hi%temp reg run data get storage wasm:localstack stack.data[7]
execute store result score %work%1%14%lo%temp reg run data get storage wasm:localstack stack.data[2]
execute store result score %work%1%14%hi%temp reg run data get storage wasm:localstack stack.data[3]
scoreboard players set %temp%3%lo reg 0
execute if score %work%1%13%hi%temp reg matches ..-1 if score %work%1%14%hi%temp reg matches 0.. run scoreboard players set %temp%3%lo reg 0
execute if score %work%1%13%hi%temp reg matches 0.. if score %work%1%14%hi%temp reg matches ..-1 run scoreboard players set %temp%3%lo reg 1
execute if score %work%1%13%hi%temp reg matches ..-1 if score %work%1%14%hi%temp reg matches ..-1 if score %work%1%13%hi%temp reg < %work%1%14%hi%temp reg run scoreboard players set %temp%3%lo reg 1
execute if score %work%1%13%hi%temp reg matches 0.. if score %work%1%14%hi%temp reg matches 0.. if score %work%1%13%hi%temp reg < %work%1%14%hi%temp reg run scoreboard players set %temp%3%lo reg 1
scoreboard players set %temp%4%lo reg 0
execute if score %work%1%14%hi%temp reg matches ..-1 if score %work%1%13%hi%temp reg matches 0.. run scoreboard players set %temp%4%lo reg 0
execute if score %work%1%14%hi%temp reg matches 0.. if score %work%1%13%hi%temp reg matches ..-1 run scoreboard players set %temp%4%lo reg 1
execute if score %work%1%14%hi%temp reg matches ..-1 if score %work%1%13%hi%temp reg matches ..-1 if score %work%1%14%hi%temp reg < %work%1%13%hi%temp reg run scoreboard players set %temp%4%lo reg 1
execute if score %work%1%14%hi%temp reg matches 0.. if score %work%1%13%hi%temp reg matches 0.. if score %work%1%14%hi%temp reg < %work%1%13%hi%temp reg run scoreboard players set %temp%4%lo reg 1
execute store success score %temp%5%lo reg if score %work%1%13%hi%temp reg = %work%1%14%hi%temp reg
scoreboard players set %temp%6%lo reg 0
execute if score %work%1%13%lo%temp reg matches ..-1 if score %work%1%14%lo%temp reg matches 0.. run scoreboard players set %temp%6%lo reg 0
execute if score %work%1%13%lo%temp reg matches 0.. if score %work%1%14%lo%temp reg matches ..-1 run scoreboard players set %temp%6%lo reg 1
execute if score %work%1%13%lo%temp reg matches ..-1 if score %work%1%14%lo%temp reg matches ..-1 if score %work%1%13%lo%temp reg < %work%1%14%lo%temp reg run scoreboard players set %temp%6%lo reg 1
execute if score %work%1%13%lo%temp reg matches 0.. if score %work%1%14%lo%temp reg matches 0.. if score %work%1%13%lo%temp reg < %work%1%14%lo%temp reg run scoreboard players set %temp%6%lo reg 1
execute if score %temp%3%lo reg matches 1.. run scoreboard players set %work%1%15%lo%temp reg 1
execute if score %temp%4%lo reg matches 1.. run scoreboard players set %work%1%15%lo%temp reg 0
execute if score %temp%5%lo reg matches 1.. run scoreboard players operation %work%1%15%lo%temp reg = %temp%6%lo reg
scoreboard players operation %work%1%16%lo%temp reg = %work%1%12%lo%temp reg
execute unless score %work%1%15%lo%temp reg matches 0 run scoreboard players operation %work%1%16%lo%temp reg = %work%1%11%lo%temp reg
scoreboard players operation %work%1%16%hi%temp reg = %work%1%12%hi%temp reg
execute unless score %work%1%15%lo%temp reg matches 0 run scoreboard players operation %work%1%16%hi%temp reg = %work%1%11%hi%temp reg
scoreboard players operation %temp%11%lo reg = %work%1%16%lo%temp reg
scoreboard players operation %temp%11%hi reg = %work%1%16%hi%temp reg
scoreboard players operation %temp%11%lo reg *= %%-1 reg
scoreboard players remove %temp%11%lo reg 1
scoreboard players operation %temp%11%hi reg *= %%-1 reg
scoreboard players remove %temp%11%hi reg 1
execute if score %temp%11%lo reg matches -1 run scoreboard players add %temp%11%hi reg 1
scoreboard players add %temp%11%lo reg 1
scoreboard players operation %work%1%17%lo%temp reg = %work%1%10%lo%temp reg
scoreboard players operation %work%1%17%hi%temp reg = %work%1%10%hi%temp reg
scoreboard players operation %work%1%17%lo%temp reg += %temp%11%lo reg
scoreboard players operation %work%1%17%hi%temp reg += %temp%11%hi reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%1%10%lo%temp reg matches ..-1 if score %temp%11%lo reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%1%10%lo%temp reg matches ..-1 if score %temp%11%lo reg matches 0.. if score %work%1%17%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%1%10%lo%temp reg matches 0.. if score %temp%11%lo reg matches ..-1 if score %work%1%17%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%1%17%hi%temp reg += %temp%10%lo reg
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%1%17%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%1%17%hi%temp reg
execute store result score %work%1%18%lo%temp reg run data get storage wasm:localstack stack.data[4]
execute store result score %work%1%18%hi%temp reg run data get storage wasm:localstack stack.data[5]
scoreboard players set %work%1%19%lo%temp reg -1
scoreboard players set %work%1%19%hi%temp reg -1
scoreboard players operation %work%1%20%lo%temp reg = %work%1%18%lo%temp reg
scoreboard players operation %work%1%20%hi%temp reg = %work%1%18%hi%temp reg
scoreboard players operation %work%1%20%lo%temp reg += %work%1%19%lo%temp reg
scoreboard players operation %work%1%20%hi%temp reg += %work%1%19%hi%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%1%18%lo%temp reg matches ..-1 if score %work%1%19%lo%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%1%18%lo%temp reg matches ..-1 if score %work%1%19%lo%temp reg matches 0.. if score %work%1%20%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%1%18%lo%temp reg matches 0.. if score %work%1%19%lo%temp reg matches ..-1 if score %work%1%20%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%1%20%hi%temp reg += %temp%10%lo reg
execute store result storage wasm:localstack stack.data[4] int 1 run scoreboard players get %work%1%20%lo%temp reg
execute store result storage wasm:localstack stack.data[5] int 1 run scoreboard players get %work%1%20%hi%temp reg
scoreboard players set %work%1%21%lo%temp reg -1
scoreboard players set %work%1%21%hi%temp reg -1
execute store success score %work%1%22%lo%temp reg unless score %work%1%20%lo%temp reg = %work%1%21%lo%temp reg
execute unless score %work%1%22%lo%temp reg matches 1 run execute store success score %work%1%22%lo%temp reg unless score %work%1%20%hi%temp reg = %work%1%21%hi%temp reg
scoreboard players set %condtaken reg 0
scoreboard players set %condtaken reg 0
execute unless score %work%1%22%lo%temp reg matches 0 run function intrinsic:i64divrem/wasm_1_2
execute if score %condtaken reg matches 0 run function intrinsic:i64divrem/wasm_1_3
scoreboard players set %condtaken reg 1