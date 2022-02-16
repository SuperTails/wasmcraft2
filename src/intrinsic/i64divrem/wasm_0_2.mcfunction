execute store result score %work%0%3%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%0%3%hi%temp reg run data get storage wasm:localstack stack.data[7]
scoreboard players set %work%0%4%lo%temp reg 1
scoreboard players set %work%0%4%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%0%3%lo%temp reg
scoreboard players operation %param0%1 reg = %work%0%3%hi%temp reg
scoreboard players operation %param1%0 reg = %work%0%4%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:shl_64
scoreboard players operation %work%0%5%lo%temp reg = %param0%0 reg
scoreboard players operation %work%0%5%hi%temp reg = %param0%1 reg
scoreboard players set %work%0%6%lo%temp reg 1
scoreboard players set %work%0%6%hi%temp reg 0
execute store result score %work%0%7%lo%temp reg run data get storage wasm:localstack stack.data[4]
execute store result score %work%0%7%hi%temp reg run data get storage wasm:localstack stack.data[5]
scoreboard players operation %param0%0 reg = %work%0%6%lo%temp reg
scoreboard players operation %param0%1 reg = %work%0%6%hi%temp reg
scoreboard players operation %param1%0 reg = %work%0%7%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:shl_64
scoreboard players operation %work%0%8%lo%temp reg = %param0%0 reg
scoreboard players operation %work%0%8%hi%temp reg = %param0%1 reg
execute store result storage wasm:localstack stack.data[10] int 1 run scoreboard players get %work%0%8%lo%temp reg
execute store result storage wasm:localstack stack.data[11] int 1 run scoreboard players get %work%0%8%hi%temp reg
execute store result score %work%0%9%lo%temp reg run data get storage wasm:localstack stack.data[0]
execute store result score %work%0%9%hi%temp reg run data get storage wasm:localstack stack.data[1]
scoreboard players operation %param0%0 reg = %work%0%8%lo%temp reg
scoreboard players operation %param1%0 reg = %work%0%9%lo%temp reg
function intrinsic:and
scoreboard players operation %work%0%10%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%0%8%hi%temp reg
scoreboard players operation %param1%0 reg = %work%0%9%hi%temp reg
function intrinsic:and
scoreboard players operation %work%0%10%hi%temp reg = %return%0 reg
scoreboard players set %work%0%11%lo%temp reg 0
scoreboard players set %work%0%11%hi%temp reg 0
execute store success score %work%0%12%lo%temp reg unless score %work%0%10%lo%temp reg = %work%0%11%lo%temp reg
execute unless score %work%0%12%lo%temp reg matches 1 run execute store success score %work%0%12%lo%temp reg unless score %work%0%10%hi%temp reg = %work%0%11%hi%temp reg
scoreboard players operation %work%0%13%lo%temp reg = %work%0%12%lo%temp reg
scoreboard players set %work%0%13%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%0%5%lo%temp reg
scoreboard players operation %param1%0 reg = %work%0%13%lo%temp reg
function intrinsic:or
scoreboard players operation %work%0%14%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%0%5%hi%temp reg
scoreboard players operation %param1%0 reg = %work%0%13%hi%temp reg
function intrinsic:or
scoreboard players operation %work%0%14%hi%temp reg = %return%0 reg
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%0%14%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%0%14%hi%temp reg
scoreboard players set %work%0%15%lo%temp reg 0
scoreboard players set %work%0%15%hi%temp reg 0
execute store result score %work%0%16%lo%temp reg run data get storage wasm:localstack stack.data[2]
execute store result score %work%0%16%hi%temp reg run data get storage wasm:localstack stack.data[3]
execute store result score %work%0%17%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%0%17%hi%temp reg run data get storage wasm:localstack stack.data[7]
execute store result score %work%0%18%lo%temp reg run data get storage wasm:localstack stack.data[2]
execute store result score %work%0%18%hi%temp reg run data get storage wasm:localstack stack.data[3]
scoreboard players set %temp%3%lo reg 0
execute if score %work%0%17%hi%temp reg matches ..-1 if score %work%0%18%hi%temp reg matches 0.. run scoreboard players set %temp%3%lo reg 0
execute if score %work%0%17%hi%temp reg matches 0.. if score %work%0%18%hi%temp reg matches ..-1 run scoreboard players set %temp%3%lo reg 1
execute if score %work%0%17%hi%temp reg matches ..-1 if score %work%0%18%hi%temp reg matches ..-1 if score %work%0%17%hi%temp reg < %work%0%18%hi%temp reg run scoreboard players set %temp%3%lo reg 1
execute if score %work%0%17%hi%temp reg matches 0.. if score %work%0%18%hi%temp reg matches 0.. if score %work%0%17%hi%temp reg < %work%0%18%hi%temp reg run scoreboard players set %temp%3%lo reg 1
scoreboard players set %temp%4%lo reg 0
execute if score %work%0%18%hi%temp reg matches ..-1 if score %work%0%17%hi%temp reg matches 0.. run scoreboard players set %temp%4%lo reg 0
execute if score %work%0%18%hi%temp reg matches 0.. if score %work%0%17%hi%temp reg matches ..-1 run scoreboard players set %temp%4%lo reg 1
execute if score %work%0%18%hi%temp reg matches ..-1 if score %work%0%17%hi%temp reg matches ..-1 if score %work%0%18%hi%temp reg < %work%0%17%hi%temp reg run scoreboard players set %temp%4%lo reg 1
execute if score %work%0%18%hi%temp reg matches 0.. if score %work%0%17%hi%temp reg matches 0.. if score %work%0%18%hi%temp reg < %work%0%17%hi%temp reg run scoreboard players set %temp%4%lo reg 1
execute store success score %temp%5%lo reg if score %work%0%17%hi%temp reg = %work%0%18%hi%temp reg
scoreboard players set %temp%6%lo reg 0
execute if score %work%0%17%lo%temp reg matches ..-1 if score %work%0%18%lo%temp reg matches 0.. run scoreboard players set %temp%6%lo reg 0
execute if score %work%0%17%lo%temp reg matches 0.. if score %work%0%18%lo%temp reg matches ..-1 run scoreboard players set %temp%6%lo reg 1
execute if score %work%0%17%lo%temp reg matches ..-1 if score %work%0%18%lo%temp reg matches ..-1 if score %work%0%17%lo%temp reg < %work%0%18%lo%temp reg run scoreboard players set %temp%6%lo reg 1
execute if score %work%0%17%lo%temp reg matches 0.. if score %work%0%18%lo%temp reg matches 0.. if score %work%0%17%lo%temp reg < %work%0%18%lo%temp reg run scoreboard players set %temp%6%lo reg 1
execute if score %temp%3%lo reg matches 1.. run scoreboard players set %work%0%19%lo%temp reg 1
execute if score %temp%4%lo reg matches 1.. run scoreboard players set %work%0%19%lo%temp reg 0
execute if score %temp%5%lo reg matches 1.. run scoreboard players operation %work%0%19%lo%temp reg = %temp%6%lo reg
execute store result storage wasm:localstack stack.data[12] int 1 run scoreboard players get %work%0%19%lo%temp reg
scoreboard players operation %work%0%20%lo%temp reg = %work%0%16%lo%temp reg
execute unless score %work%0%19%lo%temp reg matches 0 run scoreboard players operation %work%0%20%lo%temp reg = %work%0%15%lo%temp reg
scoreboard players operation %work%0%20%hi%temp reg = %work%0%16%hi%temp reg
execute unless score %work%0%19%lo%temp reg matches 0 run scoreboard players operation %work%0%20%hi%temp reg = %work%0%15%hi%temp reg
scoreboard players operation %temp%11%lo reg = %work%0%20%lo%temp reg
scoreboard players operation %temp%11%hi reg = %work%0%20%hi%temp reg
scoreboard players operation %temp%11%lo reg *= %%-1 reg
scoreboard players remove %temp%11%lo reg 1
scoreboard players operation %temp%11%hi reg *= %%-1 reg
scoreboard players remove %temp%11%hi reg 1
execute if score %temp%11%lo reg matches -1 run scoreboard players add %temp%11%hi reg 1
scoreboard players add %temp%11%lo reg 1
scoreboard players operation %work%0%21%lo%temp reg = %work%0%14%lo%temp reg
scoreboard players operation %work%0%21%hi%temp reg = %work%0%14%hi%temp reg
scoreboard players operation %work%0%21%lo%temp reg += %temp%11%lo reg
scoreboard players operation %work%0%21%hi%temp reg += %temp%11%hi reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%0%14%lo%temp reg matches ..-1 if score %temp%11%lo reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%0%14%lo%temp reg matches ..-1 if score %temp%11%lo reg matches 0.. if score %work%0%21%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%0%14%lo%temp reg matches 0.. if score %temp%11%lo reg matches ..-1 if score %work%0%21%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%0%21%hi%temp reg += %temp%10%lo reg
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%0%21%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%0%21%hi%temp reg
scoreboard players set %work%0%22%lo%temp reg 0
scoreboard players set %work%0%22%hi%temp reg 0
execute store result score %work%0%23%lo%temp reg run data get storage wasm:localstack stack.data[10]
execute store result score %work%0%23%hi%temp reg run data get storage wasm:localstack stack.data[11]
execute store result score %work%0%24%lo%temp reg run data get storage wasm:localstack stack.data[12]
scoreboard players operation %work%0%25%lo%temp reg = %work%0%23%lo%temp reg
execute unless score %work%0%24%lo%temp reg matches 0 run scoreboard players operation %work%0%25%lo%temp reg = %work%0%22%lo%temp reg
scoreboard players operation %work%0%25%hi%temp reg = %work%0%23%hi%temp reg
execute unless score %work%0%24%lo%temp reg matches 0 run scoreboard players operation %work%0%25%hi%temp reg = %work%0%22%hi%temp reg
execute store result score %work%0%26%lo%temp reg run data get storage wasm:localstack stack.data[8]
execute store result score %work%0%26%hi%temp reg run data get storage wasm:localstack stack.data[9]
scoreboard players operation %param0%0 reg = %work%0%25%lo%temp reg
scoreboard players operation %param1%0 reg = %work%0%26%lo%temp reg
function intrinsic:or
scoreboard players operation %work%0%27%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%0%25%hi%temp reg
scoreboard players operation %param1%0 reg = %work%0%26%hi%temp reg
function intrinsic:or
scoreboard players operation %work%0%27%hi%temp reg = %return%0 reg
execute store result storage wasm:localstack stack.data[8] int 1 run scoreboard players get %work%0%27%lo%temp reg
execute store result storage wasm:localstack stack.data[9] int 1 run scoreboard players get %work%0%27%hi%temp reg
execute store result score %work%0%28%lo%temp reg run data get storage wasm:localstack stack.data[4]
execute store result score %work%0%28%hi%temp reg run data get storage wasm:localstack stack.data[5]
scoreboard players set %work%0%29%lo%temp reg -1
scoreboard players set %work%0%29%hi%temp reg -1
scoreboard players operation %work%0%30%lo%temp reg = %work%0%28%lo%temp reg
scoreboard players operation %work%0%30%hi%temp reg = %work%0%28%hi%temp reg
scoreboard players operation %work%0%30%lo%temp reg += %work%0%29%lo%temp reg
scoreboard players operation %work%0%30%hi%temp reg += %work%0%29%hi%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%0%28%lo%temp reg matches ..-1 if score %work%0%29%lo%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%0%28%lo%temp reg matches ..-1 if score %work%0%29%lo%temp reg matches 0.. if score %work%0%30%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%0%28%lo%temp reg matches 0.. if score %work%0%29%lo%temp reg matches ..-1 if score %work%0%30%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%0%30%hi%temp reg += %temp%10%lo reg
execute store result storage wasm:localstack stack.data[4] int 1 run scoreboard players get %work%0%30%lo%temp reg
execute store result storage wasm:localstack stack.data[5] int 1 run scoreboard players get %work%0%30%hi%temp reg
scoreboard players set %work%0%31%lo%temp reg -1
scoreboard players set %work%0%31%hi%temp reg -1
execute store success score %work%0%32%lo%temp reg unless score %work%0%30%lo%temp reg = %work%0%31%lo%temp reg
execute unless score %work%0%32%lo%temp reg matches 1 run execute store success score %work%0%32%lo%temp reg unless score %work%0%30%hi%temp reg = %work%0%31%hi%temp reg
scoreboard players set %condtaken reg 0
scoreboard players set %condtaken reg 0
execute unless score %work%0%32%lo%temp reg matches 0 run function intrinsic:i64divrem/wasm_0_2
execute if score %condtaken reg matches 0 run function intrinsic:i64divrem/wasm_0_3
scoreboard players set %condtaken reg 1