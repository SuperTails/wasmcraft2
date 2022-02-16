execute store result score %work%3%17%lo%temp reg run data get storage wasm:localstack stack.data[12]
execute store result score %work%3%17%hi%temp reg run data get storage wasm:localstack stack.data[13]
scoreboard players set %work%3%18%lo%temp reg 1
scoreboard players set %work%3%18%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%3%17%lo%temp reg
scoreboard players operation %param0%1 reg = %work%3%17%hi%temp reg
scoreboard players operation %param1%0 reg = %work%3%18%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:shl_64
scoreboard players operation %work%3%19%lo%temp reg = %param0%0 reg
scoreboard players operation %work%3%19%hi%temp reg = %param0%1 reg
scoreboard players set %work%3%20%lo%temp reg 1
scoreboard players set %work%3%20%hi%temp reg 0
execute store result score %work%3%21%lo%temp reg run data get storage wasm:localstack stack.data[4]
execute store result score %work%3%21%hi%temp reg run data get storage wasm:localstack stack.data[5]
scoreboard players operation %param0%0 reg = %work%3%20%lo%temp reg
scoreboard players operation %param0%1 reg = %work%3%20%hi%temp reg
scoreboard players operation %param1%0 reg = %work%3%21%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:shl_64
scoreboard players operation %work%3%22%lo%temp reg = %param0%0 reg
scoreboard players operation %work%3%22%hi%temp reg = %param0%1 reg
execute store result storage wasm:localstack stack.data[14] int 1 run scoreboard players get %work%3%22%lo%temp reg
execute store result storage wasm:localstack stack.data[15] int 1 run scoreboard players get %work%3%22%hi%temp reg
execute store result score %work%3%23%lo%temp reg run data get storage wasm:localstack stack.data[10]
execute store result score %work%3%23%hi%temp reg run data get storage wasm:localstack stack.data[11]
scoreboard players operation %param0%0 reg = %work%3%22%lo%temp reg
scoreboard players operation %param1%0 reg = %work%3%23%lo%temp reg
function intrinsic:and
scoreboard players operation %work%3%24%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%3%22%hi%temp reg
scoreboard players operation %param1%0 reg = %work%3%23%hi%temp reg
function intrinsic:and
scoreboard players operation %work%3%24%hi%temp reg = %return%0 reg
scoreboard players set %work%3%25%lo%temp reg 0
scoreboard players set %work%3%25%hi%temp reg 0
execute store success score %work%3%26%lo%temp reg unless score %work%3%24%lo%temp reg = %work%3%25%lo%temp reg
execute unless score %work%3%26%lo%temp reg matches 1 run execute store success score %work%3%26%lo%temp reg unless score %work%3%24%hi%temp reg = %work%3%25%hi%temp reg
scoreboard players operation %work%3%27%lo%temp reg = %work%3%26%lo%temp reg
scoreboard players set %work%3%27%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%3%19%lo%temp reg
scoreboard players operation %param1%0 reg = %work%3%27%lo%temp reg
function intrinsic:or
scoreboard players operation %work%3%28%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%3%19%hi%temp reg
scoreboard players operation %param1%0 reg = %work%3%27%hi%temp reg
function intrinsic:or
scoreboard players operation %work%3%28%hi%temp reg = %return%0 reg
execute store result storage wasm:localstack stack.data[12] int 1 run scoreboard players get %work%3%28%lo%temp reg
execute store result storage wasm:localstack stack.data[13] int 1 run scoreboard players get %work%3%28%hi%temp reg
scoreboard players set %work%3%29%lo%temp reg 0
scoreboard players set %work%3%29%hi%temp reg 0
execute store result score %work%3%30%lo%temp reg run data get storage wasm:localstack stack.data[8]
execute store result score %work%3%30%hi%temp reg run data get storage wasm:localstack stack.data[9]
execute store result score %work%3%31%lo%temp reg run data get storage wasm:localstack stack.data[12]
execute store result score %work%3%31%hi%temp reg run data get storage wasm:localstack stack.data[13]
execute store result score %work%3%32%lo%temp reg run data get storage wasm:localstack stack.data[8]
execute store result score %work%3%32%hi%temp reg run data get storage wasm:localstack stack.data[9]
scoreboard players set %temp%3%lo reg 0
execute if score %work%3%31%hi%temp reg matches ..-1 if score %work%3%32%hi%temp reg matches 0.. run scoreboard players set %temp%3%lo reg 0
execute if score %work%3%31%hi%temp reg matches 0.. if score %work%3%32%hi%temp reg matches ..-1 run scoreboard players set %temp%3%lo reg 1
execute if score %work%3%31%hi%temp reg matches ..-1 if score %work%3%32%hi%temp reg matches ..-1 if score %work%3%31%hi%temp reg < %work%3%32%hi%temp reg run scoreboard players set %temp%3%lo reg 1
execute if score %work%3%31%hi%temp reg matches 0.. if score %work%3%32%hi%temp reg matches 0.. if score %work%3%31%hi%temp reg < %work%3%32%hi%temp reg run scoreboard players set %temp%3%lo reg 1
scoreboard players set %temp%4%lo reg 0
execute if score %work%3%32%hi%temp reg matches ..-1 if score %work%3%31%hi%temp reg matches 0.. run scoreboard players set %temp%4%lo reg 0
execute if score %work%3%32%hi%temp reg matches 0.. if score %work%3%31%hi%temp reg matches ..-1 run scoreboard players set %temp%4%lo reg 1
execute if score %work%3%32%hi%temp reg matches ..-1 if score %work%3%31%hi%temp reg matches ..-1 if score %work%3%32%hi%temp reg < %work%3%31%hi%temp reg run scoreboard players set %temp%4%lo reg 1
execute if score %work%3%32%hi%temp reg matches 0.. if score %work%3%31%hi%temp reg matches 0.. if score %work%3%32%hi%temp reg < %work%3%31%hi%temp reg run scoreboard players set %temp%4%lo reg 1
execute store success score %temp%5%lo reg if score %work%3%31%hi%temp reg = %work%3%32%hi%temp reg
scoreboard players set %temp%6%lo reg 0
execute if score %work%3%31%lo%temp reg matches ..-1 if score %work%3%32%lo%temp reg matches 0.. run scoreboard players set %temp%6%lo reg 0
execute if score %work%3%31%lo%temp reg matches 0.. if score %work%3%32%lo%temp reg matches ..-1 run scoreboard players set %temp%6%lo reg 1
execute if score %work%3%31%lo%temp reg matches ..-1 if score %work%3%32%lo%temp reg matches ..-1 if score %work%3%31%lo%temp reg < %work%3%32%lo%temp reg run scoreboard players set %temp%6%lo reg 1
execute if score %work%3%31%lo%temp reg matches 0.. if score %work%3%32%lo%temp reg matches 0.. if score %work%3%31%lo%temp reg < %work%3%32%lo%temp reg run scoreboard players set %temp%6%lo reg 1
execute if score %temp%3%lo reg matches 1.. run scoreboard players set %work%3%33%lo%temp reg 1
execute if score %temp%4%lo reg matches 1.. run scoreboard players set %work%3%33%lo%temp reg 0
execute if score %temp%5%lo reg matches 1.. run scoreboard players operation %work%3%33%lo%temp reg = %temp%6%lo reg
execute store result storage wasm:localstack stack.data[16] int 1 run scoreboard players get %work%3%33%lo%temp reg
scoreboard players operation %work%3%34%lo%temp reg = %work%3%30%lo%temp reg
execute unless score %work%3%33%lo%temp reg matches 0 run scoreboard players operation %work%3%34%lo%temp reg = %work%3%29%lo%temp reg
scoreboard players operation %work%3%34%hi%temp reg = %work%3%30%hi%temp reg
execute unless score %work%3%33%lo%temp reg matches 0 run scoreboard players operation %work%3%34%hi%temp reg = %work%3%29%hi%temp reg
scoreboard players operation %temp%11%lo reg = %work%3%34%lo%temp reg
scoreboard players operation %temp%11%hi reg = %work%3%34%hi%temp reg
scoreboard players operation %temp%11%lo reg *= %%-1 reg
scoreboard players remove %temp%11%lo reg 1
scoreboard players operation %temp%11%hi reg *= %%-1 reg
scoreboard players remove %temp%11%hi reg 1
execute if score %temp%11%lo reg matches -1 run scoreboard players add %temp%11%hi reg 1
scoreboard players add %temp%11%lo reg 1
scoreboard players operation %work%3%35%lo%temp reg = %work%3%28%lo%temp reg
scoreboard players operation %work%3%35%hi%temp reg = %work%3%28%hi%temp reg
scoreboard players operation %work%3%35%lo%temp reg += %temp%11%lo reg
scoreboard players operation %work%3%35%hi%temp reg += %temp%11%hi reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%3%28%lo%temp reg matches ..-1 if score %temp%11%lo reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%3%28%lo%temp reg matches ..-1 if score %temp%11%lo reg matches 0.. if score %work%3%35%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%3%28%lo%temp reg matches 0.. if score %temp%11%lo reg matches ..-1 if score %work%3%35%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%3%35%hi%temp reg += %temp%10%lo reg
execute store result storage wasm:localstack stack.data[12] int 1 run scoreboard players get %work%3%35%lo%temp reg
execute store result storage wasm:localstack stack.data[13] int 1 run scoreboard players get %work%3%35%hi%temp reg
scoreboard players set %work%3%36%lo%temp reg 0
scoreboard players set %work%3%36%hi%temp reg 0
execute store result score %work%3%37%lo%temp reg run data get storage wasm:localstack stack.data[14]
execute store result score %work%3%37%hi%temp reg run data get storage wasm:localstack stack.data[15]
execute store result score %work%3%38%lo%temp reg run data get storage wasm:localstack stack.data[16]
scoreboard players operation %work%3%39%lo%temp reg = %work%3%37%lo%temp reg
execute unless score %work%3%38%lo%temp reg matches 0 run scoreboard players operation %work%3%39%lo%temp reg = %work%3%36%lo%temp reg
scoreboard players operation %work%3%39%hi%temp reg = %work%3%37%hi%temp reg
execute unless score %work%3%38%lo%temp reg matches 0 run scoreboard players operation %work%3%39%hi%temp reg = %work%3%36%hi%temp reg
execute store result score %work%3%40%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%3%40%hi%temp reg run data get storage wasm:localstack stack.data[7]
scoreboard players operation %param0%0 reg = %work%3%39%lo%temp reg
scoreboard players operation %param1%0 reg = %work%3%40%lo%temp reg
function intrinsic:or
scoreboard players operation %work%3%41%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%3%39%hi%temp reg
scoreboard players operation %param1%0 reg = %work%3%40%hi%temp reg
function intrinsic:or
scoreboard players operation %work%3%41%hi%temp reg = %return%0 reg
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%3%41%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%3%41%hi%temp reg
execute store result score %work%3%42%lo%temp reg run data get storage wasm:localstack stack.data[4]
execute store result score %work%3%42%hi%temp reg run data get storage wasm:localstack stack.data[5]
scoreboard players set %work%3%43%lo%temp reg -1
scoreboard players set %work%3%43%hi%temp reg -1
scoreboard players operation %work%3%44%lo%temp reg = %work%3%42%lo%temp reg
scoreboard players operation %work%3%44%hi%temp reg = %work%3%42%hi%temp reg
scoreboard players operation %work%3%44%lo%temp reg += %work%3%43%lo%temp reg
scoreboard players operation %work%3%44%hi%temp reg += %work%3%43%hi%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%3%42%lo%temp reg matches ..-1 if score %work%3%43%lo%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%3%42%lo%temp reg matches ..-1 if score %work%3%43%lo%temp reg matches 0.. if score %work%3%44%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%3%42%lo%temp reg matches 0.. if score %work%3%43%lo%temp reg matches ..-1 if score %work%3%44%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%3%44%hi%temp reg += %temp%10%lo reg
execute store result storage wasm:localstack stack.data[4] int 1 run scoreboard players get %work%3%44%lo%temp reg
execute store result storage wasm:localstack stack.data[5] int 1 run scoreboard players get %work%3%44%hi%temp reg
scoreboard players set %work%3%45%lo%temp reg -1
scoreboard players set %work%3%45%hi%temp reg -1
execute store success score %work%3%46%lo%temp reg unless score %work%3%44%lo%temp reg = %work%3%45%lo%temp reg
execute unless score %work%3%46%lo%temp reg matches 1 run execute store success score %work%3%46%lo%temp reg unless score %work%3%44%hi%temp reg = %work%3%45%hi%temp reg
scoreboard players set %condtaken reg 0
scoreboard players set %condtaken reg 0
execute unless score %work%3%46%lo%temp reg matches 0 run function intrinsic:i64divrem/wasm_3_2
execute if score %condtaken reg matches 0 run function intrinsic:i64divrem/wasm_3_3
scoreboard players set %condtaken reg 1