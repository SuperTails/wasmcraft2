execute store result score %work%2%17%lo%temp reg run data get storage wasm:localstack stack.data[12]
execute store result score %work%2%17%hi%temp reg run data get storage wasm:localstack stack.data[13]
scoreboard players set %work%2%18%lo%temp reg 1
scoreboard players set %work%2%18%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%2%17%lo%temp reg
scoreboard players operation %param0%1 reg = %work%2%17%hi%temp reg
scoreboard players operation %param1%0 reg = %work%2%18%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:shl_64
scoreboard players operation %work%2%19%lo%temp reg = %param0%0 reg
scoreboard players operation %work%2%19%hi%temp reg = %param0%1 reg
scoreboard players set %work%2%20%lo%temp reg 1
scoreboard players set %work%2%20%hi%temp reg 0
execute store result score %work%2%21%lo%temp reg run data get storage wasm:localstack stack.data[4]
execute store result score %work%2%21%hi%temp reg run data get storage wasm:localstack stack.data[5]
scoreboard players operation %param0%0 reg = %work%2%20%lo%temp reg
scoreboard players operation %param0%1 reg = %work%2%20%hi%temp reg
scoreboard players operation %param1%0 reg = %work%2%21%lo%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:shl_64
scoreboard players operation %work%2%22%lo%temp reg = %param0%0 reg
scoreboard players operation %work%2%22%hi%temp reg = %param0%1 reg
execute store result storage wasm:localstack stack.data[14] int 1 run scoreboard players get %work%2%22%lo%temp reg
execute store result storage wasm:localstack stack.data[15] int 1 run scoreboard players get %work%2%22%hi%temp reg
execute store result score %work%2%23%lo%temp reg run data get storage wasm:localstack stack.data[10]
execute store result score %work%2%23%hi%temp reg run data get storage wasm:localstack stack.data[11]
scoreboard players operation %param0%0 reg = %work%2%22%lo%temp reg
scoreboard players operation %param1%0 reg = %work%2%23%lo%temp reg
function intrinsic:and
scoreboard players operation %work%2%24%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%2%22%hi%temp reg
scoreboard players operation %param1%0 reg = %work%2%23%hi%temp reg
function intrinsic:and
scoreboard players operation %work%2%24%hi%temp reg = %return%0 reg
scoreboard players set %work%2%25%lo%temp reg 0
scoreboard players set %work%2%25%hi%temp reg 0
execute store success score %work%2%26%lo%temp reg unless score %work%2%24%lo%temp reg = %work%2%25%lo%temp reg
execute unless score %work%2%26%lo%temp reg matches 1 run execute store success score %work%2%26%lo%temp reg unless score %work%2%24%hi%temp reg = %work%2%25%hi%temp reg
scoreboard players operation %work%2%27%lo%temp reg = %work%2%26%lo%temp reg
scoreboard players set %work%2%27%hi%temp reg 0
scoreboard players operation %param0%0 reg = %work%2%19%lo%temp reg
scoreboard players operation %param1%0 reg = %work%2%27%lo%temp reg
function intrinsic:or
scoreboard players operation %work%2%28%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%2%19%hi%temp reg
scoreboard players operation %param1%0 reg = %work%2%27%hi%temp reg
function intrinsic:or
scoreboard players operation %work%2%28%hi%temp reg = %return%0 reg
execute store result storage wasm:localstack stack.data[12] int 1 run scoreboard players get %work%2%28%lo%temp reg
execute store result storage wasm:localstack stack.data[13] int 1 run scoreboard players get %work%2%28%hi%temp reg
scoreboard players set %work%2%29%lo%temp reg 0
scoreboard players set %work%2%29%hi%temp reg 0
execute store result score %work%2%30%lo%temp reg run data get storage wasm:localstack stack.data[8]
execute store result score %work%2%30%hi%temp reg run data get storage wasm:localstack stack.data[9]
execute store result score %work%2%31%lo%temp reg run data get storage wasm:localstack stack.data[12]
execute store result score %work%2%31%hi%temp reg run data get storage wasm:localstack stack.data[13]
execute store result score %work%2%32%lo%temp reg run data get storage wasm:localstack stack.data[8]
execute store result score %work%2%32%hi%temp reg run data get storage wasm:localstack stack.data[9]
scoreboard players set %temp%3%lo reg 0
execute if score %work%2%31%hi%temp reg matches ..-1 if score %work%2%32%hi%temp reg matches 0.. run scoreboard players set %temp%3%lo reg 0
execute if score %work%2%31%hi%temp reg matches 0.. if score %work%2%32%hi%temp reg matches ..-1 run scoreboard players set %temp%3%lo reg 1
execute if score %work%2%31%hi%temp reg matches ..-1 if score %work%2%32%hi%temp reg matches ..-1 if score %work%2%31%hi%temp reg < %work%2%32%hi%temp reg run scoreboard players set %temp%3%lo reg 1
execute if score %work%2%31%hi%temp reg matches 0.. if score %work%2%32%hi%temp reg matches 0.. if score %work%2%31%hi%temp reg < %work%2%32%hi%temp reg run scoreboard players set %temp%3%lo reg 1
scoreboard players set %temp%4%lo reg 0
execute if score %work%2%32%hi%temp reg matches ..-1 if score %work%2%31%hi%temp reg matches 0.. run scoreboard players set %temp%4%lo reg 0
execute if score %work%2%32%hi%temp reg matches 0.. if score %work%2%31%hi%temp reg matches ..-1 run scoreboard players set %temp%4%lo reg 1
execute if score %work%2%32%hi%temp reg matches ..-1 if score %work%2%31%hi%temp reg matches ..-1 if score %work%2%32%hi%temp reg < %work%2%31%hi%temp reg run scoreboard players set %temp%4%lo reg 1
execute if score %work%2%32%hi%temp reg matches 0.. if score %work%2%31%hi%temp reg matches 0.. if score %work%2%32%hi%temp reg < %work%2%31%hi%temp reg run scoreboard players set %temp%4%lo reg 1
execute store success score %temp%5%lo reg if score %work%2%31%hi%temp reg = %work%2%32%hi%temp reg
scoreboard players set %temp%6%lo reg 0
execute if score %work%2%31%lo%temp reg matches ..-1 if score %work%2%32%lo%temp reg matches 0.. run scoreboard players set %temp%6%lo reg 0
execute if score %work%2%31%lo%temp reg matches 0.. if score %work%2%32%lo%temp reg matches ..-1 run scoreboard players set %temp%6%lo reg 1
execute if score %work%2%31%lo%temp reg matches ..-1 if score %work%2%32%lo%temp reg matches ..-1 if score %work%2%31%lo%temp reg < %work%2%32%lo%temp reg run scoreboard players set %temp%6%lo reg 1
execute if score %work%2%31%lo%temp reg matches 0.. if score %work%2%32%lo%temp reg matches 0.. if score %work%2%31%lo%temp reg < %work%2%32%lo%temp reg run scoreboard players set %temp%6%lo reg 1
execute if score %temp%3%lo reg matches 1.. run scoreboard players set %work%2%33%lo%temp reg 1
execute if score %temp%4%lo reg matches 1.. run scoreboard players set %work%2%33%lo%temp reg 0
execute if score %temp%5%lo reg matches 1.. run scoreboard players operation %work%2%33%lo%temp reg = %temp%6%lo reg
execute store result storage wasm:localstack stack.data[16] int 1 run scoreboard players get %work%2%33%lo%temp reg
scoreboard players operation %work%2%34%lo%temp reg = %work%2%30%lo%temp reg
execute unless score %work%2%33%lo%temp reg matches 0 run scoreboard players operation %work%2%34%lo%temp reg = %work%2%29%lo%temp reg
scoreboard players operation %work%2%34%hi%temp reg = %work%2%30%hi%temp reg
execute unless score %work%2%33%lo%temp reg matches 0 run scoreboard players operation %work%2%34%hi%temp reg = %work%2%29%hi%temp reg
scoreboard players operation %temp%11%lo reg = %work%2%34%lo%temp reg
scoreboard players operation %temp%11%hi reg = %work%2%34%hi%temp reg
scoreboard players operation %temp%11%lo reg *= %%-1 reg
scoreboard players remove %temp%11%lo reg 1
scoreboard players operation %temp%11%hi reg *= %%-1 reg
scoreboard players remove %temp%11%hi reg 1
execute if score %temp%11%lo reg matches -1 run scoreboard players add %temp%11%hi reg 1
scoreboard players add %temp%11%lo reg 1
scoreboard players operation %work%2%35%lo%temp reg = %work%2%28%lo%temp reg
scoreboard players operation %work%2%35%hi%temp reg = %work%2%28%hi%temp reg
scoreboard players operation %work%2%35%lo%temp reg += %temp%11%lo reg
scoreboard players operation %work%2%35%hi%temp reg += %temp%11%hi reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%2%28%lo%temp reg matches ..-1 if score %temp%11%lo reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%2%28%lo%temp reg matches ..-1 if score %temp%11%lo reg matches 0.. if score %work%2%35%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%2%28%lo%temp reg matches 0.. if score %temp%11%lo reg matches ..-1 if score %work%2%35%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%2%35%hi%temp reg += %temp%10%lo reg
execute store result storage wasm:localstack stack.data[12] int 1 run scoreboard players get %work%2%35%lo%temp reg
execute store result storage wasm:localstack stack.data[13] int 1 run scoreboard players get %work%2%35%hi%temp reg
scoreboard players set %work%2%36%lo%temp reg 0
scoreboard players set %work%2%36%hi%temp reg 0
execute store result score %work%2%37%lo%temp reg run data get storage wasm:localstack stack.data[14]
execute store result score %work%2%37%hi%temp reg run data get storage wasm:localstack stack.data[15]
execute store result score %work%2%38%lo%temp reg run data get storage wasm:localstack stack.data[16]
scoreboard players operation %work%2%39%lo%temp reg = %work%2%37%lo%temp reg
execute unless score %work%2%38%lo%temp reg matches 0 run scoreboard players operation %work%2%39%lo%temp reg = %work%2%36%lo%temp reg
scoreboard players operation %work%2%39%hi%temp reg = %work%2%37%hi%temp reg
execute unless score %work%2%38%lo%temp reg matches 0 run scoreboard players operation %work%2%39%hi%temp reg = %work%2%36%hi%temp reg
execute store result score %work%2%40%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%2%40%hi%temp reg run data get storage wasm:localstack stack.data[7]
scoreboard players operation %param0%0 reg = %work%2%39%lo%temp reg
scoreboard players operation %param1%0 reg = %work%2%40%lo%temp reg
function intrinsic:or
scoreboard players operation %work%2%41%lo%temp reg = %return%0 reg
scoreboard players operation %param0%0 reg = %work%2%39%hi%temp reg
scoreboard players operation %param1%0 reg = %work%2%40%hi%temp reg
function intrinsic:or
scoreboard players operation %work%2%41%hi%temp reg = %return%0 reg
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%2%41%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%2%41%hi%temp reg
execute store result score %work%2%42%lo%temp reg run data get storage wasm:localstack stack.data[4]
execute store result score %work%2%42%hi%temp reg run data get storage wasm:localstack stack.data[5]
scoreboard players set %work%2%43%lo%temp reg -1
scoreboard players set %work%2%43%hi%temp reg -1
scoreboard players operation %work%2%44%lo%temp reg = %work%2%42%lo%temp reg
scoreboard players operation %work%2%44%hi%temp reg = %work%2%42%hi%temp reg
scoreboard players operation %work%2%44%lo%temp reg += %work%2%43%lo%temp reg
scoreboard players operation %work%2%44%hi%temp reg += %work%2%43%hi%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%2%42%lo%temp reg matches ..-1 if score %work%2%43%lo%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%2%42%lo%temp reg matches ..-1 if score %work%2%43%lo%temp reg matches 0.. if score %work%2%44%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%2%42%lo%temp reg matches 0.. if score %work%2%43%lo%temp reg matches ..-1 if score %work%2%44%lo%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%2%44%hi%temp reg += %temp%10%lo reg
execute store result storage wasm:localstack stack.data[4] int 1 run scoreboard players get %work%2%44%lo%temp reg
execute store result storage wasm:localstack stack.data[5] int 1 run scoreboard players get %work%2%44%hi%temp reg
scoreboard players set %work%2%45%lo%temp reg -1
scoreboard players set %work%2%45%hi%temp reg -1
execute store success score %work%2%46%lo%temp reg unless score %work%2%44%lo%temp reg = %work%2%45%lo%temp reg
execute unless score %work%2%46%lo%temp reg matches 1 run execute store success score %work%2%46%lo%temp reg unless score %work%2%44%hi%temp reg = %work%2%45%hi%temp reg
scoreboard players set %condtaken reg 0
scoreboard players set %condtaken reg 0
execute unless score %work%2%46%lo%temp reg matches 0 run function intrinsic:i64divrem/wasm_2_2
execute if score %condtaken reg matches 0 run function intrinsic:i64divrem/wasm_2_3
scoreboard players set %condtaken reg 1