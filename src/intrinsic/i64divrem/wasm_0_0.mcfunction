data modify storage wasm:scratch stack.data set value [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
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
scoreboard players set %work%0%0%lo%temp reg 63
scoreboard players set %work%0%0%hi%temp reg 0
execute store result storage wasm:localstack stack.data[4] int 1 run scoreboard players get %work%0%0%lo%temp reg
execute store result storage wasm:localstack stack.data[5] int 1 run scoreboard players get %work%0%0%hi%temp reg
scoreboard players set %work%0%1%lo%temp reg 0
scoreboard players set %work%0%1%hi%temp reg 0
execute store result storage wasm:localstack stack.data[6] int 1 run scoreboard players get %work%0%1%lo%temp reg
execute store result storage wasm:localstack stack.data[7] int 1 run scoreboard players get %work%0%1%hi%temp reg
scoreboard players set %work%0%2%lo%temp reg 0
scoreboard players set %work%0%2%hi%temp reg 0
execute store result storage wasm:localstack stack.data[8] int 1 run scoreboard players get %work%0%2%lo%temp reg
execute store result storage wasm:localstack stack.data[9] int 1 run scoreboard players get %work%0%2%hi%temp reg
function intrinsic:i64divrem/wasm_0_2
scoreboard players set %condtaken reg 1