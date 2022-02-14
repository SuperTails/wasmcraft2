# Arguments
# %ptr
# Return value is %return%0

function intrinsic:setptr
function intrinsic:load_byte
scoreboard players operation %return%0 reg = %param0%0 reg
scoreboard players add %ptr reg 1

function intrinsic:setptr
function intrinsic:load_byte
scoreboard players operation %param0%0 reg *= %%256 reg
scoreboard players operation %return%0 reg += %param0%0 reg