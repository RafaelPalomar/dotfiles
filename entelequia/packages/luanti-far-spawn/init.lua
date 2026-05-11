-- entelequia far_spawn: assign new players to one of N slots on a
-- circle around (0, ?, 0), so they don't all clump together at the
-- world origin.  Persistent rotation across server restarts.

local S = vector.new
local RADIUS = 2000
local N = 8
local Y_SCAN_TOP = 200
local Y_SCAN_BOTTOM = -80

local slots = {}
for i = 1, N do
  local theta = (i - 1) * (2 * math.pi / N)
  slots[i] = {
    x = math.floor(RADIUS * math.cos(theta)),
    z = math.floor(RADIUS * math.sin(theta)),
  }
end

local storage = core.get_mod_storage()

local function next_slot()
  local n = (storage:get_int("idx") or 0) + 1
  if n > N then n = 1 end
  storage:set_int("idx", n)
  return slots[n], n
end

local function find_surface_y(x, z)
  for y = Y_SCAN_TOP, Y_SCAN_BOTTOM, -1 do
    local node = core.get_node(S(x, y, z))
    if node.name ~= "air" and node.name ~= "ignore" then
      return y + 1
    end
  end
  return 80
end

local function place_player(player_name, slot, idx)
  local x, z = slot.x, slot.z
  core.emerge_area(
    S(x - 24, Y_SCAN_BOTTOM, z - 24),
    S(x + 24, Y_SCAN_TOP,    z + 24),
    function(_, _, remaining)
      if remaining ~= 0 then return end
      local player = core.get_player_by_name(player_name)
      if not (player and player:is_player()) then return end
      local pos = S(x, find_surface_y(x, z), z)
      player:set_pos(pos)
      if mcl_spawn and mcl_spawn.set_spawn_pos then
        mcl_spawn.set_spawn_pos(player, pos, false)
      end
      core.log("action", string.format(
        "[far_spawn] %s -> slot %d at %s",
        player_name, idx, core.pos_to_string(pos)))
    end
  )
end

core.register_on_newplayer(function(player)
  local slot, idx = next_slot()
  place_player(player:get_player_name(), slot, idx)
end)
