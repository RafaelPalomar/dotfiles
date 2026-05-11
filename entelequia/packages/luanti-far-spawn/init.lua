-- entelequia far_spawn: assign new players to one of N slots on a
-- circle around (0, ?, 0), so they don't all clump together at the
-- world origin.  Persistent rotation across server restarts.
--
-- Death-respawn: if the player has no bed/respawn-anchor set, send
-- them back to their original far_spawn slot rather than the shared
-- world spawn point.  If they DO have a bed set, mcl_spawn handles it
-- normally.

local S = vector.new
local RADIUS = 2000
local N = 8
local Y_SCAN_TOP = 200
local Y_SCAN_BOTTOM = -80
local META_KEY = "far_spawn:pos"

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
      -- Persist for later on_respawnplayer overrides.
      player:get_meta():set_string(META_KEY, core.pos_to_string(pos))
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

-- Backfill META_KEY for players who joined under an older version of
-- this mod (or any returning player without a saved slot).  On join,
-- snapshot their current position so death respawn sends them back
-- here instead of the shared world spawn.
core.register_on_joinplayer(function(player)
  local meta = player:get_meta()
  if meta:get_string(META_KEY) == "" then
    local pos = player:get_pos()
    if pos then
      meta:set_string(META_KEY, core.pos_to_string(vector.round(pos)))
      core.log("action", string.format(
        "[far_spawn] backfill %s -> %s",
        player:get_player_name(), core.pos_to_string(vector.round(pos))))
    end
  end
end)

-- Death-respawn override: route players who have NOT set a bed or
-- respawn anchor back to their personal far_spawn slot.  Runs after
-- mcl_spawn's on_respawnplayer (mineclonia is a game-level mod that
-- loads before user mods), so mcl_spawn has already moved the player
-- to the bed or world spawn; we may re-move them to the slot.
core.register_on_respawnplayer(function(player)
  -- If the player has a real bed / respawn anchor, let mcl_spawn keep
  -- whatever it chose.
  if mcl_spawn and mcl_spawn.get_bed_spawn_pos then
    local bed_pos, custom = mcl_spawn.get_bed_spawn_pos(player)
    if bed_pos and custom then return false end
  end
  local saved = core.string_to_pos(player:get_meta():get_string(META_KEY))
  if not saved then return false end
  player:set_pos(saved)
  core.log("action", string.format(
    "[far_spawn] respawn %s -> %s",
    player:get_player_name(), core.pos_to_string(saved)))
  return true
end)
