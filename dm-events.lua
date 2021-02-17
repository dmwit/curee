function start_screen()
	DEMO = true
	report("start screen")
end

function level_select()
	MUSIC_SELECTION = ""
	for _, music_choice in pairs(music_choices) do
		r, g, b, palette = emu.getscreenpixel(music_choice.x, music_choice.y, true)
		if palette == music_choice_selected then
			MUSIC_SELECTION = MUSIC_SELECTION .. music_choice.selection
		elseif palette ~= music_choice_not_selected then
			MUSIC_SELECTION = MUSIC_SELECTION .. "ERROR"
		end
	end
	if MUSIC_SELECTION == "" then
		transition()
		return
	end

	level_1p = digits(level_select_text_bg, level_select_text_fg, level_select_level1p_x, level_select_level1p_y, level_digits)
	speed_1p = level_select_speed(speed_choice_dy_1p)
	prefix = "level select " .. MUSIC_SELECTION .. " " .. level_1p .. " " .. speed_1p

	r, g, b, palette = emu.getscreenpixel(enable2p_x, enable2p_y, true)
	if palette == enable2p_on then
		level_2p = digits(level_select_text_bg, level_select_text_fg, level_select_level2p_x, level_select_level2p_y, level_digits)
		speed_2p = level_select_speed(speed_choice_dy_2p)
		suffix = " " .. level_2p .. " " .. speed_2p
	elseif palette == enable2p_off then
		suffix = ""
	else
		suffix = "ERROR"
	end

	DEMO = false
	report(prefix .. suffix)
end

function level_select_speed(dy)
	SPEED = ""
	for _, speed_choice in pairs(speed_choices) do
		r, g, b, palette = emu.getscreenpixel(speed_choice.x, speed_choice.y + dy, true)
		if palette == speed_choice_selected then
			SPEED = SPEED .. speed_choice.selection
		elseif palette ~= speed_choice_not_selected then
			SPEED = SPEED .. "ERROR" .. palette
		end
	end
	return SPEED
end

function digits(bg, fg, X, y, n)
	RESULT = 0
	for _=1,n do
		RESULT = 10*RESULT + fingerprint(bg, fg, X, y, digit_fingerprint, digit_mapping)
		X = X + digit_width
	end
	return RESULT
end

function fingerprint(bg, fg, x, y, vectors, mapping)
	FINGERPRINT = 0
	for _, vector in ipairs(vectors) do
		r, g, b, palette = emu.getscreenpixel(x + vector.dx, y + vector.dy, true)
		FINGERPRINT = 2*FINGERPRINT
		if palette == fg then
			FINGERPRINT = 1+FINGERPRINT
		elseif palette ~= bg then
			return "fingerprint(" .. bg .. ", " .. fg .. ", " .. x .. ", " .. y .. "): saw unknown palette " .. palette .. " at +(" .. vector.dx .. ", " .. vector.dy .. ")"
		end
	end
	return mapping[FINGERPRINT+1] -- 1-indexing lmao
end

function play_low_or_transition()
	r, g, b, palette = emu.getscreenpixel(mode2_x, mode2_y, true)
	mode2_lookup[palette]("low")
end

function play_med_or_demo()
	if DEMO then report("demo") else play("med") end
end

function play_hi_or_boot()
	r, g, b, palette = emu.getscreenpixel(boot_x, boot_y, true)
	boot_lookup[palette]()
end

function play_hi()
	play("hi")
end

function boot()
	report("boot")
end

function play(speed)
	r, g, b, palette = emu.getscreenpixel(player_count_x, player_count_y, true)
	player_count_lookup[palette](speed)
end

function play_1p(speed)
	r, g, b, palette = emu.getscreenpixel(play_1p_virus_x + digit_always_fg_dx, play_1p_virus_y + digit_always_fg_dy, true)
	if palette == play_1p_text_bg then
		transition()
		return
	end
	level = digits(play_1p_text_bg, play_1p_text_fg, play_1p_level_x, play_1p_level_y, level_digits)
	viruses = digits(play_1p_text_bg, play_1p_text_fg, play_1p_virus_x, play_1p_virus_y, virus_digits)
	score = digits(play_1p_text_bg, play_1p_text_fg, play_1p_score_x, play_1p_score_y, score_digits)
	report("play " .. player_status(level, viruses, speed, score))
end

function play_2p(speed_2p)
	r, g, b, palette = emu.getscreenpixel(play_2p_virus_1p_x + digit_always_fg_dx, play_2p_virus_1p_y + digit_always_fg_dy, true)
	if palette ~= play_2p_text_fg then
		transition()
		return
	end

	CROWNS_1P = 0
	CROWNS_2P = 0
	for i=0,2 do
		r_1p, g_1p, b_1p, palette_1p = emu.getscreenpixel(crown_1p_x, crown_1p_y + i*crown_height, true)
		r_2p, g_2p, b_2p, palette_2p = emu.getscreenpixel(crown_2p_x, crown_2p_y + i*crown_height, true)
		if palette_1p == crown_on_palette then
			CROWNS_1P = CROWNS_1P + 1
		end
		if palette_2p == crown_on_palette then
			CROWNS_2P = CROWNS_2P + 1
		end
	end

	speed_1p = fingerprint(play_2p_text_bg, play_2p_text_fg, play_2p_speed_1p_x, play_2p_speed_1p_y, speed_fingerprint, speed_mapping)
	level_1p = digits(play_2p_text_bg, play_2p_text_fg, play_2p_level_1p_x, play_2p_level_1p_y, virus_digits)
	level_2p = digits(play_2p_text_bg, play_2p_text_fg, play_2p_level_2p_x, play_2p_level_2p_y, virus_digits)
	viruses_1p = digits(play_2p_text_bg, play_2p_text_fg, play_2p_virus_1p_x, play_2p_virus_1p_y, virus_digits)
	viruses_2p = digits(play_2p_text_bg, play_2p_text_fg, play_2p_virus_2p_x, play_2p_virus_2p_y, virus_digits)
	report("play " .. player_status(level_1p, viruses_1p, speed_1p, CROWNS_1P) .. " " .. player_status(level_2p, viruses_2p, speed_2p, CROWNS_2P))
end

function player_status(level, viruses, speed, extra)
	return level .. "(" .. viruses .. ") " .. speed .. " " .. extra
end

function transition_or_pause_or_cutscene()
	r0, g0, b0, palette0 = emu.getscreenpixel(cutscene_x, cutscene_y, true)
	r1, g1, b1, palette1 = emu.getscreenpixel(pause_x, pause_y, true)
	if palette0 == cutscene_palette then
		report("cutscene")
	elseif palette1 == pause_palette then
		report("pause")
	else
		transition()
	end
end

function transition()
	report("transition")
end

function unknown()
	r, g, b, palette = emu.getscreenpixel(mode_x, mode_y, true)
	mode_lookup[palette]()
end

function report(s)
	io.write("" .. emu.framecount() .. " " .. s .. "\n")
end

DEMO = true

mode_x = 0
mode_y = 16
mode_lookup =
	{ [ 0] = play_hi_or_boot
	, [ 3] = play_med_or_demo
	, [10] = play_low_or_transition
	, [15] = transition_or_pause_or_cutscene
	, [21] = transition
	, [26] = start_screen
	, [34] = transition
	, [39] = level_select
	}

mode2_x = 0
mode2_y = 24
mode2_lookup =
	{ [15] = play
	, [26] = transition
	}

music_choice_selected = 39
music_choice_not_selected = 15
music_choices =
	{ { x =  8*8, y = 24*8-4, selection = "fever" }
	, { x = 15*8, y = 24*8-4, selection = "chill" }
	, { x = 22*8, y = 24*8-4, selection = "off" }
	}

speed_choice_selected = 39
speed_choice_not_selected = 15
speed_choice_dy_1p = -2
speed_choice_dy_2p = 9
speed_choices =
	{ { x = 12*8+3, y = 18*8, selection = "low" }
	, { x = 17*8+3, y = 18*8, selection = "med" }
	, { x = 21*8+7, y = 18*8, selection = "hi" }
	}

level_select_level1p_x = 23*8
level_select_level1p_y = 10*8
level_select_level2p_x = level_select_level1p_x
level_select_level2p_y = 13*8

level_digits = 2

enable2p_x = level_select_level2p_x
enable2p_y = level_select_level2p_y - 4
enable2p_on = 41
enable2p_off = 15

digit_fingerprint =
	{ { dx = 0, dy = 0 }
	, { dx = 0, dy = 4 }
	, { dx = 0, dy = 5 }
	, { dx = 6, dy = 2 }
	}
digit_mapping =
	{ 1   -- 0b0000
	, 9   -- 0b0001
	, 3   -- 0b0010
	, 2   -- 0b0011
	, 4   -- 0b0100
	, 0   -- 0b0101
	, 6   -- 0b0110
	, 8   -- 0b0111
	, 7   -- 0b1000
	, "X" -- 0b1001
	, 5   -- 0b1010
	, "X" -- 0b1011
	, "X" -- 0b1100
	, "X" -- 0b1101
	, "X" -- 0b1110
	, "X" -- 0b1111
	}
digit_width = 8
digit_always_fg_dx = 3
digit_always_fg_dy = 0

speed_fingerprint =
	{ { dx = 0, dy = 0 }
	, { dx = 3, dy = 0 }
	}
speed_mapping =
	{ "X"   -- 0b00
	, "low" -- 0b01
	, "hi"  -- 0b10
	, "med" -- 0b11
	}

level_select_text_bg = 15
level_select_text_fg = 48

player_count_x = 10
player_count_y = 44
player_count_lookup =
	{ [ 0] = play_2p
	, [ 3] = play_2p
	, [10] = play_2p
	, [15] = transition
	, [40] = play_1p
	}

play_1p_level_x = 27*8
play_1p_level_y = 19*8
play_1p_virus_x = 27*8
play_1p_virus_y = 25*8
play_1p_score_x =  2*8
play_1p_score_y = 11*8
play_1p_text_bg = 50
play_1p_text_fg = 15

play_2p_virus_1p_x = 110
play_2p_virus_1p_y = 192
play_2p_virus_2p_x = 131
play_2p_virus_2p_y = 192
play_2p_level_1p_x = 109
play_2p_level_1p_y = 44
play_2p_level_2p_x = 132
play_2p_level_2p_y = 44
play_2p_speed_1p_x = 104
play_2p_speed_1p_y = 56
play_2p_text_bg = 49
play_2p_text_fg = 15

virus_digits = 2
score_digits = 7

boot_x = 0
boot_y = 24
boot_lookup =
	{ [ 0] = boot
	, [15] = play_hi
	}

cutscene_x = 27
cutscene_y = 27
cutscene_palette = 24

pause_x = 112
pause_y = 120
pause_palette = 48

crown_1p_x = 118
crown_1p_y = 108
crown_2p_x = 134
crown_2p_y = 108
crown_height = 16
crown_off_palette = 15
crown_on_palette = 40

emu.registerbefore(unknown)
