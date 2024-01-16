MACHINE CruiseControl

SETS
	Status = { on, off, suspended }
VARIABLES
	speed, status, cruising_speed
INVARIANT
	speed:0..7 & status : Status & cruising_speed : 3..7
INITIALIZATION
	speed, status, cruising_speed := 0, off, 3
OPERATIONS
	cruise =
	PRE	speed > 2
	THEN	cruising_speed := speed ||
		status := on
	END;
	speedup =
	PRE	speed < 7
	THEN	speed := speed + 1
	END;
	slowdown =
	PRE 	speed > 0 & status = off
	THEN
		speed := speed - 1
	END;
	regulate =
	PRE	status = on
	THEN	IF cruising_speed > speed THEN
		speed := speed + 1
		ELSE IF cruising_speed < speed THEN
		speed := speed - 1
		ELSE skip
		END END
	END;
	brake =
	PRE	speed > 0
	THEN	speed := max({0, speed - 2}) ||
		IF status = on THEN status := suspended END
	END;
	resume =
	PRE	status = suspended
	THEN	status := on
	END;
	accelerate =
	PRE	status = on & cruising_speed < 7
	THEN	cruising_speed := cruising_speed + 1
	END;
	decelerate =
	PRE	status = on & cruising_speed > 3
	THEN	cruising_speed := cruising_speed - 1
	END

END
