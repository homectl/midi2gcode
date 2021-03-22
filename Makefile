TEST_FILE = music/windmill.mid

check: test/golden.nc
	runhaskell midi2gcode.hs
	diff -u test/golden.nc output.nc | head -n40

test/golden.nc: midi2gcode.py $(TEST_FILE) Makefile
	./midi2gcode.py -i $(TEST_FILE) -o output.nc
	diff -u test/golden.nc output.nc
