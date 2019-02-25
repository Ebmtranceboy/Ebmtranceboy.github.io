fluid.wasm: fluid.c
	emcc -s WASM=1 -Os -s EXPORTED_FUNCTIONS="[\
	'_fluid_init','_density_ref','_fluid_step','_IX','_hsv2rgb'\
	]" -o fluid.wasm *.c 
