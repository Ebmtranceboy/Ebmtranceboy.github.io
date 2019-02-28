embroidery.wasm: embroidery.c
	emcc -s WASM=1 -Os -s EXPORTED_FUNCTIONS="[\
	'_embroidery_step','_init'\
	]" -o embroidery.wasm *.c 
