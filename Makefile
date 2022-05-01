all:
	cargo build
	cp target/debug/cmmc .
	cp target/debug/cmmc env/bin/cmmc-bin
	mkdir env/lib
	gcc src/stdcminusminus.c -c -o env/lib/stdcminusminus.o

clean:
	cargo clean
	cargo clean -p get-pos-derive
	rm cmmc
	rm env/bin/cmmc-bin
	rm -r env/lib/
