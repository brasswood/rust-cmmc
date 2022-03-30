all:
	cargo build
	cp target/debug/cmmc .

clean:
	cargo clean
	cargo clean -p get-pos-derive
	rm cmmc
