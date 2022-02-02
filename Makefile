all:
	cargo build
	cp target/debug/cmmc .

clean:
	cargo clean
	rm cmmc
