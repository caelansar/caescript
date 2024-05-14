.PHONY: build
build:
	cargo build --release --bin caescript --features=build-binary,vm

.PHONY: build_eval
build_eval:
	cargo build --release --bin caescript --features=build-binary

install:
	cargo install --features=build-binary,vm --path .

install_eval:
	cargo install --features=build-binary --path .

.PHONY: test
test:
	cargo test --all-features
