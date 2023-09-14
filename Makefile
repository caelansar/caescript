.PHONY: build
build:
	cargo build --release --bin caescript --features=build-binary,vm

.PHONY: build_eval
build_eval:
	cargo build --release --bin caescript --features=build-binary

.PHONY: test
test:
	cargo test --all-features
