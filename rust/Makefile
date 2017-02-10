.PHONY: d
d:
	docker run -d -v `pwd`:/source jimmycuadra/rust

.PHONY: build
build:
	docker exec -ti hungry_hypatia cargo build

.PHONY: run
run:
	docker exec -ti hungry_hypatia cargo run

.PHONY: do
do:
	make build && make run
