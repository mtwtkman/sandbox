.PHONY: d
d:
	docker run -d -ti -v `pwd`:/source jimmycuadra/rust

.PHONY: name
name:
	docker ps -f ancestor=jimmycuadra/rust --format "{{.Names}}"

.PHONY: build
build:
	docker exec -ti `make name | awk 'END{print}'` cargo build

.PHONY: run
run:
	docker exec -ti `make name | awk 'END{print}'` cargo run

.PHONY: do
do:
	make build && make run
