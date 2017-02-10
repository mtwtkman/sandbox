.PHONY: d
d:
	docker run -d -ti -v `pwd`:/source jimmycuadra/rust

.PHONY: name
name:
	docker ps -f ancestor=jimmycuadra/rust --format "{{.Names}}"

.PHONY: build
build:
	docker exec -ti ${name} cargo build

.PHONY: run
run:
	docker exec -ti ${name} cargo run

.PHONY: do
do:
	make build name=${name} && make run name=${name}
