.PHONY: pull
pull:
	docker pull jimmycuadra/rust
.PHONY: d
d:
	docker run -d --rm -ti -v `pwd`:/source jimmycuadra/rust

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

.PHONY: new
new:
	docker run -d --rm -ti -v `pwd`:/source jimmycuadra/rust cargo new ${name} --bin && cp Makefile ./${name}

.PHONY: rm
rm:
	docker rm -f `docker ps -a -q`
