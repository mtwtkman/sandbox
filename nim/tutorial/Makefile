.PHONY: pull
pull:
	docker pull frolvlad/alpine-nim

.PHONY: start
start:
	docker run -d --rm -ti -v `pwd`:/source frolvlad/alpine-nim

.PHONY: name
name:
	docker ps -f ancestor=frolvlad/alpine-nim --format "{{.Names}}"

.PHONY: compile
build:
	docker exec -ti `make name | awk 'END{print}'` compile --run ${name}
