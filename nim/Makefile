.PHONY: pull
pull:
	docker pull frolvlad/alpine-nim

.PHONY: start
start:
	docker run -d --rm -ti -v `pwd`:/source frolvlad/alpine-nim /bin/sh

.PHONY: name
name:
	docker ps -f ancestor=frolvlad/alpine-nim --format "{{.Names}}"

.PHONY: run
run:
	docker exec -ti `make name | awk 'END{print}'` nim compile --run /source/${name}
