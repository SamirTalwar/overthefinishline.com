SHELL := /bin/bash

STACK = overthefinishline

.PHONY: build
build:
	make -C dashboard build
	make -C database build
	make -C proxy build
	touch .built

.PHONY: check
check:
	make -C dashboard check

.PHONY: test
test:
	make -C dashboard test

.PHONY: connect
connect:
	DOCKER_HOST= \
		docker run \
		--rm \
		--interactive \
		--volume=/var/run/docker.sock:/var/run/docker.sock \
		--env=DOCKER_HOST \
		dockercloud/client \
		samirtalwar/overthefinishline

.PHONY: push
push:
	@ if [[ -z "$${DOCKER_HOST+x}" ]]; then \
		echo 'Please connect to a Swarm instance.'; \
		exit 1; \
	fi
	git push
	make -C dashboard push
	make -C proxy push
	sleep 60
	docker stack deploy --with-registry-auth --compose-file=docker-compose.production.yml overthefinishline

.PHONY: clean
clean:
	make -C dashboard clean
