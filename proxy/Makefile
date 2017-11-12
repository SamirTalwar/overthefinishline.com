SHELL := /bin/bash
unexport DOCKER_HOST

TAG = samirtalwar/overthefinishline.com-proxy

.PHONY: build
build:
	docker build --tag=$(TAG) .

.PHONY: push
push: build
	docker push $(TAG)
