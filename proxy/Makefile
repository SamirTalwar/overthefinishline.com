SHELL := /bin/bash

TAG = samirtalwar/overthefinishline.com-proxy

.PHONY: build
build:
	docker build --tag=$(TAG) .

.PHONY: push
push: build
	docker push $(TAG)
