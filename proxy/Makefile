SHELL := /bin/bash

TAG = samirtalwar/overthefinishline.com-proxy

.PHONY: build
build:
	docker build --tag=$(TAG) .

.PHONY: push
push:
	docker push $(TAG)
