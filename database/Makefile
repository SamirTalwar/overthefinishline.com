SHELL := /bin/bash

.PHONY: build
build:
	source ../development.env \
	&& docker build \
		--tag=overthefinishline.com/database \
		--build-arg=DASHBOARD_PASSWORD=$$DASHBOARD_PASSWORD \
		.
