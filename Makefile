STACK = overthefinishline

.PHONY: build
build:
	make -C dashboard build
	make -C database build
	touch .built

.PHONY: check
check:
	make -C dashboard check

.PHONY: test
test:
	make -C dashboard test

.PHONY: push
push:
	git push
	make -C dashboard push
	sleep 60
	docker-cloud stack update $(STACK)

.PHONY: clean
clean:
	make -C dashboard clean
