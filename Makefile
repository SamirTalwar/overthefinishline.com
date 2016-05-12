build:
	make -C api
	make -C dashboard

test:
	make -C api test
	make -C dashboard test
