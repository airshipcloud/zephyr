MODULES = \
	cloudstore

all clean: apply_config
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done

apply_config:
	script/apply_config

setup: clean all
	script/init

test:
	cd cloudstore; npm install; npm test
