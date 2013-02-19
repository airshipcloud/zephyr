MODULES = \
	cloudstore

all clean: apply_config
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done

apply_config:
	script/apply_config.sh

setup: clean all
	script/init.sh

test:
	cd cloudstore; npm install; npm test
