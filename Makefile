MODULES = \
	cloudstore

all clean:
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done

update:
	git pull origin master
	./apply_config.sh
	make

test:
	cd cloudstore; npm install; npm test
