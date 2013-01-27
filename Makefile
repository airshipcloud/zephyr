MODULES = \
	cloudstore

all clean:
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done
