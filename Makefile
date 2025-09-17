# Convenience Makefile wrappers

.PHONY: all build test a3 dist install docker quickstart clean

all: build

build:
	./scripts/rebuild.sh

test:
	./scripts/run_tests.sh

a3:
	./scripts/run_a3_all.sh

dist:
	./scripts/run_a3_finalize.sh

install:
	@echo "Installing to system (may require sudo)"
	cd build && cmake --install .

docker:
	./scripts/run_in_docker.sh

quickstart:
	./scripts/quickstart.sh

clean:
	rm -rf build dist goldens

