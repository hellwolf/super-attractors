build/%.r1cs build/%.sym build/%_cpp build/%_js : circom/%.circom
	mkdir -p build
	circom --output build $< --r1cs --wasm --sym --c

build-all: \
  build/lorenz_atractor.r1cs build/lorenz_atractor.sym build/lorenz_atractor_js build/lorenz_atractor_cpp \
  build-samples \
  build-witness

clean:
	rm -rf build

build/%.wtns: test/%-input.json build/lorenz_atractor_js
	node build/lorenz_atractor_js/generate_witness.js build/lorenz_atractor_js/lorenz_atractor.wasm \
		$< $@

build/%.wtns.json : build/%.wtns
	npx snarkjs wej $< $@

build-samples:
	./maths/lorenz.hs > test/sample1-input.json

build-witness: build/sample1.wtns build/sample1.wtns.json

.PHONY: build-* clean
