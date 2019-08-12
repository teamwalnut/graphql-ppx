clean:
	rm -f package.json
	rm -f bsconfig.json
	rm -f package-lock.json
	rm -rf node_modules

bsb5: clean
	cp ./bsb5/* .
	npm install
	npm run test
	make clean
	
bsb6: clean
	cp ./bsb6/* .
	npm install
	npm run test
	make clean
	
PHONY: bsb5 bsb6
