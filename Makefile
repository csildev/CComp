.PHONY:test
test:
	cargo run test.c > test.asm
	nasm -felf64 test.asm
	gcc test.o -o test
	./test
clean:
	rm test.o test.asm test -f
	cargo clean -p parserlib
	cargo clean -p stdio
	cargo clean

git: clean
	git add .
	@read -p "Enter commit message:" message;\
	git commit -m "$$message";
	git push
