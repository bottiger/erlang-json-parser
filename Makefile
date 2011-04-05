main:	clean
	erlc -o ebin *.erl

clean:
	mkdir -p ebin
	rm -f ebin/*

VERSION=Mailup-0.1
