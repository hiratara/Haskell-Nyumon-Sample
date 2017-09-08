
TARGETS=auction-server auction-client bidder-bot counter-system

make:

install:
	stack install

clean:
	stack clean

counter-server:
	stack build :counter-system --exec counter-system

server:
	stack install --local-bin-path=./bin
	./bin/auction-server

client:
	stack install --local-bin-path=./bin
	./bin/auction-client

$(TARGETS):
	stack build :$@ --exec $@
