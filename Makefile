ERL = erl
APP = swedish_chef

all: src_src ebin/$(APP).app tests

tests: src_src src_tests
	${ERL} -pz ebin -pz ebin_tests -b start_sasl -noshell -s init stop -eval 'test_suite:test().'

ebin:
	mkdir ebin

ebin_tests:
	mkdir ebin_tests

src_src: ebin
	cd src;erl -make

src_tests: ebin_tests
	cd tests;erl -make

clean:
	rm -rf ebin
	rm -rf ebin_tests

ebin/$(APP).app:
	@cp -v src/$(APP).app $@