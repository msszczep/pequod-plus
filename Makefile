clean:
	lein clean; rm -rf figwheel_server.log

core:
	emacs src/cljs/pequod_plus/core.cljs &

util:
	emacs src/cljc/pequod_plus/util.cljc &

serve:
	lein figwheel

gen:
	rm -rf pequod-csv-test.db ; sudo rm -rf resources/*csv; lein run -m pequod-plus.datasource

#oldgen:
#	lein run -m pequod-plus.gen ppex001 > ppex001.cljs

connect:
	python3 -m sqlite3 pequod-csv-test.db

go:
	lein run -m pequod-plus.csvgen
