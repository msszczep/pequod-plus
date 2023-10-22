clean:
	lein clean; rm -rf figwheel_server.log

core:
	emacs src/cljs/pequod_plus/core.cljs &

serve:
	lein figwheel

gen:
	lein run -m pequod-plus.gen ppex001 > ppex001.cljs
